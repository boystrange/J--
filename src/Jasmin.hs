-- This file is part of J--

-- J-- is free software: you can redistribute it and/or modify it under the
-- terms of the GNU General Public License as published by the Free Software
-- Foundation, either version 3 of the License, or (at your option) any later
-- version.

-- J-- is distributed in the hope that it will be useful, but WITHOUT ANY
-- WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
-- A PARTICULAR PURPOSE.  See the GNU General Public License for more details.

-- You should have received a copy of the GNU General Public License along with
-- J--. If not, see <http://www.gnu.org/licenses/>.

-- Copyright 2022 Luca Padovani

module Jasmin where

import Atoms
import Type
import Language
import Render (printWarning)
import Data.Char (ord)
import Control.Monad (forM_, unless)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy (StateT, liftIO)
import qualified Control.Monad.State.Lazy as State
import System.IO (Handle, hClose, hFlush, hPutStrLn, openFile, IOMode(..))

data JasminCheckerState = JasminCheckerState { frame :: Int
                                             , stype :: Maybe [Type]
                                             , ltype :: Map Label [Type]
                                             , stack :: Int }

initialJasminCheckerState :: Type -> JasminCheckerState
initialJasminCheckerState (MethodType _ ts)
    = JasminCheckerState
    { frame = sum (map sizeOf ts)
    , stype = Just []
    , ltype = Map.empty 
    , stack = 0 }

type JasminChecker = StateT JasminCheckerState IO

pop :: Type -> JasminChecker ()
pop VoidType = return ()
pop t = do
    state <- State.get
    case stype state of
        Nothing -> liftIO $ Render.printWarning $ "popping " ++ show t ++ " from undefined stack"
        Just [] -> liftIO $ Render.printWarning $ "popping " ++ show t ++ " from empty stack"
        Just (s : ts) | t == s -> State.put (state { stype = Just ts })
        Just (s : _) -> liftIO $ Render.printWarning $ "popping " ++ show t ++ " from stack having " ++ show s ++ " on top"

push :: Type -> JasminChecker ()
push VoidType = return ()
push t = do
    state <- State.get
    case stype state of
        Nothing -> liftIO $ Render.printWarning $ "pushing " ++ show t ++ " on undefined stack"
        Just ts -> do
            let m = max (stack state) (sizeOf t + sum (map sizeOf ts))
            State.put (state { stype = Just (t : ts), stack = m })

define :: [Type] -> JasminChecker ()
define ts = do
    state <- State.get
    case stype state of
        Nothing -> State.put (state { stype = Just ts })
        Just ts' | ts == ts' -> liftIO $ Render.printWarning $ "setting wrong stack " ++ show ts
        Just _ -> return ()

undefineStack :: JasminChecker ()
undefineStack = do
    state <- State.get
    State.put (state { stype = Nothing })

class Jasmin a where
    jasmin :: a -> String

instance Jasmin Type where
    jasmin VoidType          = "V"
    jasmin BooleanType       = "Z"
    jasmin IntType           = "I"
    jasmin FloatType         = "F"
    jasmin DoubleType        = "D"
    jasmin CharType          = "C"
    jasmin StringType        = "Ljava/lang/String;"
    jasmin (ArrayType t)     = "[" ++ jasmin t
    jasmin (MethodType t ts) = "(" ++ concat (map jasmin ts) ++ ")" ++ jasmin t

instance Jasmin RelOp where
    jasmin JEQ = "eq"
    jasmin JNE = "ne"
    jasmin JLT = "lt"
    jasmin JLE = "le"
    jasmin JGT = "gt"
    jasmin JGE = "ge"

instance Jasmin SignOp where
    jasmin Language.NEG = "neg"

instance Jasmin BinOp where
    jasmin ADD = "add"
    jasmin SUB = "sub"
    jasmin MUL = "mul"
    jasmin DIV = "div"
    jasmin MOD = "rem"

instance Jasmin Literal where
    jasmin (Boolean False) = "0"
    jasmin (Boolean True)  = "1"
    jasmin (Int n)         = show n
    jasmin (Float n)       = show n
    jasmin (Double n)      = show n
    jasmin (Char c)        = show (ord c)
    jasmin (String s)      = show s -- FIXME?

data Code
    = LABEL Label
    | GOTO Label
    | LDC Literal
    | LOAD Type Slot
    | STORE Type Slot
    | ALOAD Type
    | ASTORE Type
    | NOP
    | POP Type
    | DUP Type
    | DUP2 Type Type
    | DUP_X2 Type Type Type
    | RETURN Type
    | CMP Type
    | IF RelOp Label
    | IFCMP Type RelOp Label
    | UNARY Type SignOp
    | BINARY Type BinOp
    | INVOKE String Id Type
    | CONVERT Type Type
    | NEWARRAY Type
    | NEWARRAYS Type Int
    | ARRAYLENGTH Type
    deriving Eq

labels :: Code -> [Label]
labels (GOTO l) = [l]
labels (IF _ l) = [l]
labels (IFCMP _ _ l) = [l]
labels _ = []

getStackType :: JasminChecker [Type]
getStackType = do
    st <- stype <$> State.get
    case st of
        Nothing -> do
            liftIO $ Render.printWarning "undefined stack"
            return []
        Just ts -> return ts

setStackType :: [Type] -> JasminChecker ()
setStackType ts = do
    state <- State.get
    case stype state of
        Nothing -> State.put (state { stype = Just ts })
        Just ss | ts == ss -> return ()
        Just ss -> liftIO $ Render.printWarning $ "setting stack to " ++ show ts ++ " but currently is " ++ show ss

setLabelType :: Label -> [Type] -> JasminChecker ()
setLabelType l ts = do
    state <- State.get
    case Map.lookup l (ltype state) of
        Nothing -> State.put (state { ltype = Map.insert l ts (ltype state) })
        Just ss | ts == ss -> return ()
        Just ss -> liftIO $ Render.printWarning $ "label " ++ show l ++ " has already type " ++ show ss

jump :: Label -> JasminChecker ()
jump l = do
    state <- State.get
    ts <- getStackType
    setLabelType l ts

access :: Type -> Slot -> JasminChecker ()
access t i = do
    state <- State.get
    State.put (state { frame = max (frame state) (i + sizeOf t) })

check :: Code -> JasminChecker ()
check (LABEL l) = do
    state <- State.get
    case Map.lookup l (ltype state) of
        Just ts -> setStackType ts
        Nothing -> do
            ts <- getStackType
            State.put (state { stype = Just ts })
check (GOTO l) = jump l >> undefineStack
check (LDC lit) = push (typeof lit)
check (LOAD t i) = push t >> access t i
check (STORE t i) = pop t >> access t i
check (ALOAD t) = do
    pop IntType
    pop (ArrayType t)
    push t
check (ASTORE t) = do
    pop t
    pop IntType
    pop (ArrayType t)
check NOP = return ()
check (POP t) = pop t
check (DUP t) = pop t >> push t >> push t
check (DUP2 t s) = pop s >> pop t >> push t >> push s >> push t >> push s
check (DUP_X2 t s r) = pop r >> pop s >> pop t >> push r >> push t >> push s >> push r
check (RETURN t) = pop t >> setStackType [] >> undefineStack
check (CMP t) = pop t >> pop t >> push IntType
check (IF _ l) = do
    pop IntType
    jump l
check (IFCMP t _ l) = pop t >> pop t >> jump l
check (UNARY t _) = pop t >> push t
check (BINARY t _) = pop t >> pop t >> push t
check (INVOKE _ _ (MethodType t ts)) = do
    forM_ (reverse ts) pop
    push t
check (CONVERT t s) = pop t >> push s
check (NEWARRAY t) = pop IntType >> push (ArrayType t)
check (NEWARRAYS t n) = do
    forM_ [1..n] (\_ -> pop IntType)
    push t
check (ARRAYLENGTH t) = pop (ArrayType t) >> push IntType

checkMethod :: [Code] -> JasminChecker ()
checkMethod is = forM_ is check

data Method = Method Id Type [Code]

outputMethod :: (String -> IO ()) -> Method -> IO ()
outputMethod output (Method m t is) = do
    state <- State.execStateT (checkMethod is) (initialJasminCheckerState t)
    output $ ".method public static " ++ m ++ jasmin t
    output $ "    .limit locals " ++ show (frame state)
    output $ "    .limit stack " ++ show (stack state)
    forM_ is (output . jasmin)
    output $ ".end method"

outputClass :: String -> [Method] -> IO ()
outputClass cls methods = do
    handle <- openFile (cls ++ ".j") WriteMode
    let output = hPutStrLn handle
    output $ ".class public " ++ cls
    output $ ".super java/lang/Object"
    forM_ methods (outputMethod output)
    hClose handle

library :: String -> Type -> [Type] -> Code
library m t ts = INVOKE "StandardLibrary" m (MethodType t ts)

instance Jasmin Code where
    jasmin (LABEL l)       = show l ++ ":"
    jasmin (GOTO l)        = "    goto " ++ show l
    jasmin (LDC lit)       = "    ldc" ++ literalDouble lit ++ " " ++ jasmin lit
    jasmin (LOAD t i)      = "    " ++ typePrefix t ++ "load " ++ show i
    jasmin (STORE t i)     = "    " ++ typePrefix t ++ "store " ++ show i
    jasmin (ALOAD t)       = "    " ++ typePrefix t ++ "aload"
    jasmin (ASTORE t)      = "    " ++ typePrefix t ++ "astore"
    jasmin (CMP t)         = "    " ++ typePrefix t ++ "cmpl"
    jasmin NOP             = "    nop"
    jasmin (POP t)         = "    pop" ++ typeDouble t
    jasmin (DUP t)         = "    dup" ++ typeDouble t
    jasmin (DUP2 t s)      = "    dup2"
    jasmin (DUP_X2 t s r)  = "    dup" ++ typeDouble r ++ "_x2"
    jasmin (RETURN t)      = "    " ++ typePrefix t ++ "return"
    jasmin (IF op l)       = "    if" ++ jasmin op ++ " " ++ show l
    jasmin (IFCMP t op l)  = "    if_" ++ typePrefix t ++ "cmp" ++ jasmin op ++ " " ++ show l
    jasmin (UNARY t op)    = "    " ++ typePrefix t ++ jasmin op
    jasmin (BINARY t op)   = "    " ++ typePrefix t ++ jasmin op
    jasmin (INVOKE c m t)  = "    invokestatic " ++ c ++ "/" ++ m ++ jasmin t
    jasmin (CONVERT t s)   = "    " ++ conversion t s
    jasmin (NEWARRAY t)    = "    newarray " ++ jasmin t
    jasmin (NEWARRAYS t n) = "    multianewarray " ++ jasmin t ++ " " ++ show n
    jasmin (ARRAYLENGTH _) = "    arraylength"

conversion :: Type -> Type -> String
conversion t StringType = "invokestatic stringOf(" ++ jasmin t ++ ")Ljava/lang/String;"
conversion IntType FloatType = "i2f"
conversion IntType DoubleType = "i2d"
conversion IntType CharType = "i2c"
conversion FloatType DoubleType = "f2d"
conversion FloatType IntType = "f2i"
conversion DoubleType IntType = "d2i"
conversion DoubleType FloatType = "d2f"

literalDouble :: Literal -> String
literalDouble (Double _) = "_w"
literalDouble _          = ""

typeDouble :: Type -> String
typeDouble t | double t  = "2"
             | otherwise = ""

typePrefix :: Type -> String
typePrefix VoidType      = ""
typePrefix (ArrayType _) = "a"
typePrefix BooleanType   = "i"
typePrefix IntType       = "i"
typePrefix FloatType     = "f"
typePrefix DoubleType    = "d"
typePrefix CharType      = "i"
typePrefix StringType    = "a"
