module Jasmin where

import Atoms
import Type
import Language
import Render ()
import Data.Char (ord)
import Control.Monad (forM_, unless)
import Data.Set (Set)
import qualified Data.Set as Set
import Data.Map (Map)
import qualified Data.Map as Map
import Control.Monad.State.Lazy (StateT)
import qualified Control.Monad.State.Lazy as State
import System.IO (Handle, hClose, hFlush, hPutStrLn, openFile, IOMode(..))

data JasminCheckerState = JasminCheckerState { frame :: Int
                                             , stack :: Int
                                             , maxStack :: Int
                                             , lmap :: Map Label Int }

initialJasminCheckerState :: Type -> JasminCheckerState
initialJasminCheckerState (MethodType _ ts) = JasminCheckerState {
    frame = sum (map sizeOf ts),
    stack = 0,
    maxStack = 0,
    lmap = Map.empty
}

type JasminChecker = StateT JasminCheckerState IO

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
    | DUP_X2 Type
    | RETURN Type
    | CMP Type
    | IF RelOp Label
    | IFCMP Type RelOp Label
    | UNARY Type SignOp
    | BINARY Type BinOp
    | INVOKE String Id Type
    | CONVERT Type Type
    deriving Eq

locals :: Code -> Int
locals (LOAD t i) = i + sizeOf t
locals (STORE t i) = i + sizeOf t
locals _ = 0

labels :: Code -> [Label]
labels (GOTO l) = [l]
labels (IF _ l) = [l]
labels (IFCMP _ _ l) = [l]
labels _ = []

delta :: Code -> Int
delta (LABEL _) = 0
delta (GOTO _) = 0
delta (LDC lit) = sizeOf (typeof lit)
delta (LOAD t _) = sizeOf t
delta (STORE t _) = negate (sizeOf t)
delta (ALOAD t) = sizeOf t - 2
delta (ASTORE t) = 2 + sizeOf t
delta NOP = 0
delta (POP t) = negate (sizeOf t)
delta (DUP t) = sizeOf t
delta (DUP_X2 t) = sizeOf t
delta (RETURN t) = negate (sizeOf t)
delta (CMP t) = 1 - 2 * sizeOf t
delta (IF _ _) = -1
delta (IFCMP t _ _) = negate (2 * sizeOf t)
delta (UNARY _ _) = 0
delta (BINARY t _) = negate (sizeOf t)
delta (INVOKE _ _ (MethodType t ts)) = sizeOf t - sum (map sizeOf ts)
delta (CONVERT t s) = sizeOf s - sizeOf t

checkLabel :: Label -> JasminChecker ()
checkLabel l = do
    state <- State.get
    let m = stack state
    case Map.lookup l (lmap state) of
        Just n -> do
            unless (m == n) $ error $ "label " ++ show l ++ " was defined at " ++ show n ++ " but now the stack is " ++ show m
        Nothing -> do
            let lmap' = Map.insert l m (lmap state)
            State.put (state { lmap = lmap' })

checkCode :: Code -> JasminChecker ()
checkCode (LABEL l) = checkLabel l
checkCode i = do
    state <- State.get
    let frame' = max (frame state) (locals i)
    let stack' = stack state + delta i
    let maxStack' = max (maxStack state) stack'
    unless (stack' >= 0) $ error "negative stack!"
    State.put (state { frame = frame', stack = stack', maxStack = maxStack' })
    forM_ (labels i) checkLabel

checkMethod :: [Code] -> JasminChecker ()
checkMethod is = forM_ is checkCode

data Method = Method Id Type [Code]

outputMethod :: (String -> IO ()) -> Method -> IO ()
outputMethod output (Method m t is) = do
    state <- State.execStateT (checkMethod is) (initialJasminCheckerState t)
    output $ ".method public static " ++ show m ++ jasmin t
    output $ "    .limit locals " ++ show (frame state)
    output $ "    .limit stack " ++ show (maxStack state)
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
library m t ts = INVOKE "StandardLibrary" (Id Somewhere m) (MethodType t ts)

instance Jasmin Code where
    jasmin (LABEL l)      = show l ++ ":"
    jasmin (GOTO l)       = "    goto " ++ show l
    jasmin (LDC lit)      = "    ldc" ++ literalDouble lit ++ " " ++ jasmin lit
    jasmin (LOAD t i)     = "    " ++ typePrefix t ++ "load " ++ show i
    jasmin (STORE t i)    = "    " ++ typePrefix t ++ "store " ++ show i
    jasmin (ALOAD t)      = "    " ++ typePrefix t ++ "aload"
    jasmin (ASTORE t)     = "    " ++ typePrefix t ++ "astore"
    jasmin (CMP t)        = "    " ++ typePrefix t ++ "cmpl"
    jasmin NOP            = "    nop"
    jasmin (POP t)        = "    pop" ++ typeDouble t
    jasmin (DUP t)        = "    dup" ++ typeDouble t
    jasmin (DUP_X2 t)     = "    dup" ++ typeDouble t ++ "_x2"
    jasmin (RETURN t)     = "    " ++ typePrefix t ++ "return"
    jasmin (IF op l)      = "    if" ++ jasmin op ++ " " ++ show l
    jasmin (IFCMP t op l) = "    if_" ++ typePrefix t ++ "cmp" ++ jasmin op ++ " " ++ show l
    jasmin (UNARY t op)   = "    " ++ typePrefix t ++ jasmin op
    jasmin (BINARY t op)  = "    " ++ typePrefix t ++ jasmin op
    jasmin (INVOKE c m t) = "    invokestatic " ++ c ++ "/" ++ show m ++ jasmin t
    jasmin (CONVERT t s)  = "    " ++ conversion t s

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
