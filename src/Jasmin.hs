module Jasmin where

import Atoms
import Type
import Language
import Render ()
import Data.Char (ord)
import Control.Monad (forM_)

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
    | NEG Type
    | RETURN Type
    | CMP Type
    | IF RelOp Label
    | IFCMP Type RelOp Label
    | UNARY Type SignOp
    | BINARY Type BinOp
    | INVOKE Id Type
    | CONVERT Type Type
    deriving Eq

data Method = Method Id Type [Code]

printMethod :: Method -> IO ()
printMethod (Method m t is) = do
    putStrLn $ ".method public " ++ show m ++ jasmin t
    forM_ is (putStrLn . show)
    putStrLn $ ".end method"

instance Show Code where
    show (LABEL l)      = show l ++ ":"
    show (GOTO l)       = "    goto " ++ show l
    show (LDC lit)      = "    ldc" ++ literalDouble lit ++ " " ++ jasmin lit
    show (LOAD t i)     = "    " ++ typePrefix t ++ "load " ++ show i
    show (STORE t i)    = "    " ++ typePrefix t ++ "store " ++ show i
    show (ALOAD t)      = "    " ++ typePrefix t ++ "aload"
    show (ASTORE t)     = "    " ++ typePrefix t ++ "astore"
    show (CMP t)        = "    " ++ typePrefix t ++ "cmpl"
    show NOP            = "    nop"
    show (POP t)        = "    pop" ++ typeDouble t
    show (DUP t)        = "    dup" ++ typeDouble t
    show (DUP_X2 t)     = "    dup" ++ typeDouble t ++ "_x2"
    show (RETURN t)     = "    " ++ typePrefix t ++ "return"
    show (IF op l)      = "    if" ++ jasmin op ++ " " ++ show l
    show (IFCMP t op l) = "    if_" ++ typePrefix t ++ "cmp" ++ jasmin op ++ " " ++ show l
    show (UNARY t op)   = "    " ++ typePrefix t ++ jasmin op
    show (BINARY t op)  = "    " ++ typePrefix t ++ jasmin op
    show (INVOKE m t)   = "    invokestatic " ++ show m ++ jasmin t
    show (CONVERT t s)  = "    " ++ conversion t s

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
