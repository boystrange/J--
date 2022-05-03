module Jasmin where

import Atoms
import Type
import Language
import Render ()
import Data.Char (ord)
import Control.Monad (forM_)

class Jasmin a where
    jasmin :: a -> String

instance Jasmin BaseType where
    jasmin BooleanType = "Z"
    jasmin IntType = "I"
    jasmin FloatType = "F"
    jasmin DoubleType = "D"
    jasmin CharType = "C"
    jasmin StringType = "Ljava/lang/String;"

instance Jasmin Type where
    jasmin VoidType = "V"
    jasmin (BaseType dt) = jasmin dt
    jasmin (ArrayType t) = "[" ++ jasmin t
    jasmin (MethodType rt ts) = "(" ++ concat (map jasmin ts) ++ ")" ++ jasmin rt

instance Jasmin RelOp where
    jasmin JEQ = "eq"
    jasmin JNE = "ne"
    jasmin JLT = "lt"
    jasmin JLE = "le"
    jasmin JGT = "gt"
    jasmin JGE = "ge"

instance Jasmin UnOp where
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
    jasmin (String s)      = show s -- FIXME

data Code
    = LABEL Label
    | GOTO Label
    | LDC Literal
    | LOAD Type Slot
    | STORE Type Slot
    | ALOAD Type
    | ASTORE Type
    | DUP Type
    | DUP_X2 Type
    | NEG Type
    | RETURN Type
    | CMP Type
    | IF RelOp Label
    | IFCMP Type RelOp Label
    | UNARY Type UnOp
    | BINARY Type BinOp
    | INVOKE Id Type
    | CONVERT BaseType BaseType

data Method = Method Id Type [Code]

printMethod :: Method -> IO ()
printMethod (Method m t is) = do
    putStrLn $ ".method public " ++ show m ++ jasmin t
    forM_ is (putStrLn . show)
    putStrLn $ ".end method"

instance Show Code where
    show (LABEL l)              = show l ++ ":"
    show (GOTO l)               = "    goto " ++ show l
    show (LDC lit@(Double n))   = "    ldc_w " ++ jasmin lit
    show (LDC lit)              = "    ldc " ++ jasmin lit
    show (LOAD t i)             = "    " ++ typePrefix t ++ "load " ++ show i
    show (STORE t i)            = "    " ++ typePrefix t ++ "store " ++ show i
    show (ALOAD t)              = "    " ++ typePrefix t ++ "aload"
    show (ASTORE t)             = "    " ++ typePrefix t ++ "astore"
    show (CMP t)                = "    " ++ typePrefix t ++ "cmpl"
    show (DUP t) | double t     = "    dup2"
                 | otherwise    = "    dup"
    show (DUP_X2 t) | double t  = "    dup2_x2"
                    | otherwise = "    dup_x2"
    show (RETURN t)             = "    " ++ typePrefix t ++ "return"
    show (IF op l)              = "    if" ++ jasmin op ++ " " ++ show l
    show (IFCMP t op l)         = "    if_" ++ typePrefix t ++ "cmp" ++ jasmin op ++ " " ++ show l
    show (UNARY t op)           = "    " ++ typePrefix t ++ jasmin op
    show (BINARY t op)          = "    " ++ typePrefix t ++ jasmin op
    show (INVOKE m t)           = "    invokestatic " ++ show m ++ jasmin t
    show (CONVERT t s)          = "    " ++ conversion t s

conversion :: BaseType -> BaseType -> String
conversion t StringType = "invokestatic conversion(" ++ jasmin t ++ ")Ljava/lang/String;"
conversion IntType FloatType = "i2f"
conversion IntType DoubleType = "i2d"
conversion IntType CharType = "i2c"
conversion FloatType DoubleType = "f2d"
conversion FloatType IntType = "f2i"
conversion DoubleType IntType = "d2i"
conversion DoubleType FloatType = "d2f"

typePrefix :: Type -> String
typePrefix VoidType               = ""
typePrefix (ArrayType _)          = "a"
typePrefix (BaseType BooleanType) = "i"
typePrefix (BaseType IntType)     = "i"
typePrefix (BaseType FloatType)   = "f"
typePrefix (BaseType DoubleType)  = "d"
typePrefix (BaseType CharType)    = "i"
typePrefix (BaseType StringType)  = "a"
