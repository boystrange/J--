module Jasmin where

import Atoms
import Type
import Language
import Render ()
import Data.Char (ord)

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

goto :: Label -> String
goto l = "goto " ++ show l

ldc :: Literal -> String
ldc (Boolean False) = "ldc 0"
ldc (Boolean True)  = "ldc 1"
ldc (Int n)         = "ldc " ++ show n
ldc (Float n)       = "ldc " ++ show n
ldc (Double n)      = "ldc_w " ++ show n
ldc (Char c)        = "ldc " ++ show (ord c)
ldc (String s)      = "ldc " ++ show s

load :: Type -> Slot -> String
load t i = typePrefix t ++ "load " ++ show i

store :: Type -> Slot -> String
store t i = typePrefix t ++ "store " ++ show i

aload :: Type -> String
aload t = typePrefix t ++ "aload"

astore :: Type -> String
astore t = typePrefix t ++ "astore"

dup :: Type -> String
dup t | double t = "dup2"
      | otherwise = "dup"

dup_x2 :: Type -> String
dup_x2 t | double t = "dup2_x2"
         | otherwise = "dup_x2"

neg :: Type -> String
neg t = typePrefix t ++ "neg"

ret :: Type -> String
ret t = typePrefix t ++ "return"

zif :: RelOp -> Label -> String
zif op l = "if" ++ jasmin op ++ " " ++ show l

ifcmp :: Type -> RelOp -> Label -> String
ifcmp t op l = "if_" ++ typePrefix t ++ "cmp" ++ jasmin op ++ " " ++ show l

unary :: Type -> UnOp -> String
unary t NEG = typePrefix t ++ "neg"

binary :: Type -> BinOp -> String
binary t ADD = typePrefix t ++ "add"
binary t SUB = typePrefix t ++ "sub"
binary t MUL = typePrefix t ++ "mul"
binary t DIV = typePrefix t ++ "div"
binary t MOD = typePrefix t ++ "rem"

invokestatic :: Id -> Type -> String
invokestatic m t = "invokestatic " ++ show m ++ jasmin t

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
