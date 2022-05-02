module Jasmin where

import Type

class Jasmin a where
    jasmin :: a -> String

instance Jasmin DataType where
    jasmin BooleanType = "Z"
    jasmin IntType = "I"
    jasmin FloatType = "F"
    jasmin DoubleType = "D"
    jasmin CharType = "C"
    jasmin StringType = "Ljava/lang/String;"

instance Jasmin Type where
    jasmin VoidType = "V"
    jasmin (DataType dt) = jasmin dt
    jasmin (ArrayType t) = "[" ++ jasmin t
    jasmin (MethodType rt ts) = "(" ++ concat (map jasmin ts) ++ ")" ++ jasmin rt

goto :: Label -> String
goto l = "goto " ++ show l

ldc :: Literal -> String
ldc (Boolean False) = "ldc 0"
ldc (Boolean True)  = "ldc 1"
ldc (Int n)         = "ldc " ++ show n
ldc (Float n)       = "ldc " ++ show n
ldc (Double n)      = "ldc_w " ++ show n
ldc (Char c)        = "ldc " ++ ord c
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

invokestatic :: Id -> Type -> String
invokestatic m t = "invokestatic " ++ show x ++ jasmin t

conversion :: DataType -> DataType -> String
conversion t StringType = "invokestatic conversion(" ++ jasmin t ++ ")Ljava/lang/String;"
conversion IntType FloatType = "i2f"
conversion IntType DoubleType = "i2d"
conversion IntType CharType = "i2c"
conversion FloatType DoubleType = "f2d"
conversion FloatType IntType = "f2i"
conversion DoubleType IntType = "d2i"
conversion DoubleType FloatType = "d2f"

typePrefix :: Type -> String
typePrefix (ArrayType _) = "a"
typePrefix (DataType BooleanType) = "i"
typePrefix (DataType IntType) = "i"
typePrefix (DataType FloatType) = "f"
typePrefix (DataType DoubleType) = "d"
typePrefix (DataType CharType) = "i"
typePrefix (DataType StringType) = "a"
