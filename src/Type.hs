-- This file is part of J--

-- J-- is free software: you can redistribute it and/or modify it
-- under the terms of the GNU General Public License as published by
-- the Free Software Foundation, either version 3 of the License, or
-- (at your option) any later version.

-- J-- is distributed in the hope that it will be useful, but
-- WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.

-- You should have received a copy of the GNU General Public License
-- along with J--. If not, see <http://www.gnu.org/licenses/>.

-- Copyright 2022 Luca Padovani

module Type where

data BaseType
  = BooleanType
  | IntType
  | FloatType
  | DoubleType
  | CharType
  | StringType
  deriving Eq

data Type
  = VoidType
  | BaseType BaseType
  | ArrayType Type
  | MethodType Type [Type]
  deriving Eq

isNumeric :: BaseType -> Bool
isNumeric IntType = True
isNumeric FloatType = True
isNumeric DoubleType = True
isNumeric _ = False

isFloating :: BaseType -> Bool
isFloating FloatType = True
isFloating DoubleType = True
isFloating _ = False

isEnumeration :: BaseType -> Bool
isEnumeration t = isNumeric t || t == CharType

sizeOf :: Type -> Int
sizeOf VoidType = 0
sizeOf (BaseType DoubleType) = 2
sizeOf (BaseType _) = 1
sizeOf (ArrayType _) = 1
sizeOf (MethodType _ _) = 0

double :: Type -> Bool
double (BaseType DoubleType) = True
double _ = False

merge :: Type -> Type -> Type
merge t s | t `subtype` s = s
          | s `subtype` t = t

subdatatype :: BaseType -> BaseType -> Bool
subdatatype CharType IntType = True
subdatatype CharType FloatType = True
subdatatype CharType DoubleType = True
subdatatype IntType FloatType = True
subdatatype IntType DoubleType = True
subdatatype FloatType DoubleType = True
subdatatype _ StringType = True
subdatatype t s = t == s

subtype :: Type -> Type -> Bool
subtype (BaseType dt1) (BaseType dt2) = subdatatype dt1 dt2
subtype t s = t == s
