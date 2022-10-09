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

module Type where

data Type
  = VoidType
  | BooleanType
  | ByteType
  | ShortType
  | IntType
  | LongType
  | FloatType
  | DoubleType
  | CharType
  | StringType
  | ArrayType Type
  | MethodType Type [Type]
  deriving Eq

isNumeric :: Type -> Bool
isNumeric ByteType = True
isNumeric ShortType = True
isNumeric IntType = True
isNumeric LongType = True
isNumeric FloatType = True
isNumeric DoubleType = True
isNumeric _ = False

isString :: Type -> Bool
isString StringType = True
isString _ = False

isFloating :: Type -> Bool
isFloating FloatType = True
isFloating DoubleType = True
isFloating _ = False

isEnumeration :: Type -> Bool
isEnumeration CharType = True
isEnumeration t = isNumeric t

isStringable :: Type -> Bool
isStringable VoidType = False
isStringable (ArrayType _) = False
isStringable (MethodType _ _) = False
isStringable _ = True

sizeOf :: Type -> Int
sizeOf VoidType = 0
sizeOf (MethodType _ _) = 0
sizeOf DoubleType = 2
sizeOf LongType = 2
sizeOf _ = 1

double :: Type -> Bool
double t = sizeOf t == 2

merge :: Type -> Type -> Maybe Type
merge t s | widening t s = Just s
          | widening s t = Just t
merge _ _ = Nothing

widening :: Type -> Type -> Bool
widening ByteType  CharType   = True
widening ByteType  ShortType  = True
widening ByteType  IntType    = True
widening ByteType  LongType   = True
widening ByteType  FloatType  = True
widening ByteType  DoubleType = True
widening CharType  ShortType  = True
widening CharType  IntType    = True
widening CharType  LongType   = True
widening CharType  FloatType  = True
widening CharType  DoubleType = True
widening ShortType ShortType  = True
widening ShortType IntType    = True
widening ShortType LongType   = True
widening ShortType FloatType  = True
widening ShortType DoubleType = True
widening IntType   LongType   = True
widening IntType   FloatType  = True
widening IntType   DoubleType = True
widening LongType  FloatType  = True
widening LongType  DoubleType = True
widening FloatType DoubleType = True
widening t         s          = t == s

narrowing :: Type -> Type -> Bool
narrowing = flip widening

casting :: Type -> Type -> Bool
casting t s = widening t s || narrowing t s
