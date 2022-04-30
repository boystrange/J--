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

module Language where

import Atoms

data AtomicType
  = VoidType
  | BooleanType
  | IntType
  | FloatType
  | DoubleType
  | CharType
  | StringType
  deriving Eq

data Type
  = AtomicType AtomicType
  | ArrayType Type
  | MethodType Type [Type]
  deriving Eq

sizeOf :: Type -> Int
sizeOf (AtomicType VoidType) = 0
sizeOf (AtomicType DoubleType) = 2
sizeOf (AtomicType _) = 1
sizeOf (ArrayType _) = 1
sizeOf (MethodType _ _) = 0

union :: Type -> Type -> Type
union t s | t `subtype` s = s
          | s `subtype` t = t

subtype :: Type -> Type -> Bool
subtype (AtomicType CharType) (AtomicType IntType) = True
subtype (AtomicType CharType) (AtomicType FloatType) = True
subtype (AtomicType CharType) (AtomicType DoubleType) = True
subtype (AtomicType IntType) (AtomicType FloatType) = True
subtype (AtomicType IntType) (AtomicType DoubleType) = True
subtype (AtomicType FloatType) (AtomicType DoubleType) = True
subtype t s = t == s

data Method = Method Type Id [(Id, Type)] Statement

data Statement
  = Empty
  | If Expression Statement Statement
  | While Expression Statement
  | Do Statement Expression
  | Return (Maybe Expression)
  | Block Statement
  | Local Type Id
  | Expression Expression
  | Seq Statement Statement

data Reference
  = IdRef Id
  | ArrayRef Reference Expression

data Expression
  = Literal Literal
  | Call Id [Expression]
  | New Type Expression
  | Ref Reference
  | Unary UnOp Expression
  | Binary BinOp Expression Expression
  | Cast Type Expression

data BinOp
  = ASSIGN
  | ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | LT
  | LE
  | GT
  | GE
  | EQ
  | NE
  | AND
  | OR

data UnOp
  = NEG
  | POS
  | NOT
  | PREINC
  | PREDEC
  | POSTINC
  | POSTDEC

data Literal
  = Int Int
  | Boolean Bool
  | Float Float
  | Double Double
  | Char Char
  | String String

typeOfLiteral :: Literal -> AtomicType
typeOfLiteral (Int _) = IntType
typeOfLiteral (Boolean _) = BooleanType
typeOfLiteral (Float _) = FloatType
typeOfLiteral (Double _) = DoubleType
typeOfLiteral (Char _) = CharType
typeOfLiteral (String _) = StringType

typeOfMethod :: Method -> (Id, Type)
typeOfMethod (Method t x binds _) = (x, MethodType t (map snd binds))

