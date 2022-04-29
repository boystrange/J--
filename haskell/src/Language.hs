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

data Type
  = AtomicType AtomicType
  | ArrayType Type
  | MethodType Type [Type]

data Method = Method Type Id [(Type, Id)] Statement

data Statement
  = Empty
  | If Expression Statement Statement
  | While Expression Statement
  | Do Statement Expression
  | Return (Maybe Expression)
  | Block [Statement]
  | Locals Type [(Id, Maybe Expression)]
  | Expression Expression

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
