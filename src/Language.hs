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
import Type

data Literal
  = Boolean Bool
  | Int Int
  | Float Float
  | Double Double
  | Char Char
  | String String

data BinOp
  = ADD
  | SUB
  | MUL
  | DIV
  | MOD

data RelOp
  = JLT
  | JLE
  | JGT
  | JGE
  | JEQ
  | JNE

data UnOp = NEG | POS

data IncDecOp = PREINC | PREDEC | POSTINC | POSTDEC

binary :: BinOp -> DataType -> Maybe DataType
binary ADD t          | isNumeric t = Just t
binary ADD CharType   = Just IntType
binary ADD StringType = Just StringType
binary SUB t          | isNumeric t = Just t
binary SUB CharType   = Just IntType
binary MUL t          | isNumeric t = Just t
binary DIV t          | isNumeric t = Just t
binary MOD IntType    = Just IntType
binary _   _          = Nothing

unary :: UnOp -> DataType -> Maybe DataType
unary NEG t           | isNumeric t = Just t
unary POS t           | isNumeric t = Just t
unary _   _           = Nothing

incdec :: DataType -> Maybe DataType
incdec t        | isNumeric t = Just t
incdec CharType = Just CharType
incdec _        = Nothing

typeOfLiteral :: Literal -> DataType
typeOfLiteral (Int _) = IntType
typeOfLiteral (Boolean _) = BooleanType
typeOfLiteral (Float _) = FloatType
typeOfLiteral (Double _) = DoubleType
typeOfLiteral (Char _) = CharType
typeOfLiteral (String _) = StringType
