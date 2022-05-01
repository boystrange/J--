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

data Method r = Method Type Id [(Id, Type)] (Statement r)

data Statement r
  = Empty
  | If (Expression r) (Statement r) (Statement r)
  | While (Expression r) (Statement r)
  | Do (Statement r) (Expression r)
  | Return (Maybe (Expression r))
  | Block (Statement r)
  | Local Type Id
  | Expression (Expression r)
  | Seq (Statement r) (Statement r)

data Reference
  = IdRef Id
  | ArrayRef Reference (Expression Reference)

type Slot = Int

data TypedReference
  = TypedIdRef Id Type Slot
  | TypedArrayRef TypedReference (Expression TypedReference)

data Literal
  = Int Int
  | Boolean Bool
  | Float Float
  | Double Double
  | Char Char
  | String String

data Expression r
  = Literal Literal
  | Call Id [(Expression r)]
  | New Type (Expression r)
  | Assign r (Expression r)
  | Ref r
  | Unary UnOp (Expression r)
  | Binary BinOp (Expression r) (Expression r)
  | IncDec IncDecOp r
  | Cast Type (Expression r)
  | Conversion (Expression r) Type Type

type SourceMethod     = Method Reference
type SourceStatement  = Statement Reference
type SourceExpression = Expression Reference

type TypedMethod      = Method TypedReference
type TypedStatement   = Statement TypedReference
type TypedExpression  = Expression TypedReference

data BinOp
  = ADD
  | SUB
  | MUL
  | DIV
  | MOD
  | JLT
  | JLE
  | JGT
  | JGE
  | JEQ
  | JNE
  | AND
  | OR

data UnOp
  = NEG
  | POS
  | NOT

data IncDecOp
  = PREINC
  | PREDEC
  | POSTINC
  | POSTDEC

binary :: BinOp -> DataType -> Maybe DataType
binary ADD t           | isNumeric t = Just t
binary ADD CharType    = Just IntType
binary ADD StringType  = Just StringType
binary SUB t           | isNumeric t = Just t
binary SUB CharType    = Just IntType
binary MUL t           | isNumeric t = Just t
binary DIV t           | isNumeric t = Just t
binary MOD IntType     = Just IntType
binary JLT _           = Just BooleanType
binary JGT _           = Just BooleanType
binary JLE _           = Just BooleanType
binary JGE _           = Just BooleanType
binary JEQ _           = Just BooleanType
binary JNE _           = Just BooleanType
binary AND BooleanType = Just BooleanType
binary OR  BooleanType = Just BooleanType
binary _   _           = Nothing

unary :: UnOp -> DataType -> Maybe DataType
unary NEG t           | isNumeric t = Just t
unary POS t           | isNumeric t = Just t
unary NOT BooleanType = Just BooleanType
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

typeOfMethod :: Method r -> (Id, Type)
typeOfMethod (Method t x binds _) = (x, MethodType t (map snd binds))
