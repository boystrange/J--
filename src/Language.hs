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

data DataType
  = BooleanType
  | IntType
  | FloatType
  | DoubleType
  | CharType
  | StringType
  deriving Eq

data Type
  = VoidType
  | DataType DataType
  | ArrayType Type
  | MethodType Type [Type]
  deriving Eq

isNumeric :: DataType -> Bool
isNumeric IntType = True
isNumeric FloatType = True
isNumeric DoubleType = True
isNumeric _ = False

isEnumeration :: DataType -> Bool
isEnumeration t = isNumeric t || t == CharType

sizeOf :: Type -> Int
sizeOf VoidType = 0
sizeOf (DataType DoubleType) = 2
sizeOf (DataType _) = 1
sizeOf (ArrayType _) = 1
sizeOf (MethodType _ _) = 0

merge :: Type -> Type -> Type
merge t s | t `subtype` s = s
          | s `subtype` t = t

subdatatype :: DataType -> DataType -> Bool
subdatatype CharType IntType = True
subdatatype CharType FloatType = True
subdatatype CharType DoubleType = True
subdatatype IntType FloatType = True
subdatatype IntType DoubleType = True
subdatatype FloatType DoubleType = True
subdatatype _ StringType = True
subdatatype t s = t == s

subtype :: Type -> Type -> Bool
subtype (DataType dt1) (DataType dt2) = subdatatype dt1 dt2
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
  | Assign Reference Expression
  | Ref Reference
  | Unary UnOp Expression
  | Binary BinOp Expression Expression
  | IncDec IncDecOp Reference
  | Cast Type Expression

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

data IncDecOp
  = PREINC
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

typeOfLiteral :: Literal -> DataType
typeOfLiteral (Int _) = IntType
typeOfLiteral (Boolean _) = BooleanType
typeOfLiteral (Float _) = FloatType
typeOfLiteral (Double _) = DoubleType
typeOfLiteral (Char _) = CharType
typeOfLiteral (String _) = StringType

typeOfMethod :: Method -> (Id, Type)
typeOfMethod (Method t x binds _) = (x, MethodType t (map snd binds))

