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

module Language where

import Atoms
import Type

data Literal
  = Boolean Bool
  | Int Int
  | Long Integer
  | Float Float
  | Double Double
  | Char Char
  | String String
  deriving Eq

data BinOp
  = ADD
  | SUB
  | MUL
  | DIV
  | MOD
  deriving Eq

data RelOp
  = JLT
  | JLE
  | JGT
  | JGE
  | JEQ
  | JNE
  deriving Eq

notRel :: RelOp -> RelOp
notRel JLT = JGE
notRel JLE = JGT
notRel JGT = JLE
notRel JGE = JLT
notRel JEQ = JNE
notRel JNE = JEQ

data SignOp = NEG | POS
  deriving Eq

one :: Type -> Literal
one ByteType = Int 1
one ShortType = Int 1
one IntType = Int 1
one LongType = Long 1
one FloatType = Float 1
one DoubleType = Double 1

data StepOp = PRE | POST
  deriving Eq

class Typed a where
  typeof :: a -> Type

instance Typed Literal where
  typeof (Int n)     | -128 <= n && n < 128 = ByteType
                     | -32768 <= n && n < 32768 = ShortType
                     | otherwise = IntType
  typeof (Long _)    = LongType
  typeof (Boolean _) = BooleanType
  typeof (Float _)   = FloatType
  typeof (Double _)  = DoubleType
  typeof (Char _)    = CharType
  typeof (String _)  = StringType
