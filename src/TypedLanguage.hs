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

module TypedLanguage where

import Atoms
import Type
import Language

data Reference
  = IdRef Type Slot Id
  | ArrayRef Type Reference Expression

data Expression
  = Literal Literal
  | Ref Reference
  | Call Type String Id [Expression]
  | New Type [Expression]
  | Array Type InitExpression
  | Assign Reference Expression
  | Unary Type SignOp Expression
  | Binary Type BinOp Expression Expression
  | Ternary Type Proposition Expression Expression
  | Step Type StepOp SignOp Reference
  | Convert Type Expression
  | StringOf Type Expression

data InitExpression
  = SimpleInit Expression
  | ArrayInit [InitExpression]

data Proposition
  = TrueProp
  | FalseProp
  | Rel Type RelOp Expression Expression
  | And Proposition Proposition
  | Or Proposition Proposition
  | Not Proposition
  | FromExpression Expression

data Statement
  = Skip
  | If Proposition Statement Statement
  | While Proposition Statement
  | Do Statement Proposition
  | Return Type (Maybe Expression)
  | Ignore Expression
  | Seq Statement Statement

data Method = Method Type Id Statement

instance Typed Reference where
  typeof (IdRef t _ _) = t
  typeof (ArrayRef t _ _) = t

instance Typed Expression where
  typeof (Literal lit) = typeof lit
  typeof (Ref ref) = typeof ref
  typeof (Call rt _ _ _) = rt
  typeof (New t exprs) = foldr (const ArrayType) t exprs
  typeof (Array t _) = t
  typeof (Assign ref _) = typeof ref
  typeof (Unary t _ _) = t
  typeof (Binary t _ _ _) = t
  typeof (Ternary t _ _ _) = t
  typeof (Step t _ _ _) = t
  typeof (Convert t _) = t
  typeof (StringOf _ _) = StringType
