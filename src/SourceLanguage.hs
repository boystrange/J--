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

module SourceLanguage where

import Atoms
import Type
import Language

data Expression
  = Literal Literal
  | IdRef (Located Id)
  | ArrayRef Expression Expression
  | Call (Located Id) [Expression]
  | New Pos Type [Expression]
  | Array Pos Type InitExpression
  | Length Pos Expression
  | Assign Pos Expression Expression
  | Unary Pos SignOp Expression
  | Binary Pos BinOp Expression Expression
  | Ternary Pos Expression Expression Expression
  | Step Pos StepOp SignOp Expression
  | Cast Pos Type Expression
  | Rel Pos RelOp Expression Expression
  | And Expression Expression
  | Or Expression Expression
  | Not Expression

instance Positioned Expression where
  posof (Literal _) = Somewhere
  posof (Call x _) = posof x
  posof (New pos _ _) = pos
  posof (Array pos _ _) = pos
  posof (ArrayRef expr _) = posof expr
  posof (Length pos _) = pos
  posof (Assign pos _ _) = pos
  posof (IdRef x) = posof x
  posof (Unary pos _ _) = pos
  posof (Binary pos _ _ _) = pos
  posof (Ternary pos _ _ _) = pos
  posof (Step pos _ _ _) = pos
  posof (Cast pos _ _) = pos
  posof (Rel pos _ _ _) = pos
  posof (And expr _) = posof expr
  posof (Or expr _) = posof expr
  posof (Not expr) = posof expr

data InitExpression
  = NoInit
  | SimpleInit Expression
  | ArrayInit Pos [InitExpression]

data Statement
  = Skip
  | If Expression Statement Statement
  | While Expression Statement
  | Do Statement Expression
  | Return Pos (Maybe Expression)
  | Block Statement
  | Local Type (Located Id)
  | Ignore Expression
  | Seq Statement Statement

data Method = Method { methodReturnType :: Type
                     , methodName :: Located Id
                     , methodArgs :: [(Located Id, Type)]
                     , methodBody :: Statement }

methodId :: Method -> Id
methodId = locatedData . methodName

methodPos :: Method -> Pos
methodPos = locatedPos . methodName

methodType :: Method -> Type
methodType m = MethodType (methodReturnType m) (map snd (methodArgs m))

returns :: Statement -> Bool
returns Skip = False
returns (If _ stmt1 stmt2) = returns stmt1 && returns stmt2
returns (While _ _) = False
returns (Do stmt _) = returns stmt
returns (Return _ _) = True
returns (Block stmt) = returns stmt
returns (Local _ _) = False
returns (Ignore _) = False
returns (Seq stmt1 stmt2) = returns stmt1 || returns stmt2
