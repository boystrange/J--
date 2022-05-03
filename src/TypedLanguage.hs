module TypedLanguage where

import Atoms
import Type
import Language

data Reference
  = IdRef Type Slot Id
  | ArrayRef Type Reference Expression

data Expression
  = Literal Literal
  | Call Type Id [Expression]
  | New Type Expression
  | Assign Reference Expression
  | Ref Reference
  | Unary Type UnOp Expression
  | Binary Type BinOp Expression Expression
  | IncDec Type IncDecOp Reference
  | Conversion DataType DataType Expression
  | FromProposition Proposition

data Proposition
  = TrueProp
  | FalseProp
  | Rel Type RelOp Expression Expression
  | And Proposition Proposition
  | Or Proposition Proposition
  | Not Proposition
  | FromExpression Expression

type PropositionOrExpression = Either Proposition Expression

data Statement
  = Skip
  | If Proposition Statement Statement
  | While Proposition Statement
  | Do Statement Proposition
  | Return Type (Maybe Expression)
  | Expression Expression
  | Seq Statement Statement

data Method = Method Type Id Statement
