module TypedLanguage where

import Atoms
import Type
import Language

data Reference
  = IdRef Type Slot Id
  | ArrayRef Type Reference Expression

instance Typed Reference where
  typeof (IdRef t _ _) = t
  typeof (ArrayRef t _ _) = t

data Expression
  = Literal Literal
  | Ref Reference
  | Call Type Id [Expression]
  | New Type Expression
  | Assign Reference Expression
  | Unary Type SignOp Expression
  | Binary Type BinOp Expression Expression
  | Step Type StepOp SignOp Reference
  | Convert Type Expression
  | StringOf Type Expression
  | FromProposition Proposition

instance Typed Expression where
  typeof (Literal lit) = typeof lit
  typeof (Ref ref) = typeof ref
  typeof (Call rt _ _) = rt
  typeof (New t _) = ArrayType t
  typeof (Assign ref _) = typeof ref
  typeof (Unary t _ _) = t
  typeof (Binary t _ _ _) = t
  typeof (Step t _ _ _) = t
  typeof (Convert t _) = t
  typeof (StringOf _ _) = StringType
  typeof (FromProposition _) = BooleanType

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
  | Ignore Expression
  | Seq Statement Statement

data Method = Method Type Id Statement
