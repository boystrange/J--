module SourceLanguage where

import Atoms
import Type
import Language

data Reference
  = IdRef Id
  | ArrayRef Reference Expression

data Expression
  = Literal Literal
  | Call Id [Expression]
  | New Type Expression
  | Assign Reference Expression
  | Ref Reference
  | Unary SignOp Expression
  | Binary BinOp Expression Expression
  | Step StepOp SignOp Reference
  | Cast Type Expression
  | Rel RelOp Expression Expression
  | And Expression Expression
  | Or Expression Expression
  | Not Expression

data Statement
  = Skip
  | If Expression Statement Statement
  | While Expression Statement
  | Do Statement Expression
  | Return (Maybe Expression)
  | Block Statement
  | Local Type Id
  | Ignore Expression
  | Seq Statement Statement

data Method = Method Type Id [(Id, Type)] Statement

typeOfMethod :: Method -> (Id, Type)
typeOfMethod (Method t x binds _) = (x, MethodType t (map snd binds))

returns :: Statement -> Bool
returns Skip = False
returns (If _ stmt1 stmt2) = returns stmt1 && returns stmt2
returns (While _ _) = False
returns (Do stmt _) = returns stmt
returns (Return _) = True
returns (Block stmt) = returns stmt
returns (Local _ _) = False
returns (Ignore _) = False
returns (Seq stmt1 stmt2) = returns stmt1 || returns stmt2
