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

-- Copyright 2022 Luca Padovani

module Exceptions where

import Atoms
import Type
import Language
import SourceLanguage
import Render ()
import Control.Exception (Exception)

-- |The type of FairCheck exceptions.
data MyException
  = ErrorSyntax Pos String
  | ErrorVoidReturn Pos Type
  | ErrorWrongNumberOfArguments (Located Id) Int Int
  | ErrorTypeMismatch Pos Type Type
  | ErrorStepOperator Pos StepOp Type
  | ErrorUnaryOperator Pos SignOp Type
  | ErrorBinaryOperator Pos BinOp Type Type
  | ErrorBinaryRelation Pos RelOp Type Type
  | ErrorMethodExpected (Located Id) Type
  | ErrorArrayExpected Pos Type
  | ErrorArrayInitializer Pos Type
  | ErrorMissingReturn (Located Id)
  | ErrorUnknownIdentifier (Located Id)
  | ErrorMultipleDeclarations (Located Id)

instance Exception MyException

instance Positioned MyException where
  posof (ErrorSyntax pos _) = pos
  posof (ErrorVoidReturn pos _) = pos
  posof (ErrorWrongNumberOfArguments x _ _) = locatedPos x
  posof (ErrorStepOperator pos _ _) = pos
  posof (ErrorUnaryOperator pos _ _) = pos
  posof (ErrorBinaryOperator pos _ _ _) = pos
  posof (ErrorTypeMismatch pos _ _) = pos
  posof (ErrorBinaryRelation pos _ _ _) = pos
  posof (ErrorMethodExpected x _) = locatedPos x
  posof (ErrorArrayExpected pos _) = pos
  posof (ErrorArrayInitializer pos _) = pos
  posof (ErrorMissingReturn x) = locatedPos x
  posof (ErrorUnknownIdentifier x) = locatedPos x
  posof (ErrorMultipleDeclarations x) = locatedPos x

instance Show MyException where
  show (ErrorSyntax _ tok) = "syntax error at token '" ++ tok ++ "'"
  show (ErrorUnknownIdentifier x) = "unknown reference to " ++ show x
  show (ErrorMultipleDeclarations x) = "multiple declarations of " ++ show x
  show (ErrorWrongNumberOfArguments x en an) = "wrong number of arguments when calling " ++ show x ++", expected " ++ show en ++ ", actual " ++ show an
  show (ErrorMissingReturn x) = "missing return statement for method " ++ show x
  show (ErrorVoidReturn _ t) = "method should return value of type " ++ show t
  show (ErrorArrayExpected _ t) = "array expected, actual type " ++ show t
  show (ErrorArrayInitializer _ t) = "using array initializer for type " ++ show t
  show (ErrorMethodExpected x t) = show x ++ " is not a method, its type is " ++ show t
  show (ErrorStepOperator _ op t) = show op ++ " operator cannot be applied to operand of type " ++ show t
  show (ErrorUnaryOperator _ op t) = "unary operator " ++ show op ++ " cannot be applied to operand of type " ++ show t
  show (ErrorBinaryOperator _ op t s) = "binary operator " ++ show op ++ " cannot be applied to operands of type " ++ show t ++ " and " ++ show s
  show (ErrorBinaryRelation _ op t s) = "relation operator " ++ show op ++ " cannot compare operands of type " ++ show t ++ " and " ++ show s
  show (ErrorTypeMismatch _ et at) = "type mismatch, expected type " ++ show et ++ ", actual type " ++ show at