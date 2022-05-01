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

module Exceptions where

import Atoms
import Language
import Render ()
import Control.Exception (Exception)

-- |The type of FairCheck exceptions.
data MyException
  = ErrorSyntax String
  | ErrorUnknownIdentifier Id
  | ErrorMultipleDeclarations Id
  | ErrorTypeMismatch Type Type
  | ErrorWrongNumberOfArguments Id Int Int
  | ErrorNotMethod Id Type
  | ErrorMissingReturn Id
  | ErrorArrayExpected Reference Type
  | ErrorUnaryOperator UnOp Type
  | ErrorBinaryOperator BinOp Type Type
  | ErrorIncDecOperator IncDecOp Type

instance Exception MyException

instance Show MyException where
  show (ErrorSyntax msg) = msg
  show (ErrorUnknownIdentifier x) = "unknown reference to " ++ showWithPos x
  show (ErrorMultipleDeclarations x) = "multiple declarations of " ++ showWithPos x
  show (ErrorWrongNumberOfArguments x en an) = "wrong number of arguments when calling " ++ showWithPos x ++", expected " ++ show en ++ ", actual " ++ show an
  show (ErrorNotMethod x t) = showWithPos x ++ " is not a method, it has type " ++ show t
  show (ErrorMissingReturn x) = "missing return statement for method " ++ showWithPos x
  show (ErrorTypeMismatch et at) = "expected type " ++ show et ++ ", actual type " ++ show at
  show (ErrorArrayExpected ref t) = show ref ++ " is not an array, it has type " ++ show t
  show (ErrorUnaryOperator op t) = "unary operator " ++ show op ++ " cannot be applied to operand of type " ++ show t
  show (ErrorBinaryOperator op t s) = "binary operator " ++ show op ++ " cannot be applied to operands of type " ++ show t ++ " and " ++ show s
  show (ErrorIncDecOperator op t) = "unary operator " ++ show op ++ " cannot be applied to operand of type " ++ show t
