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
  | ErrorNotMethod Id

instance Exception MyException

instance Show MyException where
  show (ErrorSyntax msg) = msg
  show (ErrorMultipleDeclarations x) = "multiple declarations: " ++ showWithPos x
  show (ErrorWrongNumberOfArguments x en an) = "wrong number of arguments when calling " ++ showWithPos x ++": expected " ++ show en ++ ", actual " ++ show an
  show (ErrorNotMethod x) = "call of non-method " ++ showWithPos x
