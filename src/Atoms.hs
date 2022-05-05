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

-- |This module defines the representation of __identifiers__.
module Atoms where

-- |A position refers to line and column within a script. The constructor
-- 'Somewhere' builds an unknown position.
data Pos = Somewhere
         | At (Int, Int)

instance Show Pos where
  show Somewhere = ""
  show (At (l, c)) = " [line " ++ show l ++ "]"

data Id = Id { identifierPos :: Pos
             , identifierName :: String }
instance Show Id where
  show = identifierName

-- |Show an identifier along with its position in the source code, if known.
showWithPos :: Id -> String
showWithPos u = show u ++ show (identifierPos u)

-- |Two identifiers are the same regardless of the position in which they occur.
instance Eq Id where
  (==) u v = show u == show v

-- |Two identifiers are ordered regardless of the position in which they occur.
instance Ord Id where
  compare u v = compare (show u) (show v)

type Slot = Int

data Label = L Int
    deriving (Eq, Ord)

instance Enum Label where
  toEnum = L
  fromEnum (L n) = n