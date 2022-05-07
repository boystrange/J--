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

module SymbolTable where

import Atoms
import Type
import Exceptions
import Control.Exception (throw)

data Entry
  = Entry { entryType  :: Type
          , entryClass :: Maybe String
          , entrySlot  :: Int
          , entryInit  :: Bool }

type Env = [(Located Id, Entry)]

type SymbolTable = [Env]

empty :: SymbolTable
empty = []

update :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
update _ _ [] = error "this should not happen"
update a b ((c, _) : xs) | a == c = (a, b) : xs
update a b ((c, d) : xs) = (c, d) : update a b xs

get :: Located Id -> SymbolTable -> Entry
get x [] = throw $ ErrorUnknownIdentifier x
get x (env : envs) | Just entry <- lookup x env = entry
get x (_ : envs) = get x envs

has :: Located Id -> SymbolTable -> Bool
has x = any (\env -> x `elem` map fst env)

set :: Located Id -> Entry -> SymbolTable -> SymbolTable
set x entry = map (update x entry)

new :: Maybe String -> Located Id -> Type -> SymbolTable -> SymbolTable
new _ _ _ [] = error "add entry with empty symbol table"
new _ x _ envs | has x envs = throw $ ErrorMultipleDeclarations x
new c x t st@(env : envs) = ((x, entry) : env) : envs
  where
    entry = Entry { entryType  = t
                  , entryClass = c
                  , entrySlot  = slots st
                  , entryInit  = False }

push :: SymbolTable -> SymbolTable
push envs = [] : envs

pop :: SymbolTable -> SymbolTable
pop [] = error "pop with empty symbol table"
pop (_ : envs) = envs

slots :: SymbolTable -> Int
slots = sum . map (sum . map (sizeOf . entryType . snd))
