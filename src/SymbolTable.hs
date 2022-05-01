
module SymbolTable where

import Atoms
import Type
import Exceptions
import Control.Exception (throw)

data Entry
  = Entry { entryType :: Type
          , entrySlot :: Int
          , entryInit :: Bool }

type Env = [(Id, Entry)]

type SymbolTable = [Env]

empty :: SymbolTable
empty = []

update :: Eq a => a -> b -> [(a, b)] -> [(a, b)]
update _ _ [] = error "this should not happen"
update a b ((c, _) : xs) | a == c = (a, b) : xs
update a b ((c, d) : xs) = (c, d) : update a b xs

get :: Id -> SymbolTable -> Entry
get x [] = throw $ ErrorUnknownIdentifier x
get x (env : envs) | Just entry <- lookup x env = entry
get x (_ : envs) = get x envs

has :: Id -> SymbolTable -> Bool
has x = any (\env -> x `elem` map fst env)

set :: Id -> Entry -> SymbolTable -> SymbolTable
set x entry = map (update x entry)

new :: Id -> Type -> SymbolTable -> SymbolTable
new _ _ [] = error "add entry with empty symbol table"
new x _ envs | has x envs = throw $ ErrorMultipleDeclarations x
new x t st@(env : envs) = ((x, entry) : env) : envs
  where
    entry = Entry { entryType = t
                  , entrySlot = slots st
                  , entryInit = False }

push :: SymbolTable -> SymbolTable
push envs = [] : envs

pop :: SymbolTable -> SymbolTable
pop [] = error "pop with empty symbol table"
pop (_ : envs) = envs

slots :: SymbolTable -> Int
slots = sum . map (sum . map (sizeOf . entryType . snd))
