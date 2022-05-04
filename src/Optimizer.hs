module Optimizer where

import Atoms
import Language
import Jasmin

import Data.Set (Set)
import qualified Data.Set as Set

usedLabels :: Code -> Set Label
usedLabels (GOTO l) = Set.singleton l
usedLabels (IF _ l) = Set.singleton l
usedLabels (IFCMP _ _ l) = Set.singleton l
usedLabels _ = Set.empty

useful :: Set Label -> Code -> Bool
useful lset (LABEL l) = Set.member l lset
useful _ _ = True

optimize :: [Code] -> [Code]
optimize = aux
    where
        aux [] = []
        aux (GOTO l1 : LABEL l2 : is) | l1 == l2 = aux (LABEL l2 : is)
        aux (DUP s1 : STORE t n : POP s2 : is) | s1 == s2 = aux (STORE t n : is)
        aux (IFCMP t rel l1 : GOTO l2 : LABEL l3 : is) | l1 == l3 = aux (IFCMP t (notRel rel) l2 : is)
        aux (i : is) = i : aux is

optimizeMethod :: Method -> Method
optimizeMethod (Method x t is) = Method x t (filter (useful lset) is')
    where
        is' = optimize is
        lset = Set.unions (map usedLabels is')

optimizeMethods :: [Method] -> [Method]
optimizeMethods = map optimizeMethod
