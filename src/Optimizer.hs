module Optimizer where

import Common (limit)
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

peephole :: [Code] -> [Code]
peephole = aux
    where
        aux [] = []
        aux (GOTO l1 : LABEL l2 : is) | l1 == l2 = aux (LABEL l2 : is)
        aux (DUP s1 : STORE t n : POP s2 : is) | s1 == s2 = aux (STORE t n : is)
        aux (IFCMP t rel l1 : GOTO l2 : LABEL l3 : is) | l1 == l3 = aux (IFCMP t (notRel rel) l2 : LABEL l3 : is)
        aux (i : is) = i : aux is

removeUselessLabels :: [Code] -> [Code]
removeUselessLabels is = filter useful is
    where
        lset = Set.unions (map usedLabels is)

        useful :: Code -> Bool
        useful (LABEL l) = Set.member l lset
        useful _ = True

optimize :: [Code] -> [Code]
optimize = limit (removeUselessLabels . peephole)

optimizeMethod :: Method -> Method
optimizeMethod (Method x t is) = Method x t (optimize is)

optimizeMethods :: [Method] -> [Method]
optimizeMethods = map optimizeMethod
