module Optimizer where

import Common (limit)
import Atoms
import Type
import Language
import Jasmin

import Data.Set (Set)
import qualified Data.Set as Set

peephole :: [Code] -> [Code]
peephole = aux
    where
        aux [] = []
        aux (GOTO l1 : LABEL l2 : is) | l1 == l2 = aux (LABEL l2 : is)
        aux (DUP s1 : STORE t n : POP s2 : is) | s1 == s2 = aux (STORE t n : is)
        aux (IFCMP t rel l1 : GOTO l2 : LABEL l3 : is) | l1 == l3 = aux (IFCMP t (notRel rel) l2 : LABEL l3 : is)
        aux (LDC (Int 0) : IFCMP IntType rel l1 : is) = aux (IF rel l1 : is)
        aux (i : is) = i : aux is

removeUselessLabels :: [Code] -> [Code]
removeUselessLabels is = filter useful is
    where
        lset = Set.fromList (concat $ map labels is)

        useful :: Code -> Bool
        useful (LABEL l) = Set.member l lset
        useful _ = True

optimize :: [Code] -> [Code]
optimize = limit (removeUselessLabels . peephole)

optimizeMethod :: Method -> Method
optimizeMethod (Method x t is) = Method x t (optimize is)

optimizeMethods :: [Method] -> [Method]
optimizeMethods = map optimizeMethod
