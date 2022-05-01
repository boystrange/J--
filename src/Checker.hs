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

module Checker (checkMethods) where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Set (Set)
import qualified Data.Set as Set
import Control.Monad.State.Lazy (StateT)
import qualified Control.Monad.State.Lazy as State
import Control.Monad (forM_, when, unless)
import Control.Exception (throw)
import Data.Maybe ( fromJust )
import Debug.Trace (traceM)

import Common
import Atoms
import Type
import Language
import Exceptions
import SymbolTable (SymbolTable, Entry)
import qualified SymbolTable

data CheckerState = CheckerState { table :: SymbolTable }

type Checker = StateT CheckerState IO

modifySymbolTable :: (SymbolTable -> SymbolTable) -> Checker ()
modifySymbolTable f = do
  state <- State.get
  State.put (state { table = f (table state) })

pushScope :: Checker ()
pushScope = modifySymbolTable SymbolTable.push

popScope :: Checker ()
popScope = modifySymbolTable SymbolTable.pop

getEntry :: Id -> Checker Entry
getEntry x = SymbolTable.get x <$> table <$> State.get

getType :: Id -> Checker Type
getType x = SymbolTable.entryType <$> getEntry x

setEntry :: Id -> Entry -> Checker ()
setEntry x entry = modifySymbolTable (SymbolTable.set x entry)

newEntry :: Id -> Type -> Checker ()
newEntry x t = modifySymbolTable (SymbolTable.new x t)

initializeEntry :: Id -> Checker ()
initializeEntry x = do
  entry <- getEntry x
  setEntry x (entry { SymbolTable.entryInit = True })

checkStmt :: Type -> Statement -> Checker Bool
checkStmt rt Empty = return False
checkStmt rt (If expr stmt1 stmt2) = do
  t <- checkExpr expr
  checkType (DataType BooleanType) t
  b1 <- checkStmt rt stmt1
  b2 <- checkStmt rt stmt2
  return $ b1 && b2
checkStmt rt (While expr stmt) = do
  t <- checkExpr expr
  checkType (DataType BooleanType) t
  _ <- checkStmt rt stmt
  return False
checkStmt rt (Do stmt expr) = do
  b <- checkStmt rt stmt
  t <- checkExpr expr
  checkType (DataType BooleanType) t
  return b
checkStmt rt (Return Nothing) = do
  checkType VoidType rt
  return True
checkStmt rt (Return (Just expr)) = do
  t <- checkExpr expr
  checkType rt t
  return True
checkStmt rt (Block stmt) = do
  pushScope
  b <- checkStmt rt stmt
  popScope
  return b
checkStmt rt (Seq stmt1 stmt2) = do
  b1 <- checkStmt rt stmt1
  -- if b1 == True then stmt2 is unreachable
  b2 <- checkStmt rt stmt2
  return $ b1 || b2
checkStmt rt (Local t x) = do
  newEntry x t
  return False
checkStmt rt (Expression expr) = do
  t <- checkExpr expr
  -- if not Void emit warning
  return False

checkType :: Type -> Type -> Checker ()
checkType et at | at `subtype` et = return () -- should emit code for conversion
checkType et at = throw $ ErrorTypeMismatch et at

checkExpr :: Expression -> Checker Type
checkExpr (Literal lit) = return $ DataType (typeOfLiteral lit)
checkExpr (Call x exprs) = do
  t <- getType x
  (rt, ts) <- unpackMethodType x (length exprs) t
  forM_ (zip exprs ts) (uncurry checkExprType)
  return rt
checkExpr (New t expr) = do
  s <- checkExpr expr
  checkType (DataType IntType) s
  return $ ArrayType t
checkExpr (Ref ref) = checkRef ref
checkExpr (Unary op expr) = do
  t <- checkExpr expr
  checkUnary op t
checkExpr (Binary op expr1 expr2) = do
  t1 <- checkExpr expr1
  t2 <- checkExpr expr2
  checkBinary op t1 t2
checkExpr (Assign ref expr) = do
  t <- checkRef ref
  checkExprType expr t
  return t
checkExpr (IncDec op ref) = do
  t <- checkRef ref
  checkIncDec op t
checkExpr (Cast t expr) = do
  checkExprType expr t
  return t

unpackMethodType :: Id -> Int -> Type -> Checker (Type, [Type])
unpackMethodType x n (MethodType rt ts) | n == length ts = return (rt, ts)
                                        | otherwise = throw $ ErrorWrongNumberOfArguments x (length ts) n
unpackMethodType x _ t = throw $ ErrorNotMethod x t

unpackArrayType :: Reference -> Type -> Checker Type
unpackArrayType ref (ArrayType t) = return t
unpackArrayType ref t = throw $ ErrorArrayExpected ref t

checkRef :: Reference -> Checker Type
checkRef (IdRef x) = getType x
checkRef (ArrayRef ref expr) = do
  t <- checkRef ref
  s <- unpackArrayType ref t
  checkExprType expr (DataType IntType)
  return s

checkUnary :: UnOp -> Type -> Checker Type
checkUnary op (DataType dt) | Just ds <- unary op dt = return (DataType ds)
checkUnary op t = throw $ ErrorUnaryOperator op t

checkBinary :: BinOp -> Type -> Type -> Checker Type
checkBinary op t1 t2 | DataType dt <- merge t1 t2
                     , Just dt <- binary op dt = return (DataType dt)
checkBinary op t1 t2 = throw $ ErrorBinaryOperator op t1 t2

checkIncDec :: IncDecOp -> Type -> Checker Type
checkIncDec op (DataType dt) | Just ds <- incdec dt = return (DataType ds)
checkIncDec op t = throw $ ErrorIncDecOperator op t

checkExprType :: Expression -> Type -> Checker ()
checkExprType expr t = do
  s <- checkExpr expr
  checkType t s

checkMethod :: Method -> Checker ()
checkMethod (Method t x args stmt) = do
  pushScope
  forM_ args (uncurry newEntry)
  ret <- checkStmt t stmt
  when (not ret && t /= VoidType) $ throw $ ErrorMissingReturn x
  popScope

checkMethods :: [Method] -> IO ()
checkMethods methods = do
  putStrLn $ show $ length methods
  State.evalStateT aux (CheckerState { table = SymbolTable.empty })
  where
    aux :: Checker ()
    aux = do
      pushScope
      forM_ methods (uncurry newEntry . typeOfMethod)
      forM_ methods checkMethod
