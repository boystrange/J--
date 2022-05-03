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

import Control.Monad.State.Lazy (StateT)
import qualified Control.Monad.State.Lazy as State
import Control.Monad (forM_, when, unless)
import Control.Exception (throw)
import Debug.Trace (traceM)

import Common
import Atoms
import Type
import Language
import SourceLanguage
import qualified TypedLanguage as Typed
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

getSlot :: Id -> Checker Slot
getSlot x = SymbolTable.entrySlot <$> getEntry x

setEntry :: Id -> Entry -> Checker ()
setEntry x entry = modifySymbolTable (SymbolTable.set x entry)

newEntry :: Id -> Type -> Checker ()
newEntry x t = modifySymbolTable (SymbolTable.new x t)

initializeEntry :: Id -> Checker ()
initializeEntry x = do
  entry <- getEntry x
  setEntry x (entry { SymbolTable.entryInit = True })

checkStmt :: Type -> Statement -> Checker (Bool, Typed.Statement)
checkStmt rt Skip = return (False, Typed.Skip)
checkStmt rt (If expr stmt1 stmt2) = do
  prop <- checkProp expr
  (b1, stmt1') <- checkStmt rt stmt1
  (b2, stmt2') <- checkStmt rt stmt2
  return (b1 && b2, Typed.If prop stmt1' stmt2')
checkStmt rt (While expr stmt) = do
  prop <- checkProp expr
  (_, stmt') <- checkStmt rt stmt
  return (False, Typed.While prop stmt')
checkStmt rt (Do stmt expr) = do
  (b, stmt') <- checkStmt rt stmt
  prop <- checkProp expr
  return (b, Typed.Do stmt' prop)
checkStmt rt (Return Nothing) = do
  checkType VoidType rt
  return (True, Typed.Return VoidType Nothing)
checkStmt rt (Return (Just expr)) = do
  (t, expr') <- checkExpr expr
  cast <- checkType rt t
  return (True, Typed.Return t (Just (cast expr')))
checkStmt rt (Block stmt) = do
  pushScope
  (b, stmt') <- checkStmt rt stmt
  popScope
  return (b, stmt')
checkStmt rt (Seq stmt1 stmt2) = do
  (b1, stmt1') <- checkStmt rt stmt1
  if b1
    then return (True, stmt1')
    else do
      (b2, stmt2') <- checkStmt rt stmt2
      return (b2, Typed.Seq stmt1' stmt2')
checkStmt rt (Local t x) = do
  newEntry x t
  return (False, Typed.Skip)
checkStmt rt (Ignore expr) = do
  (t, expr') <- checkExpr expr
  return (False, Typed.Expression expr')

conversion :: Type -> Type -> Typed.Expression -> Typed.Expression
conversion s t | s == t = id
conversion (DataType s) (DataType t) = Typed.Conversion s t

checkType :: Type -> Type -> Checker (Typed.Expression -> Typed.Expression)
checkType et at | at `subtype` et = return (conversion at et)
checkType et at = throw $ ErrorTypeMismatch et at

checkExpr :: Expression -> Checker (Type, Typed.Expression)
checkExpr (Literal lit) = return (DataType (typeOfLiteral lit), Typed.Literal lit)
checkExpr (Call x exprs) = do
  t <- getType x
  (rt, ts) <- unpackMethodType x (length exprs) t
  exprs' <- mapM (uncurry checkExprType) (zip exprs ts)
  return (rt, Typed.Call (MethodType rt ts) x exprs')
checkExpr (New t expr) = do
  (s, expr') <- checkExpr expr
  cast <- checkType (DataType IntType) s
  return (ArrayType t, Typed.New t (cast expr'))
checkExpr (Ref ref) = do
  (t, ref') <- checkRef ref
  return (t, Typed.Ref ref')
checkExpr (Unary op expr) = do
  (t, expr') <- checkExpr expr
  s <- checkUnary op t
  return (s, Typed.Unary s op expr')
checkExpr (Binary op expr1 expr2) = do
  (t1, expr1') <- checkExpr expr1
  (t2, expr2') <- checkExpr expr2
  t <- checkBinary op t1 t2
  return (t, Typed.Binary t op expr1' expr2')
checkExpr (Assign ref expr) = do
  (t, ref') <- checkRef ref
  expr' <- checkExprType expr t
  return (t, Typed.Assign ref' expr')
checkExpr (IncDec op ref) = do
  (t, ref') <- checkRef ref
  s <- checkIncDec op t
  return (s, Typed.IncDec s op ref')
checkExpr (Cast t expr) = do
  expr' <- checkExprType expr t -- cast != subtyping
  return (t, expr')
checkExpr expr = do
  prop <- checkProp expr
  return (DataType BooleanType, Typed.FromProposition prop)

checkProp :: Expression -> Checker Typed.Proposition
checkProp (Literal (Boolean True)) = return Typed.TrueProp
checkProp (Literal (Boolean False)) = return Typed.FalseProp
checkProp (Rel op expr1 expr2) = do
  (t1, expr1') <- checkExpr expr1
  (t2, expr2') <- checkExpr expr2
  t <- checkRel op t1 t2
  return $ Typed.Rel t op expr1' expr2'
checkProp (And prop1 prop2) = do
  prop1' <- checkProp prop1
  prop2' <- checkProp prop2
  return $ Typed.And prop1' prop2'
checkProp (Or prop1 prop2) = do
  prop1' <- checkProp prop1
  prop2' <- checkProp prop2
  return $ Typed.Or prop1' prop2'
checkProp (Not prop) = Typed.Not <$> checkProp prop
checkProp expr = Typed.FromExpression <$> checkExprType expr (DataType BooleanType)

unpackMethodType :: Id -> Int -> Type -> Checker (Type, [Type])
unpackMethodType x n (MethodType rt ts) | n == length ts = return (rt, ts)
                                        | otherwise = throw $ ErrorWrongNumberOfArguments x (length ts) n
unpackMethodType x _ t = throw $ ErrorNotMethod x t

unpackArrayType :: Reference -> Type -> Checker Type
unpackArrayType ref (ArrayType t) = return t
unpackArrayType ref t = throw $ ErrorArrayExpected ref t

checkRef :: Reference -> Checker (Type, Typed.Reference)
checkRef (IdRef x) = do
  t <- getType x
  n <- getSlot x
  return (t, Typed.IdRef t n x)
checkRef (ArrayRef ref expr) = do
  (t, ref') <- checkRef ref
  s <- unpackArrayType ref t
  expr' <- checkExprType expr (DataType IntType)
  return (s, Typed.ArrayRef s ref' expr')

checkUnary :: UnOp -> Type -> Checker Type
checkUnary op (DataType dt) | Just ds <- unary op dt = return (DataType ds)
checkUnary op t = throw $ ErrorUnaryOperator op t

checkBinary :: BinOp -> Type -> Type -> Checker Type
checkBinary op t1 t2 | DataType dt <- merge t1 t2
                     , Just ds <- binary op dt = return (DataType ds)
checkBinary op t1 t2 = throw $ ErrorBinaryOperator op t1 t2

checkRel :: RelOp -> Type -> Type -> Checker Type
checkRel op t s | DataType dt <- merge t s = return (DataType dt)
checkRel op t s = throw $ ErrorBinaryRelation op t s

checkIncDec :: IncDecOp -> Type -> Checker Type
checkIncDec op (DataType dt) | Just ds <- incdec dt = return (DataType ds)
checkIncDec op t = throw $ ErrorIncDecOperator op t

checkExprType :: Expression -> Type -> Checker Typed.Expression
checkExprType expr t = do
  (s, expr') <- checkExpr expr
  conv <- checkType t s
  return (conv expr')

checkMethod :: Method -> Checker Typed.Method
checkMethod method@(Method t x args stmt) = do
  pushScope
  forM_ args (uncurry newEntry)
  (ret, stmt') <- checkStmt t stmt
  when (not ret && t /= VoidType) $ throw $ ErrorMissingReturn x
  let stmt'' = if ret then stmt' else Typed.Seq stmt' (Typed.Return VoidType Nothing)
  popScope
  return $ Typed.Method (snd (typeOfMethod method)) x stmt''

checkMethods :: [Method] -> IO [Typed.Method]
checkMethods methods = do
  putStrLn $ show $ length methods
  State.evalStateT aux (CheckerState { table = SymbolTable.empty })
  where
    aux :: Checker [Typed.Method]
    aux = do
      pushScope
      forM_ methods (uncurry newEntry . typeOfMethod)
      mapM checkMethod methods
