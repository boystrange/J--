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

checkStmt :: Type -> Statement -> Checker Typed.Statement
checkStmt rt Skip = return Typed.Skip
checkStmt rt (If expr stmt1 stmt2) = do
  prop <- checkProp expr
  stmt1' <- checkStmt rt stmt1
  stmt2' <- checkStmt rt stmt2
  return $ Typed.If prop stmt1' stmt2'
checkStmt rt (While expr stmt) = do
  prop <- checkProp expr
  stmt' <- checkStmt rt stmt
  return $ Typed.While prop stmt'
checkStmt rt (Do stmt expr) = do
  stmt' <- checkStmt rt stmt
  prop <- checkProp expr
  return $ Typed.Do stmt' prop
checkStmt rt (Return Nothing) = do
  unless (rt == VoidType) $ throw $ ErrorTypeMismatch rt VoidType
  return $ Typed.Return VoidType Nothing
checkStmt rt (Return (Just expr)) = do
  expr' <- checkExpr expr
  return $ Typed.Return rt (Just (widen rt expr'))
checkStmt rt (Block stmt) = do
  pushScope
  stmt' <- checkStmt rt stmt
  popScope
  return stmt'
checkStmt rt (Seq stmt1 stmt2) = do
  stmt1' <- checkStmt rt stmt1
  if returns stmt1
    then return stmt1'
    else (Typed.Seq stmt1' <$> checkStmt rt stmt2)
checkStmt rt (Local t x) = do
  newEntry x t
  return Typed.Skip
checkStmt rt (Ignore expr) = do
  expr' <- checkExpr expr
  return $ Typed.Ignore expr'

checkExpr :: Expression -> Checker Typed.Expression
checkExpr (Literal lit) = return $ Typed.Literal lit
checkExpr (Call x exprs) = do
  (rt, ts) <- getMethodType x
  unless (length ts == length exprs) $ throw $ ErrorWrongNumberOfArguments x (length ts) (length exprs)
  exprs' <- mapM checkExpr exprs
  return $ Typed.Call rt x (map (uncurry widen) (zip ts exprs'))
checkExpr (New t expr) = do
  expr' <- checkExpr expr
  return $ Typed.New t (widen IntType expr')
checkExpr (Ref ref) = Typed.Ref <$> checkRef ref
checkExpr (Unary op expr) = do
  expr' <- checkExpr expr
  let t = typeof expr'
  unless (isNumeric t) $ throw $ ErrorNumberExpected t
  return $ Typed.Unary t op expr'
checkExpr (Binary op expr1 expr2) = do
  expr1' <- checkExpr expr1
  expr2' <- checkExpr expr2
  let t1 = typeof expr1'
  let t2 = typeof expr2'
  case merge t1 t2 of
    Just t -> return $ Typed.Binary t op (widen t expr1') (widen t expr2')
    Nothing | stringable op t1 t2 -> return $ Typed.Binary StringType op (string expr1') (string expr2')
    Nothing -> throw $ ErrorBinaryOperator op t1 t2
checkExpr (Assign ref expr) = do
  ref' <- checkRef ref
  expr' <- checkExpr expr
  return $ Typed.Assign ref' (widen (typeof ref') expr')
checkExpr (Step step sign ref) = do
  ref' <- checkRef ref
  let t = typeof ref'
  unless (isNumeric t) $ throw $ ErrorNumberExpected t
  return $ Typed.Step t step sign ref'
checkExpr (Cast t expr) = do
  expr' <- checkExpr expr
  return $ cast t expr'
checkExpr expr = Typed.FromProposition <$> checkProp expr

stringable :: BinOp -> Type -> Type -> Bool
stringable ADD t s = isString t || isString s
stringable _   _ _ = False

string :: Typed.Expression -> Typed.Expression
string expr = if isString t then expr else Typed.StringOf t expr
  where
    t = typeof expr

widen :: Type -> Typed.Expression -> Typed.Expression
widen t expr | widening s t = if t == s then expr else Typed.Convert t expr
             | otherwise    = throw $ ErrorInvalidWidening s t
  where
    s = typeof expr

cast :: Type -> Typed.Expression -> Typed.Expression
cast t expr = if t == s then expr else Typed.Convert t expr
  where
    s = typeof expr

checkProp :: Expression -> Checker Typed.Proposition
checkProp (Literal (Boolean True)) = return Typed.TrueProp
checkProp (Literal (Boolean False)) = return Typed.FalseProp
checkProp (Rel op expr1 expr2) = do
  expr1' <- checkExpr expr1
  expr2' <- checkExpr expr2
  let t1 = typeof expr1'
  let t2 = typeof expr2'
  case merge t1 t2 of
    Just t -> return $ Typed.Rel t op (widen t expr1') (widen t expr2')
    Nothing -> throw $ ErrorBinaryRelation op t1 t2
checkProp (And prop1 prop2) = do
  prop1' <- checkProp prop1
  prop2' <- checkProp prop2
  return $ Typed.And prop1' prop2'
checkProp (Or prop1 prop2) = do
  prop1' <- checkProp prop1
  prop2' <- checkProp prop2
  return $ Typed.Or prop1' prop2'
checkProp (Not prop) = Typed.Not <$> checkProp prop
checkProp expr = Typed.FromExpression <$> widen BooleanType <$> checkExpr expr

getMethodType :: Id -> Checker (Type, [Type])
getMethodType x = do
  t <- getType x
  case t of
    MethodType rt ts -> return (rt, ts)
    t -> throw $ ErrorMethodExpected x t

getArrayType :: Typed.Reference -> Checker Type
getArrayType ref =
  case typeof ref of
    ArrayType t -> return t
    t           -> throw $ ErrorArrayExpected t

checkRef :: Reference -> Checker Typed.Reference
checkRef (IdRef x) = do
  t <- getType x
  n <- getSlot x
  return $ Typed.IdRef t n x
checkRef (ArrayRef ref expr) = do
  ref' <- checkRef ref
  expr' <- checkExpr expr
  t <- getArrayType ref'
  return $ Typed.ArrayRef t ref' (widen IntType expr')

checkMethod :: Method -> Checker Typed.Method
checkMethod method@(Method t x args stmt) = do
  pushScope
  forM_ args (uncurry newEntry)
  stmt' <- checkStmt t stmt
  let ret = returns stmt
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
