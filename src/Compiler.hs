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

module Compiler where

import Control.Monad.State.Lazy (StateT)
import qualified Control.Monad.State.Lazy as State
import Control.Monad (forM_, when, unless)

import Atoms
import Type
import qualified Jasmin
import Language
import TypedLanguage

data CompilerState = CompilerState { next :: Label
                                   , code :: [Jasmin.Code]
                                   , assertions :: Bool }

type Compiler = StateT CompilerState IO

newLabel :: Compiler Label
newLabel = do
    state <- State.get
    let n = next state
    State.put (state { next = succ n })
    return n

emit :: Jasmin.Code -> Compiler ()
emit instr = do
    state <- State.get
    State.put (state { code = instr : code state })

getCode :: Compiler [Jasmin.Code]
getCode = do
    state <- State.get
    State.put (state { code = [] })
    return $ reverse $ code state

compileClass :: Bool -> [Method] -> IO [Jasmin.Method]
compileClass assertions methods = State.evalStateT (mapM compileMethod methods) init
    where
        init = CompilerState { next = L 0, code = [], assertions = assertions }

compileMethod :: Method -> Compiler Jasmin.Method
compileMethod (Method t x stmt) = do
    next <- newLabel
    compileStmt next stmt
    Jasmin.Method x t <$> getCode

compileStmt :: Label -> Statement -> Compiler ()
compileStmt next Skip = emit $ Jasmin.GOTO next
compileStmt next (If prop stmt1 stmt2) = do
    tt <- newLabel
    ff <- newLabel
    compileProp tt ff prop
    emit $ Jasmin.LABEL tt
    compileStmt next stmt1
    emit $ Jasmin.LABEL ff
    compileStmt next stmt2
compileStmt next (While prop stmt) = do
    cont <- newLabel
    tt <- newLabel
    emit $ Jasmin.LABEL cont
    compileProp tt next prop
    emit $ Jasmin.LABEL tt
    compileStmt cont stmt
compileStmt next (Do stmt prop) = do
    cont <- newLabel
    tt <- newLabel
    emit $ Jasmin.LABEL tt
    compileStmt cont stmt
    emit $ Jasmin.LABEL cont
    compileProp tt next prop
compileStmt next (Return t Nothing) = emit $ Jasmin.RETURN t
compileStmt next (Return t (Just expr)) = do
    compileExpr expr
    emit $ Jasmin.RETURN t
compileStmt next (Seq stmt1 stmt2) = do
    cont <- newLabel
    compileStmt cont stmt1
    emit $ Jasmin.LABEL cont
    compileStmt next stmt2
compileStmt next (Ignore expr) = do
    compileExpr expr
    let t = typeof expr
    unless (sizeOf t == 0) $ emit $ Jasmin.POP t
    emit $ Jasmin.GOTO next
compileStmt next (Assert (At l _) prop) = do
    assertions <- assertions <$> State.get
    when assertions $ do
        ff <- newLabel
        compileProp next ff prop
        emit $ Jasmin.LABEL ff
        emit $ Jasmin.LDC (Int l)
        emit $ Jasmin.INVOKE "StandardLibrary" "failed_assertion" VoidType [IntType]
    emit $ Jasmin.GOTO next
compileStmt next (Assert Somewhere _) = error "impossible"

compileProp :: Label -> Label -> Proposition -> Compiler ()
compileProp tt ff TrueProp = emit $ Jasmin.GOTO tt
compileProp tt ff FalseProp = emit $ Jasmin.GOTO ff
compileProp tt ff (Rel t op expr1 expr2) = do
    compileExpr expr1
    compileExpr expr2
    jumpIf t op tt ff
compileProp tt ff (And prop1 prop2) = do
    cont <- newLabel
    compileProp cont ff prop1
    emit $ Jasmin.LABEL cont
    compileProp tt ff prop2
compileProp tt ff (Or prop1 prop2) = do
    cont <- newLabel
    compileProp tt cont prop1
    emit $ Jasmin.LABEL cont
    compileProp tt ff prop2
compileProp tt ff (Not prop) = compileProp ff tt prop
compileProp tt ff (FromExpression expr) = do
    compileExpr expr
    emit $ Jasmin.CONVERT BooleanType IntType
    emit $ Jasmin.IF JEQ ff
    emit $ Jasmin.GOTO tt

compileInit :: Type -> InitExpression -> Compiler ()
compileInit t (SimpleInit expr) = compileExpr expr
compileInit (ArrayType t) (ArrayInit inits) = do
    emit $ Jasmin.LDC (Int (length inits))
    emit $ Jasmin.NEWARRAYS (ArrayType t) 1
    forM_ (zip [0..] inits) (\(i, init) -> do emit $ Jasmin.DUP (ArrayType t)
                                              emit $ Jasmin.LDC (Int i)
                                              compileInit t init
                                              emit $ Jasmin.ASTORE t)
compileInit _ (ArrayInit _) = error "impossible"

compileStep :: Type -> StepOp -> SignOp -> Reference -> Compiler ()
compileStep _ step sign (IdRef t i _) = do
    emit $ Jasmin.LOAD t i
    when (step == POST) $ emit $ Jasmin.DUP t
    emit $ Jasmin.LDC (one t)
    emit $ Jasmin.BINARY t (if sign == POS then ADD else SUB)
    when (step == PRE) $ emit $ Jasmin.DUP t
    emit $ Jasmin.STORE t i
compileStep _ step sign (ArrayRef t expr1 expr2) = do
    compileExpr expr1
    compileExpr expr2
    emit $ Jasmin.DUP2 (ArrayType t) IntType
    emit $ Jasmin.ALOAD t
    when (step == POST) $ emit $ Jasmin.DUP_X2 (ArrayType t) IntType t
    emit $ Jasmin.LDC (one t)
    emit $ Jasmin.BINARY t (if sign == POS then ADD else SUB)
    when (step == PRE) $ emit $ Jasmin.DUP_X2 (ArrayType t) IntType t
    emit $ Jasmin.ASTORE t

compileAssign :: Reference -> Expression -> Compiler ()
compileAssign (IdRef t i _) expr = do
    compileExpr expr
    emit $ Jasmin.DUP t
    emit $ Jasmin.STORE t i
compileAssign (ArrayRef t expr1 expr2) expr3 = do
    compileExpr expr1
    compileExpr expr2
    compileExpr expr3
    emit $ Jasmin.DUP_X2 (ArrayType t) IntType t
    emit $ Jasmin.ASTORE t

compileExpr :: Expression -> Compiler ()
compileExpr (Literal lit) = emit $ Jasmin.LDC lit
compileExpr (Call t cls x exprs) = do
    forM_ exprs compileExpr
    emit $ Jasmin.INVOKE cls x t (map typeof exprs)
compileExpr (New t exprs) = do
    forM_ exprs compileExpr
    let s = foldr (const ArrayType) t exprs
    emit $ Jasmin.NEWARRAYS s (length exprs)
compileExpr (Array t init) = compileInit t init
compileExpr (Length t expr) = do
    compileExpr expr
    emit $ Jasmin.ARRAYLENGTH t
compileExpr (Assign ref expr) = compileAssign ref expr
compileExpr (Ref ref) = compileRef ref
compileExpr (Unary t op expr) = do
    compileExpr expr
    unless (op == POS) $ emit $ Jasmin.UNARY t op
compileExpr (Binary t op expr1 expr2) = do
    compileExpr expr1
    compileExpr expr2
    if t == StringType && op == ADD
        then emit $ Jasmin.library "String_concat" StringType [StringType, StringType]
        else emit $ Jasmin.BINARY t op
compileExpr (Ternary _ prop expr1 expr2) = do
    tt <- newLabel
    ff <- newLabel
    next <- newLabel
    compileProp tt ff prop
    emit $ Jasmin.LABEL tt
    compileExpr expr1
    emit $ Jasmin.GOTO next
    emit $ Jasmin.LABEL ff
    compileExpr expr2
    emit $ Jasmin.LABEL next
compileExpr (Step t step sign ref) = compileStep t step sign ref
compileExpr (Convert t expr) = do
    compileExpr expr
    emit $ Jasmin.CONVERT (typeof expr) t
compileExpr (StringOf t expr) = do
    compileExpr expr
    unless (t == StringType) $ emit $ Jasmin.library (show t ++ "_to_String") StringType [t]

compileRef :: Reference -> Compiler ()
compileRef (IdRef t i _) = emit $ Jasmin.LOAD t i
compileRef (ArrayRef t expr1 expr2) = do
    compileExpr expr1
    compileExpr expr2
    emit $ Jasmin.ALOAD t

jumpIf :: Type -> RelOp -> Label -> Label -> Compiler ()
jumpIf t op tt ff | isFloating t = do
    emit $ Jasmin.CMP t
    emit $ Jasmin.IF op tt
    emit $ Jasmin.GOTO ff
jumpIf t op tt ff = do
    emit $ Jasmin.IFCMP t op tt
    emit $ Jasmin.GOTO ff
