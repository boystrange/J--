module Compiler where

import Control.Monad.State.Lazy (StateT)
import qualified Control.Monad.State.Lazy as State
import Control.Monad (forM_, unless)

import Atoms
import Type
import qualified Jasmin
import Language
import TypedLanguage

data CompilerState = CompilerState { next :: Label
                                   , code :: [Jasmin.Code] }

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

compileRef :: Reference -> Compiler ()
compileRef (IdRef t i _) = emit $ Jasmin.LOAD t i
compileRef (ArrayRef t r expr) = do
    compileRef r
    compileExpr expr
    emit $ Jasmin.ALOAD t

jumpIf :: Type -> RelOp -> Label -> Label -> Compiler ()
jumpIf t op tt ff | isFloating t = do
    emit $ Jasmin.CMP t
    emit $ Jasmin.IF op tt
    emit $ Jasmin.GOTO ff
jumpIf t op tt ff = do
    emit $ Jasmin.IFCMP t op tt
    emit $ Jasmin.GOTO ff

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
    emit $ Jasmin.IF JEQ ff
    emit $ Jasmin.GOTO tt

compileExpr :: Expression -> Compiler ()
compileExpr (Literal lit) = emit $ Jasmin.LDC lit
compileExpr (Call t x exprs) = do
    forM_ exprs compileExpr
    emit $ Jasmin.INVOKE x (MethodType t (map typeof exprs))
compileExpr (New t expr) = do
    compileExpr expr
    undefined
compileExpr (Assign (IdRef t i _) expr) = do
    compileExpr expr
    emit $ Jasmin.DUP t
    emit $ Jasmin.STORE t i
compileExpr (Assign (ArrayRef t r expr1) expr2) = do
    compileRef r
    compileExpr expr1
    compileExpr expr2
    emit $ Jasmin.DUP_X2 t
    emit $ Jasmin.ASTORE t
compileExpr (Ref r) = compileRef r
compileExpr (Unary t op expr) = do
    compileExpr expr
    emit $ Jasmin.UNARY t op
compileExpr (Binary t op expr1 expr2) = do
    compileExpr expr1
    compileExpr expr2
    emit $ Jasmin.BINARY t op
compileExpr (Step t step sign ref) = compileStep t step sign ref
compileExpr (Convert t expr) = do
    compileExpr expr
    emit $ Jasmin.CONVERT (typeof expr) t
compileExpr (FromProposition prop) = do
    tt <- newLabel
    ff <- newLabel
    next <- newLabel
    compileProp tt ff prop
    emit $ Jasmin.LABEL tt
    emit $ Jasmin.LDC (Boolean True)
    emit $ Jasmin.GOTO next
    emit $ Jasmin.LABEL ff
    emit $ Jasmin.LDC (Boolean False)
    emit $ Jasmin.LABEL next

compileStep :: Type -> StepOp -> SignOp -> Reference -> Compiler ()
compileStep _ POST sign (ArrayRef t ref expr) = undefined
compileStep _ POST sign (IdRef t i _) = do
    emit $ Jasmin.LOAD t i
    emit $ Jasmin.DUP t
    emit $ Jasmin.LDC (one t)
    emit $ Jasmin.BINARY t (if sign == POS then ADD else SUB)
    emit $ Jasmin.STORE t i
compileStep _ PRE sign (IdRef t i _) = do
    emit $ Jasmin.LOAD t i
    emit $ Jasmin.LDC (one t)
    emit $ Jasmin.BINARY t (if sign == POS then ADD else SUB)
    emit $ Jasmin.DUP t
    emit $ Jasmin.STORE t i

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

compileMethod :: Method -> Compiler Jasmin.Method
compileMethod (Method t x stmt) = do
    next <- newLabel
    compileStmt next stmt
    Jasmin.Method x t <$> getCode

compileMethods :: [Method] -> IO [Jasmin.Method]
compileMethods methods = State.evalStateT aux (CompilerState { next = L 0, code = [] })
    where
        aux = mapM compileMethod methods
