module Compiler where

import Control.Monad.State.Lazy (StateT)
import qualified Control.Monad.State.Lazy as State
import Control.Monad (forM_)

import Atoms
import Type
import qualified Jasmin
import Language
import TypedLanguage

data CompilerState = CompilerState { next :: Label
                                   , code :: [String] }

type Compiler = StateT CompilerState IO

newLabel :: Compiler Label
newLabel = do
    state <- State.get
    let n = next state
    State.put (state { next = succ n })
    return n

append :: String -> Compiler ()
append s = do
    state <- State.get
    State.put (state { code = s : code state })

emit :: String -> Compiler ()
emit i = append ("    " ++ i)

emitLabel :: Label -> Compiler ()
emitLabel l = append ("  " ++ show l ++ ":")

emitMethod :: Type -> Id -> Compiler ()
emitMethod t x = append (".method public " ++ show x ++ Jasmin.jasmin t)

emitEndMethod :: Compiler ()
emitEndMethod = append ".end method"

compileRef :: Reference -> Compiler ()
compileRef (IdRef t i _) = emit $ Jasmin.load t i
compileRef (ArrayRef t r expr) = do
    compileRef r
    compileExpr expr
    emit $ Jasmin.aload t

jumpIf :: Type -> RelOp -> Label -> Compiler ()
jumpIf (BaseType FloatType) op tt = do
    emit "fcmpl"
    emit $ Jasmin.zif op tt
jumpIf (BaseType DoubleType) op tt = do
    emit "dcmpl"
    emit $ Jasmin.zif op tt
jumpIf t op tt = emit $ Jasmin.ifcmp t op tt

compileProp :: Label -> Label -> Proposition -> Compiler ()
compileProp tt ff TrueProp = emit $ Jasmin.goto tt
compileProp tt ff FalseProp = emit $ Jasmin.goto ff
compileProp tt ff (Rel t op expr1 expr2) = do
    compileExpr expr1
    compileExpr expr2
    jumpIf t op tt
    emit $ Jasmin.goto ff
compileProp tt ff (And prop1 prop2) = do
    true <- newLabel
    compileProp true ff prop1
    emitLabel true
    compileProp tt ff prop2
compileProp tt ff (Or prop1 prop2) = do
    false <- newLabel
    compileProp tt false prop1
    emitLabel false
    compileProp tt ff prop2
compileProp tt ff (Not prop) = compileProp ff tt prop
compileProp tt ff (FromExpression expr) = do
    compileExpr expr
    emit $ Jasmin.zif JEQ ff
    emit $ Jasmin.goto tt

compileExpr :: Expression -> Compiler ()
compileExpr (Literal lit) = emit $ Jasmin.ldc lit
compileExpr (Call x t exprs) = do
    forM_ exprs compileExpr
    emit $ Jasmin.invokestatic t x
compileExpr (New t expr) = do
    compileExpr expr
    undefined
compileExpr (Assign (IdRef t i _) expr) = do
    compileExpr expr
    emit $ Jasmin.dup t
    emit $ Jasmin.store t i
compileExpr (Assign (ArrayRef t r expr1) expr2) = do
    compileRef r
    compileExpr expr1
    compileExpr expr2
    emit $ Jasmin.dup_x2 t
    emit $ Jasmin.astore t
compileExpr (Ref r) = compileRef r
compileExpr (Unary t op expr) = do
    compileExpr expr
    emit $ Jasmin.unary t op
compileExpr (Binary t op expr1 expr2) = do
    compileExpr expr1
    compileExpr expr2
    emit $ Jasmin.binary t op
compileExpr (IncDec t op r) = undefined
compileExpr (Conversion s t expr) = do
    compileExpr expr
    emit $ Jasmin.conversion s t
compileExpr (FromProposition prop) = do
    true <- newLabel
    false <- newLabel
    next <- newLabel
    compileProp true false prop
    emitLabel true
    emit $ Jasmin.ldc (Boolean True)
    emit $ Jasmin.goto next
    emitLabel false
    emit $ Jasmin.ldc (Boolean False)
    emitLabel next

compileStmt :: Label -> Statement -> Compiler ()
compileStmt next Skip = emit $ Jasmin.goto next
compileStmt next (If prop stmt1 stmt2) = do
    true <- newLabel
    false <- newLabel
    compileProp true false prop
    emitLabel true
    compileStmt next stmt1
    emitLabel false
    compileStmt next stmt2
compileStmt next (While prop stmt) = do
    cont <- newLabel
    true <- newLabel
    emitLabel cont
    compileProp true next prop
    emitLabel true
    compileStmt cont stmt
compileStmt next (Do stmt prop) = do
    cont <- newLabel
    true <- newLabel
    emitLabel true
    compileStmt cont stmt
    emitLabel cont
    compileProp true next prop
compileStmt next (Return t Nothing) = emit $ Jasmin.ret t
compileStmt next (Return t (Just expr)) = do
    compileExpr expr
    emit $ Jasmin.ret t
compileStmt next (Seq stmt1 stmt2) = do
    cont <- newLabel
    compileStmt cont stmt1
    emitLabel cont
    compileStmt next stmt2

compileMethod :: Method -> Compiler ()
compileMethod (Method t x stmt) = do
    emitMethod t x
    next <- newLabel
    compileStmt next stmt
    emitEndMethod

compileMethods :: [Method] -> IO [String]
compileMethods methods = reverse <$> code <$> State.execStateT aux (CompilerState { next = L 0, code = [] })
    where
        aux = forM_ methods compileMethod
