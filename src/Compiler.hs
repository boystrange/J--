module Compiler where

import Control.Monad.State.Lazy (StateT)
import qualified Control.Monad.State.Lazy as State

import qualified Jasmin
import Language
import TypedLanguage

data Label = L Int
    deriving (Eq, Enum)

instance Show Label where
    show (L n) = "L" ++ show n

data CompilerState = CompilerState { next :: Label
                                   , code :: [String] }

type Compiler = StateT CompilerState IO

newLabel :: Compiler Label
newLabel = do
    state <- State.get
    let n = next state
    State.put (state { next = succ n })
    return n

emit :: String -> Compiler ()
emit i = do
    state <- State.get
    State.put (state { code = ("    " ++ i) : code state })

emitLabel :: Label -> Compiler ()
emitLabel l = do
    state <- State.get
    State.put (state { code = ("  " ++ show l ++ ":") : code state })

compileRef :: Reference -> Compiler ()
compileRef (IdRef t i _) = emit $ Jasmin.load t i
compileRef (ArrayRef t r expr) = do
    compileRef r
    compilexExpr expr
    emit $ Jasmin.aload t

compileProp :: Label -> Label -> Proposition -> Compiler ()
compileProp tt ff TrueProp = emit $ Jasmin.goto tt
compileProp tt ff FalseProp = emit $ Jasmin.goto ff
compileProp tt ff (Rel t op expr1 expr2) = do
    compileExpr expr1
    compileExpr expr2
    emit $ Jasmin.ifcmp t op tt
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
    emit $ Jasmin.ifeq ff
    emit $ Jasmin.goto tt

compileExpr :: Expression -> Compiler ()
compileExpr (Literal lit) = emit $ Jasmin.ldc lit
compileExpr (Call x t exprs) = do
    forM_ exprs compileExpr
    emit $ Jasmin.invokestatic x t
compileExpr (New t expr) = do
    compleExpr expr
    emit $ "newarray " ++ jasmin t
compileExpr (Assign (IdRef t i _) expr) = do
    compileExpr expr
    emit $ dup t
    emit $ Jasmin.store t i
compileExpr (Assign (ArrayRef t r expr1) expr2) = do
    compileRef r
    compileExpr expr1
    compileExpr expr2
    emit $ dup_x2 t
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
    emit $ conversion s t
compileExpr (FromProposition prop) = do
    true <- newLabel
    false <- newLabel
    next <- newLabel
    compileProp true false prop
    emitLabel true
    emit $ Jasmin.ldc (Literal (Boolean True))
    emit $ Jasmin.goto next
    emitLabel false
    emit $ Jasmin.ldc (Literal (Boolean False))
    emitLabel next