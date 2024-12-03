module Contextuality where

import Data.Bool
import Data.List
import Control.Monad
import Control.Monad.State.Lazy
import System.Random

-- cabal install --lib mtl
-- cabal install --lib random
--

type Name = String

data Expr where
    Var :: Name -> Expr
    Const :: Bool -> Expr
    NotE :: Expr -> Expr
    OrE :: Expr -> Expr -> Expr
    AndE :: Expr -> Expr -> Expr
    PairE :: Expr -> Expr -> Expr
    Fst :: Expr -> Expr
    Snd :: Expr -> Expr
    LetE :: Name -> Expr -> Expr -> Expr
    deriving (Show)

data Value where
    Error :: Value
    BoolV :: Bool -> Value
    PairV :: Value -> Value -> Value
    deriving (Show)

type Outcome = Bool

type Env = [(Name, Value)]


type M = State Bool

-- examples of interpreters

lookupM :: Name -> Env -> M Value
lookupM s [] = return Error
lookupM s ((x,v):rest) = if (x == s) then (return v) else (lookupM s rest)

notM :: Value -> M Value
notM (BoolV b) = return (BoolV (not b))
notM _ = return Error

orM, andM :: Value -> Value -> M Value
orM (BoolV b1) (BoolV b2) = return (BoolV (or [b1, b2]))
orM _ _ = return Error

andM (BoolV b1) (BoolV b2) = return (BoolV (and [b1, b2]))
andM _ _ = return Error

interpM :: Env -> Expr -> M Value
interpM env (Var s) = lookupM s env
interpM env (Const b) = return (BoolV b)
interpM env (NotE e) = do
    v <- interpM env e
    notM v
interpM env (OrE e1 e2) = do
    v1 <- interpM env e1
    v2 <- interpM env e2
    orM v1 v2
interpM env (AndE e1 e2) = do
    v1 <- interpM env e1
    v2 <- interpM env e2
    andM v1 v2
interpM env (PairE e1 e2) = do
    v1 <- interpM env e1
    v2 <- interpM env e2
    return (PairV v1 v2)
interpM env (Fst e) = do
    v <- interpM env e
    case v of
        PairV v1 v2 -> return v1
        _ -> return Error
interpM env (Snd e) = do
    v <- interpM env e
    case v of
        PairV v1 v2 -> return v2
        _ -> return Error
interpM env (LetE x e1 e2) = do
    v1 <- interpM env e1
    interpM ((x,v1):env) e2


pairVReducer :: (Value -> Value -> M Value) -> Value -> M Value
pairVReducer binop (BoolV b) = return (BoolV b)
pairVReducer binop (PairV v1 v2) = do
    v1' <- pairVReducer binop v1
    v2' <- pairVReducer binop v2
    binop v1' v2'
pairVReducer binop Error = return Error



-- examples of expressions

e1, e2, e3 :: Expr
e1 = NotE (Var "s")
e2 = LetE "x" (AndE (NotE (Const True)) (Const False)) (NotE (Var "x"))
e3 = PairE (PairE (Const True) e2) (Const True)

runInterpM :: Expr -> Value
runInterpM expr = evalState (interpM [] expr) True


-- examples of test cases

pureContingent :: Expr -> M Outcome
pureContingent expr = get

contingentOnInterp :: Expr -> M Outcome
contingentOnInterp expr = do
    v <- interpM [] expr
    case v of
        BoolV b -> return b
        _ -> get

contingentOnReducerOr :: Expr -> M Outcome
contingentOnReducerOr expr = do
    v <- interpM [] expr
    v <- pairVReducer orM v
    case v of
        BoolV b -> return b
        _ -> get

contingentOnReducerAnd :: Expr -> M Outcome
contingentOnReducerAnd expr = do
    v <- interpM [] expr
    v <- pairVReducer andM v
    case v of
        BoolV b -> return b
        _ -> get

causeTrue :: Expr -> M Outcome
causeTrue expr = do
    put True
    contingentOnInterp expr

causeFalse :: Expr -> M Outcome
causeFalse expr = do
    put False
    contingentOnInterp expr

hasError :: Expr -> M Outcome
hasError expr = do
    v <- interpM [] expr
    case v of
        Error -> return True
        _ -> return False

isBoolV :: Expr -> M Outcome
isBoolV expr = do
    v <- interpM [] expr
    case v of
        BoolV b -> return True
        _ -> return False 

isNotPair :: Expr -> M Outcome
isNotPair expr = do
    v <- interpM [] expr
    case v of
        PairV _ _ -> return False
        _ -> return True


-- experiment apparatus

randomBool :: Int -> (Bool, StdGen)
randomBool seed = random (mkStdGen seed)

randomBoolStream :: Int -> [Bool]
randomBoolStream seed = randoms (mkStdGen seed)

-- expression as quantum system
type Qsystem = Expr
-- test as single observable
type Observable = Qsystem -> M Outcome
-- test suite as measurement context
type Context = [Observable]
-- joint outcomes given a context
type Experiment = Qsystem -> Context -> M [Outcome]


exp1 :: Qsystem -> Observable -> M Outcome
exp1 expr f = f expr

exp2 :: Qsystem -> (Observable, Observable) -> M (Outcome, Outcome)
exp2 expr (f1, f2) = do
    o1 <- f1 expr
    o2 <- f2 expr
    return (o1, o2)

exp3 :: Qsystem 
    -> (Observable, Observable, Observable) 
    -> M (Outcome, Outcome, Outcome)
exp3 expr (f1, f2, f3) = do
    o1 <- f1 expr
    o2 <- f2 expr
    o3 <- f3 expr
    return (o1, o2, o3)

exp4 :: Qsystem
    -> (Observable, Observable, Observable, Observable)
    -> M (Outcome, Outcome, Outcome, Outcome)
exp4 expr (f1, f2, f3, f4) = do
    o1 <- f1 expr
    o2 <- f2 expr
    o3 <- f3 expr
    o4 <- f4 expr
    return (o1, o2, o3, o4)

expn :: Qsystem -> Context -> M [Outcome]
expn expr [] = return []
expn expr (f:fs) = do
    o <- f expr
    os <- expn expr fs
    return (o:os)

--runExperimentM :: Qsystem -> Context -> Experiment -> [Outcome]
--runExperimentM expr fs expn = evalState (expn expr fs) (fst (randomBool 123))

type Seed = Int
-- joint outcomes given a context
runExperiment :: Qsystem -> Context -> Seed -> [Outcome]
runExperiment expr ctx seed =
    let m = expn expr ctx in
        evalState m (fst (randomBool seed)) 

-- Qsystem independent contextuality...
ctx1 = [causeTrue, pureContingent]
ctx2 = [causeFalse, pureContingent]
ctx3 = [causeTrue, hasError]
ctx4 = [causeFalse, hasError]
suite1 = [ctx1,ctx2,ctx3,ctx4]

-- one by one
suite2 = [[pureContingent], [causeFalse], [hasError], [causeTrue]]


printResult :: Qsystem -> [Context] -> Seed -> IO ()
printResult expr [] seed = pure ()
printResult expr (c:cs) seed = do
    print (runExperiment expr c seed)
    printResult expr cs (seed + 1)


main :: IO ()
main = do
    printResult e1 suite1 111