module Main where

data Term = TmTrue
          | TmFalse
          | TmIf Term Term Term
          | TmZero
          | TmSucc Term
          | TmPred Term
          | TmIsZero Term
          deriving Show

main = do
    useEval1


isNumericVal :: Term -> Bool
isNumericVal t1 = case t1 of
    TmZero    -> True
    TmSucc t1 -> isNumericVal t1
    _         -> False


isVal :: Term -> Bool
isVal t = case t of
    TmTrue  -> True
    TmFalse -> True
    t       -> isNumericVal t


-- 単一ステップの評価
eval1 :: Term -> Term
eval1 t = case t of
    TmIf TmTrue  t2 t3                       -> t2
    TmIf TmFalse _  t3                       -> t3
    TmIf t1      t2 t3                       -> TmIf (eval1 t1) t2 t3
    TmSucc t1                                -> TmSucc (eval1 t1)
    TmPred TmZero                            -> TmZero
    TmPred (TmSucc nv1) | isNumericVal nv1   -> nv1
    TmPred   t1                              -> TmPred (eval1 t1)
    TmIsZero TmZero                          -> TmTrue
    TmIsZero (TmSucc nv1) | isNumericVal nv1 -> TmFalse
    TmIsZero t1                              -> TmIsZero (eval1 t1)
    _                                        -> error "no rules applies"


-- examples
useIsNumeriVal = do
    print $ isNumericVal (TmIsZero (TmSucc TmZero))


useIsVal = do
    print $ isVal (TmIsZero (TmSucc TmZero))


useEval1 = do
    print $ eval1 (TmIsZero (TmSucc TmZero))