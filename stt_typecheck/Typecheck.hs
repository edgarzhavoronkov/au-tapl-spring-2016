{-# LANGUAGE FlexibleContexts #-}

module Typecheck where

import Control.Monad.Except
import Control.Arrow
import Data.Monoid
import Data.Maybe
import Data.List
import Term

freeVars :: Expr -> [Symb]
freeVars (Var v) = [v]
freeVars (a :@ b) = freeVars a `union` freeVars b
freeVars (Lam v e) = freeVars e \\ [v]

freeTVars :: Type -> [Symb]
freeTVars (TVar t) = [t]
freeTVars (t1 :-> t2) = freeTVars t1 `union` freeTVars t2

extendEnv :: Env -> Symb -> Type -> Env
extendEnv (Env env) v t = Env $ (v, t) : env

freeTVarsEnv :: Env -> [Symb]
freeTVarsEnv (Env env) = nub $ concatMap (freeTVars . snd) env

appEnv :: (MonadError String m) => Env -> Symb -> m Type
appEnv (Env xs) v = case lookup v xs of
    Just t -> return t
    Nothing -> throwError ("There is no variable " ++ show v ++ " in the enviroment.")

appSubsTy :: SubsTy -> Type -> Type
appSubsTy (SubsTy xs) (TVar v) = fromMaybe (TVar v) (lookup v xs)
appSubsTy (SubsTy xs) (t1 :-> t2) = appSubsTy (SubsTy xs) t1 :-> appSubsTy (SubsTy xs) t2

appSubsEnv' :: SubsTy -> Env -> [(Symb,Type)]
appSubsEnv' sbs (Env env) = do
    (v, t) <- env
    return (v, appSubsTy sbs t)

appSubsEnv :: SubsTy -> Env -> Env
appSubsEnv sbs env = Env $ appSubsEnv' sbs env

composeSubsTy :: SubsTy -> SubsTy -> SubsTy
composeSubsTy sub@(SubsTy xs) (SubsTy ys) =
    let subs1 = map (second (appSubsTy sub)) ys in
        let subs2 = filter (\(x, _) -> isNothing $ lookup x ys) xs in
            SubsTy $ subs1 ++ subs2

instance Monoid SubsTy where
    mempty = SubsTy []
    mappend = composeSubsTy

unify :: (MonadError String m) => Type -> Type -> m SubsTy
unify (TVar x) (TVar y)
    | x == y = return mempty
    | otherwise = return $ SubsTy [(x, TVar y)]
unify (TVar x) t
    | x `elem` freeTVars t = throwError $ "Can't unify " ++ show (TVar x) ++ " with " ++ show t ++ "!"
    | otherwise = return $ SubsTy [(x, t)]
unify (t1 :-> t2) (TVar x) = unify (TVar x) (t1 :-> t2)
unify (t1 :-> t2) (u1 :-> u2) = do
    sub1 <- unify t2 u2
    let new_t1 = appSubsTy sub1 t1
    let new_t2 = appSubsTy sub1 u1
    sub2 <- unify new_t1 new_t2
    return $ sub2 `mappend` sub1

equations :: (MonadError String m) => Env -> Expr -> Type -> m [(Type,Type)]
equations env (Var x) t = do
    t' <- appEnv env x
    return [(t, t')]
equations env (m :@ n) t = do
    let a = getFreshVar t
    eq1 <- equations env m (a :-> t)
    eq2 <- equations env n a
    return $ eq1 `union` eq2
equations env (Lam x e) t = do
    let a = getFreshVar t
    let b = getFreshVar a
    let new_env = extendEnv env x a
    eqs <- equations new_env e b
    return $ eqs `union` [(a :-> b, t)]

getFreshVar :: Type -> Type
getFreshVar t = t'
    where
        fvs = freeTVars t
        fvs' = map (++ "\'") fvs
        fv = concat fvs' ++ "\'"
        t' = TVar fv

principlePair :: (MonadError String m) =>  Expr -> m (Env,Type)
principlePair term = do
    let env = Env $ zip (freeVars term) (map ((\a b -> TVar (a ++ b)) "t") (iterate (++"\'") "\'"))
    eqs <- equations env term (TVar "b")
    subst <- unify (foldr1 (:->) (map fst eqs)) (foldr1 (:->) (map snd eqs))
    return (appSubsEnv subst env, appSubsTy subst (TVar "b"))
