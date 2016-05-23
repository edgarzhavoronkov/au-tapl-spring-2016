{-# LANGUAGE
    GADTs
    , KindSignatures
    , FlexibleInstances
    , EmptyDataDecls
    , FlexibleContexts
    , RankNTypes #-}
module SystemF where

import Prelude hiding(succ)
import Control.Applicative
import Control.Monad.Except

type Err a = Either String a

ok :: a -> Err a
ok = Right

err :: String -> Err a
err = Left

isOk :: Err a -> Bool
isOk Right{} = False
isOk Left{} = True

get :: Err a -> a
get (Right r) = r
get (Left e) = error e

-- data Primitive :: * -> * where
--     Num :: Integer -> Primitive Integer
--     Succ :: Primitive (Integer -> Integer)

data Primitive
    = Num Integer
    | Succ
    deriving (Show, Eq)

data Type
    = NumTy
    | FunTy Type Type
    | VTy (Type -> Type)
    | TyVar Char
--
-- data Type :: * -> * where
--     NumTy :: Type Integer
--     FunTy :: Type a -> Type b -> Type (a -> b)
--     VTy :: (Type a -> Type b) -> Type (V a b)
--     TyVar :: Char -> Type a

-- data Term :: * -> * where
--     Prim :: Primitive a -> Term a
--     Abs :: Type a -> (Term a -> Term b) -> Term (a -> b)
--     App :: Term (a -> b) -> Term a -> Term b
--     TAbs :: (Type a -> Term b) -> Term (V a b)
--     TApp :: Term (V a b) -> Type a -> Term b
--     Unknown :: Char -> Term a

data Term
    = Prim Primitive
    | Abs Type (Term -> Term)
    | App Term Term
    | TAbs (Type -> Term)
    | TApp Term Type
    | Unknown Char
--
-- data V a b

instance Eq Type where
    (==) = helper (['A'..'Z'] ++ ['a'..'z']) where
        helper :: String -> Type -> Type -> Bool
        helper _ NumTy NumTy = True
        helper cs (FunTy dom rng) (FunTy dom' rng') = helper cs dom dom' && helper cs rng rng'
        helper (c:cs) (VTy f) (VTy f') = helper cs (f (TyVar c)) (f' (TyVar c))
        helper [] _ _ = error "Not enough characters"
        helper _ (TyVar c) (TyVar c') = c == c'
        helper _ _ _ = False

instance Show Type where
    show = helper ("XYZ" ++ ['a'..'z']) where
        helper :: String -> Type -> String
        helper _ NumTy = "Num"
        helper cs (FunTy dom rng) = "(" ++ helper cs dom ++ " -> " ++ helper cs rng ++ ")"
        helper (c:cs) (VTy f) = "(forall " ++ [c] ++ ". " ++ helper cs (f (TyVar c)) ++ ")"
        helper [] VTy{} = error "Too many nested type applications"
        helper _ (TyVar t) = [t]



instance Show Term where
    show t = let
                var = get $ eval t
                ty = get $ typeOf t
            in show var ++ " : " ++ show ty

instance Eq Term where
    (==) = undefined

instance Num Term where
    fromInteger = num
    (+) = undefined
    (-) = undefined
    (*) = undefined
    abs = undefined
    signum = undefined
    negate = undefined

eval' :: (MonadError String m, Applicative m) => Term -> m Term
eval' (Prim p) = return $ Prim p
eval' (Abs t f) = return $ Abs t f
eval' (TAbs f) = return $ TAbs f
eval' (App f x) = do
    f' <- eval' f
    res <- runApp f' <*> eval' x
    eval' res
eval' (TApp f x) = do
    f' <- eval' f
    res <- runTApp f' <*> pure x
    eval' res

eval :: (MonadError String m, Applicative m) => Term -> m Primitive
eval t = eval' t >>= valueOf

valueOf :: (MonadError String m) => Term -> m Primitive
valueOf (Prim (Num x)) = return $ Num x
valueOf _ = throwError "Not a primitive type"

runApp :: (MonadError String m) => Term -> m (Term -> Term)
runApp (Abs _ f) = return f
runApp (Prim p) = helper p where
    helper :: (MonadError String m) => Primitive -> m (Term -> Term)
    helper Succ = return $ \(Prim (Num n)) -> num (n + 1)
runApp _ = throwError "unexpected non-abstraction used in application"

runTApp :: (MonadError String m) => Term -> m (Type -> Term)
runTApp (TAbs f) = return f
runTApp _ = throwError "runTApp failed unexpectedly"

typeOf :: (MonadError String m, Applicative m) => Term -> m Type
typeOf (Prim p) = case p of
    Num{} -> return NumTy
    Succ -> return $ FunTy NumTy NumTy
typeOf (Abs t f) = FunTy t <$> typeOf (f (genTy t))
typeOf (TAbs f) = return $ VTy (get . typeOf . f)
typeOf (App f x) = do
    FunTy dom rng <- typeOf f
    t <- typeOf x
    if t == dom
        then return rng
        else throwError "function domain does not match application input"
typeOf (TApp f x) = do
    VTy f' <- typeOf f
    return $ f' x
typeOf (Unknown c) = return $ TyVar c

genTy :: Type -> Term
genTy NumTy = num 0
genTy (FunTy dom rng) = l dom (\_ -> genTy rng)
genTy (VTy f) = TAbs (genTy . f)
genTy (TyVar c) = Unknown c


num = Prim . Num

succ = Prim Succ


v = TAbs

l = Abs

app = App

tapp = TApp

--simple tests

succ' = l NumTy (\x -> app succ x)

id' = v (\t -> l t (\x -> x))

const' = v (\t1 -> v (\t2 -> l t1 (\x -> l t2 (\_ -> x))))
--       \X:* -> \f: X -> X
double = v (\t -> l (FunTy t t) (\f -> l t (\x -> app f (app f x))))

quadruple = v (\t -> app (tapp double (FunTy t t)) (tapp double t))

doubleNat = tapp quadruple NumTy

-- self :: Term (V x (x -> x) -> V x (x -> x))
self = l (VTy $ \t -> FunTy t t) (\x -> app (tapp x (VTy $ \t -> FunTy t t)) x)

bot = VTy id
botTy = FunTy bot (FunTy bot bot)

tripleSelf = l bot (\x -> (tapp x botTy) `app` x `app` x)

-- tripleSelf' = l bot (v (\t -> l (FunTy t (FunTy t t)))
