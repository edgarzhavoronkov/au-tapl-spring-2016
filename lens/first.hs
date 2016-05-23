{-# LANGUAGE ExistentialQuantification
    , RankNTypes
    , MultiParamTypeClasses
    , FlexibleInstances
    , FunctionalDependencies #-}
module Lens1 where

type Lens s a = forall f. Functor f => (a -> f a) -> s -> f s

newtype Const c a = Const {getConst :: c}

instance Functor (Const c) where
    fmap _ (Const v) = Const v

newtype Identity a = Identity {runIdentity :: a}

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

lens :: (s -> a) -> (s -> a -> s) -> Lens s a
lens getter setter ret s = fmap (setter s) (ret $ getter s)

view :: Lens s a -> s -> a
view lns s = getConst (lns Const s)


over :: Lens s a -> (a -> a) -> s -> s
over lns fn s = runIdentity $ lns (Identity . fn) s

set :: Lens s a -> a -> s -> s
set lns a s = runIdentity $ lns (Identity . const a) s

class Field1 s a | s -> a where
    _1 :: Lens s a

class Field2 s a | s -> a where
    _2 :: Lens s a

class Field3 s a | s -> a where
    _3 :: Lens s a

instance Field1 (a, b) a where
    _1 = lens fst (\(_, y) v -> (v, y))

instance Field2 (a, b) b where
    _2 = lens snd (\(x, _) v -> (x, v))

instance Field1 (a, b, c) a where
    _1 = lens (\(x, _, _) -> x) (\(_, y, z) v -> (v, y, z))

instance Field2 (a, b, c) b where
    _2 = lens (\(_, y, _) -> y) (\(x, _, z) v -> (x, v, z))

instance Field3 (a, b, c) c where
    _3 = lens (\(_, _, z) -> z) (\(x, y, _) v -> (x, y, v))
