{-# LANGUAGE ExistentialQuantification
    , RankNTypes
    , MultiParamTypeClasses
    , FlexibleInstances
    , FunctionalDependencies #-}
module Lens2 where

type Lens s t a b = forall f. Functor f => (a -> f b) -> s -> f t

type Lens' s a = Lens s s a a

newtype Const c a = Const {getConst :: c}

instance Functor (Const c) where
    fmap _ (Const v) = Const v

newtype Identity a = Identity {runIdentity :: a}

instance Functor Identity where
    fmap f (Identity x) = Identity (f x)

lens :: (s -> a) -> (s -> b -> t) -> Lens s t a b
lens getter setter ret s = fmap (setter s) (ret $ getter s)

-- _1 :: Lens (a, b) (c, b) a c
-- _1 = lens fst (\(_, y) v -> (v, y))
--
-- _2 :: Lens (a, b) (a, d) b d
-- _2 = lens snd (\(x, _) v -> (x, v))

view :: Lens s t a b -> s -> a
view lns s = getConst (lns Const s)

over :: Lens s t a b -> (a -> b) -> s -> t
over lns fn s = runIdentity $ lns (Identity . fn) s

set :: Lens s t a b -> b -> s -> t
set lns a s = runIdentity $ lns (Identity . const a) s

class Field1 s t a b | s -> a, s b -> t where
    _1 :: Lens s t a b

class Field2 s t a b | s -> a, s b -> t where
    _2 :: Lens s t a b

class Field3 s t a b | s -> a, s b -> t where
    _3 :: Lens s t a b

instance Field1 (a, b) (c, b) a c where
    _1 = lens fst (\(_, y) v -> (v, y))
--
instance Field2 (a, b) (a, d) b d where
    _2 = lens snd (\(x, _) v -> (x, v))

instance Field1 (a, b, c) (d, b, c) a d where
    _1 = lens (\(x, _, _) -> x) (\(_, y, z) v -> (v, y, z))

instance Field2 (a, b, c) (a, e, c) b e where
    _2 = lens (\(_, y, _) -> y) (\(x, _, z) v -> (x, v, z))

instance Field3 (a, b, c) (a, b, f) c f where
    _3 = lens (\(_, _, z) -> z) (\(x, y, _) v -> (x, y, v))
