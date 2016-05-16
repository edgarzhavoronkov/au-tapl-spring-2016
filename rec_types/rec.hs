{-# LANGUAGE StandaloneDeriving, FlexibleContexts, UndecidableInstances #-}

module Hw where

newtype Mu f = In (f (Mu f))

deriving instance Show (f (Mu f)) => Show (Mu f)
deriving instance Eq (f (Mu f)) => Eq (Mu f)

data N x = Z | S x

instance Functor N where
    fmap _ Z = Z
    fmap g (S x) = S (g x)

type Nat = Mu N

cata :: Functor f => (f a -> a) -> Mu f -> a
cata phi (In x) = phi $ fmap (cata phi) x

phiN :: N Int -> Int
phiN Z = 0
phiN (S n) = succ n

natToInt :: Nat -> Int
natToInt = cata phiN

ana :: Functor f => (a -> f a) -> a -> Mu f
ana psi x = In $ fmap (ana psi) (psi x)

psiN :: Int -> N Int
psiN 0 = Z
psiN n = S (n-1)

intToNat :: Int -> Nat
intToNat = ana psiN

data B x = Empty | Zero x | One x deriving Show

instance Functor B where
    fmap _ Empty = Empty
    fmap f (Zero b) = Zero (f b)
    fmap f (One b) = One (f b)

type Bin = Mu B

bin2int :: Bin -> Int
bin2int = cata phiB

int2bin :: Int -> Bin
int2bin = ana psiB

phiB :: B Int -> Int
phiB Empty = 0
phiB (Zero x) = 2 * x
phiB (One x) = 2 * x + 1

psiB :: Int -> B Int
psiB 0 = Empty
psiB x
    | x `mod` 2 == 0 = Zero (x `div` 2)
    | otherwise = One ((x - 1) `div` 2)

--expressions computer

data E x = Num Int | Add x x | Mult x x deriving Show

instance Functor E where
    fmap _ (Num x) = Num x
    fmap f (Add x y) = Add (f x) (f y)
    fmap f (Mult x y) = Mult (f x) (f y)

type Expr = Mu E

--tests
en = In . Num
e3 = en 3
ep35 = In (Add e3 (en 5))
emp357 = In (Mult ep35 (en 7))
em7p35 = In (Mult (en 7) ep35)

eval :: Expr -> Int
eval = cata phiE

phiE :: E Int -> Int
phiE (Num n) = n
phiE (Add a b) = a + b
phiE (Mult a b) = a * b

phiEShow :: E String -> String
phiEShow (Num n) = show n
phiEShow (Add a b) = "(" ++ a ++ "+" ++ b ++ ")"
phiEShow (Mult a b) = "(" ++ a ++ "*" ++ b ++ ")"

phiEShowS :: E ShowS -> ShowS
phiEShowS (Num n) = (++ show n ++ " ")
phiEShowS (Add a b) = (++ (b . a) "+ ")
phiEShowS (Mult a b) = (++ (b . a) "* ")

--stack computer

type Stack = [Int]

push :: Int -> Stack -> Stack
push a as = a : as

add :: Stack -> Stack
add (a : b : cs) = (b + a) : cs

mult :: Stack -> Stack
mult (a : b : cs) = (b * a) : cs

eval' :: Expr -> Stack -> Stack
eval' = cata phiE'

phiE' :: E (Stack -> Stack) -> Stack -> Stack
phiE' (Num x) = push x
phiE' (Add x y) = add . x . y
phiE' (Mult x y) = mult . x . y
