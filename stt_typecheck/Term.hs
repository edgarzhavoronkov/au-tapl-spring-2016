module Term where

infixl 2 :@
infixr 3 :->

type Symb = String

-- Терм
data Expr =
    Var Symb
    | Expr :@ Expr
    | Lam Symb Expr
    deriving (Eq,Show,Read)

-- Тип
data Type =
    TVar Symb
    | Type :-> Type
    deriving (Eq,Show)

-- Контекст
newtype Env = Env [(Symb,Type)]
    deriving (Eq,Show)

-- Подстановка
newtype SubsTy = SubsTy [(Symb, Type)]
    deriving (Eq,Show)
