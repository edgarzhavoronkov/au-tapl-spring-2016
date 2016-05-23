{-# LANGUAGE TemplateHaskell #-}
module Lens3 where

import Language.Haskell.TH

data EitherOrOr a b c
    = Lft { _lft :: a }
    | Mid { _mid :: b }
    | Rgh { _rgh :: c }
    deriving (Eq, Show)
