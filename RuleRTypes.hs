module RuleRTypes where

import qualified Data.Set as S (Set)

type ObsrName = String

data Formula = FTrue
             | FFalse
             | Obsr ObsrName
             | Neg  Formula
             | Conj Formula Formula
             | Disj Formula Formula
             | Impl Formula Formula
             | U    Formula Formula
             | W    Formula Formula
             | S    Formula Formula
             | Z    Formula Formula
               deriving (Eq, Ord, Show)

data RuleName = Plain    String
              | Complex  Formula
              | NComplex String Formula
              | QM       RuleName Formula (Maybe Formula)
                deriving (Eq, Ord, Show)

data Literal = LiRu RuleName | LiOb Formula deriving (Eq, Ord, Show)

data Rule = Rule RuleName (S.Set Literal) (S.Set (S.Set Literal)) deriving (Eq, Ord, Show)

type R = S.Set RuleName
type O = S.Set ObsrName
type P = S.Set Rule
type I = S.Set (S.Set Literal)
type F = S.Set RuleName
type S = S.Set Literal
type Q = S.Set (S.Set Literal)
