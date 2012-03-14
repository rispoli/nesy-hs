module Utils where

import qualified Data.Set as S (Set, singleton, union, map, toList, fromList)
import Control.Monad (liftM2)
import RuleRTypes

ss :: a -> S.Set (S.Set a)
ss = S.singleton . S.singleton

cp :: (Eq a, Ord a) => S.Set (S.Set a) -> S.Set (S.Set a) -> S.Set (S.Set a)
cp a b = S.fromList . map S.fromList $ [x ++ y | x <- a', y <- b']
         where a' = map S.toList . S.toList $ a
               b' = map S.toList . S.toList $ b

l2f :: Literal -> Either String Formula
l2f (LiRu (Complex f)) = Right f
l2f (LiRu e)           = Left $ "This: '" ++ show e ++ "' Literal cannot be converted to a Formula."
l2f (LiOb f)           = Right f

q2rnr :: (Ord a) => Q -> (Either String Formula -> S.Set Literal -> a) -> S.Set a
q2rnr q lc = S.fromList [lc x x' | x' <- S.toList q, let x = foldl1 (liftM2 Conj) (map l2f . S.toList $ x')]

rn2suliru :: [S.Set RuleName] -> S.Set (S.Set Literal)
rn2suliru = S.singleton . foldl1 S.union . map (S.map LiRu)
