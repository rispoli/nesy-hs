module Monitoring where

import qualified Data.Set as S (Set, null, member, empty, union, intersection, filter, map, toList)
import GTSI2RuleR
import RuleRTypes

type Trace = [S.Set Literal]
type Steps = [(S.Set Literal, S.Set (S.Set Literal), S.Set (S.Set Literal))]

monitor :: Formula -> Formula -> Trace -> Either String (Bool, Steps)
monitor past future trace = let Right (_, _, p, i, f) = t past future
                             in monitor' trace i p f
                            where monitor' :: Trace -> I -> P -> F -> Either String (Bool, Steps)
                                  monitor' []     i _ f = Right (S.member S.empty $ S.map (S.intersection $ S.map LiRu f) i, [])
                                  monitor' (x:xs) i p f = let consistent_states = check_consistency $ resultant_states x i
                                                           in case consistent_states of
                                                                  y | S.null y -> Left "No consistent states found."
                                                                  _            -> case monitor' xs i p f of
                                                                                      Right (acceptable, states) -> Right (acceptable, (x, i, consistent_states) : states)
                                                                                      e -> e
                                  resultant_states :: S.Set Literal -> S.Set (S.Set Literal) -> S.Set (S.Set Literal)
                                  resultant_states obs = S.map (S.union obs)
                                  check_consistency :: S.Set (S.Set Literal) -> S.Set (S.Set Literal)
                                  check_consistency = S.filter (\s -> let l = S.toList s in [p | p <- l, n <- l, p == neg n] == [])
                                  neg :: Literal -> Literal
                                  neg (LiRu _)       = LiRu (Plain "")
                                  neg (LiOb (Neg o)) = LiOb o
                                  neg (LiOb o)       = LiOb (Neg o)
