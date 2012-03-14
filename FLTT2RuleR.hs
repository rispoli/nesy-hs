module FLTT2RuleR where

import qualified Data.Set as S (empty, singleton, union, fromList)
import RuleRTypes
import Utils

type RuleSystem = (R, O, P, I, F)

tf :: Formula -> Either String RuleSystem
tf FTrue              = Right (S.singleton $ Plain "g", S.empty, S.singleton $ Rule (Plain "g") S.empty (ss (LiRu (Plain "g"))), ss (LiRu (Plain "g")), S.empty)
tf FFalse             = Right (S.empty, S.empty, S.empty, S.empty, S.empty)
tf op@(Obsr p)        = Right (S.singleton $ Plain "g", S.singleton p, S.singleton $ Rule (Plain "g") S.empty (ss (LiRu (Plain "g"))), S.singleton $ S.fromList [LiOb op, LiRu (Plain "g")], S.empty)
tf noq@(Neg (Obsr q)) = Right (S.singleton $ Plain "g", S.singleton q, S.singleton $ Rule (Plain "g") S.empty (ss (LiRu (Plain "g"))), S.singleton $ S.fromList [LiOb noq, LiRu (Plain "g")], S.empty)
tf (Neg _)            = Left "Formula not in NNF."
tf (phi `Conj` psi)   = let Right (r_phi, o_phi, p_phi, i_phi, f_phi) = tf phi
                            Right (r_psi, o_psi, p_psi, i_psi, f_psi) = tf psi
                         in Right (r_phi `S.union` r_psi, o_phi `S.union` o_psi, p_phi `S.union` p_psi, i_phi `cp` i_psi, f_phi `S.union` f_psi)
tf (phi `Disj` psi)   = let Right (r_phi, o_phi, p_phi, i_phi, f_phi) = tf phi
                            Right (r_psi, o_psi, p_psi, i_psi, f_psi) = tf psi
                         in Right (r_phi `S.union` r_psi, o_phi `S.union` o_psi, p_phi `S.union` p_psi, i_phi `S.union` i_psi, f_phi `S.union` f_psi)
tf f'@(phi `U` psi)    = let (r, o, p, i, f) = uw U phi psi
                         in Right (r, o, p, i, f `S.union` srpup)
                        where srpup = S.singleton . Complex $ f'
tf (phi `W` psi)      = Right $ uw W phi psi
tf _                  = Left "Only strict 'until' and 'unless' operators (U and W) can be used in a future linear-time temporal formula."

uw :: (Formula -> Formula -> Formula) -> Formula -> Formula -> RuleSystem
uw cs phi psi = let Right (r_phi, o_phi, p_phi, i_phi, f_phi) = tf phi
                    Right (r_psi, o_psi, p_psi, i_psi, f_psi) = tf psi
                 in (foldl1 S.union [r_phi, r_psi, S.singleton rpcp],
                     o_phi `S.union` o_psi,
                     foldl1 S.union [p_phi, p_psi, S.singleton $ Rule rpcp S.empty (S.union i_psi $ cp i_phi (S.singleton srpcp))],
                     S.singleton srpcp,
                     f_phi `S.union` f_psi)
                where srpcp = S.singleton $ LiRu rpcp
                      rpcp  = Complex $ phi `cs` psi

-- Tests:
-- tf ((Obsr "a") `U` (Obsr "b"))
-- tf ((Obsr "a") `Conj` ((Obsr "c") `W` (Obsr "d")))
-- tf (((Obsr "a") `U` (Obsr "b")) `U` ((Obsr "a") `Conj` ((Obsr "c") `W` (Obsr "d"))))
