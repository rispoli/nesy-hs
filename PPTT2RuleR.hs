module PPTT2RuleR where

import qualified Data.Set as S (Set, empty, singleton, union, fromList)
import RuleRTypes
import Utils

type RuleSystem = (R, O, P, I, S, Q)

tp :: Formula -> Either String RuleSystem
tp FTrue              = Right (S.empty, S.empty, S.empty, S.singleton S.empty, S.empty, S.singleton S.empty)
tp FFalse             = Right (S.empty, S.empty, S.empty, S.empty, S.empty, S.empty)
tp op@(Obsr p)        = Right (S.empty, S.singleton p, S.empty, S.singleton S.empty, S.empty, ss (LiOb op))
tp noq@(Neg (Obsr q)) = Right (S.empty, S.singleton q, S.empty, S.singleton S.empty, S.empty, ss (LiOb noq))
tp (Neg _)            = Left "Formula not in NNF."
tp (phi `Conj` psi)   = let Right (r_phi, o_phi, p_phi, i_phi, s_phi, q_phi) = tp phi
                            Right (r_psi, o_psi, p_psi, i_psi, s_psi, q_psi) = tp psi
                         in Right (r_phi `S.union` r_psi, o_phi `S.union` o_psi, p_phi `S.union` p_psi, i_phi `cp` i_psi, s_phi `S.union` s_psi, q_phi `cp` q_psi)
tp (phi `Disj` psi)   = let Right (r_phi, o_phi, p_phi, i_phi, s_phi, q_phi) = tp phi
                            Right (r_psi, o_psi, p_psi, i_psi, s_psi, q_psi) = tp psi
                         in Right (r_phi `S.union` r_psi, o_phi `S.union` o_psi, p_phi `S.union` p_psi, i_phi `S.union` i_psi, s_phi `S.union` s_psi, q_phi `S.union` q_psi)
tp f@(phi `S` psi)    = let (r, o, p, i, s, q) = sz S phi psi
                         in Right (r, o, p, i, s `S.union` srpsp, q)
                        where srpsp = S.singleton . LiRu $ Complex (Neg f)
tp f@(phi `Z` psi)    = let (r, o, p, i, s, q) = sz Z phi psi
                         in Right (r, o, p, i, s `S.union` srpzp, q)
                        where srpzp = S.singleton . LiRu $ Complex f
tp _                  = Left "Only strict and weak 'since' operators (S and Z) can be used in a present and past-time temporal query formula."

sz :: (Formula -> Formula -> Formula) -> Formula -> Formula -> RuleSystem
sz cs phi psi = let Right (r_phi, o_phi, p_phi, _, s_phi, q_phi) = tp phi
                    Right (r_psi, o_psi, p_psi, _, s_psi, q_psi) = tp psi
                    srnqm_phi                                    = srnqm phi q_phi
                    srnqm_psi                                    = srnqm psi q_psi
                    ssliqm_phi_psi_g                             = rn2suliru [srnqm_phi, srnqm_psi, S.singleton qmg]
                 in (foldl1 S.union [r_phi, r_psi, S.fromList [rpcp, qmg], srnqm_phi, srnqm_psi],
                     o_phi `S.union` o_psi,
                     foldl1 S.union [p_phi, p_psi, S.singleton $ Rule rpcp S.empty S.empty, S.singleton $ Rule qmg S.empty ssliqm_phi_psi_g, sr psi q_psi S.empty, sr phi q_phi (S.singleton $ LiRu rpcp)],
                     ssliqm_phi_psi_g,
                     s_phi `S.union` s_psi,
                     ss . LiRu $ rpcp)
                where rpcp                   = Complex pcp
                      pcp                    = phi `cs` psi
                      qmg                    = QM (Plain "g") pcp Nothing
                      srnqm :: Formula -> Q -> S.Set RuleName
                      srnqm p q              = q2rnr q $ \(Right x) _ -> QM (Complex p) pcp (Just x)
                      sr :: Formula -> Q -> S.Set Literal -> S.Set Rule
                      sr p q init_head       = q2rnr q $ \(Right x) x' -> Rule (QM (Complex p) pcp (Just x)) (x' `S.union` init_head) (ss (LiRu rpcp))
