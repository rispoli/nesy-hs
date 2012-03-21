module GTSI2RuleR where

import qualified Data.Set as S (empty, singleton, union, difference, map)
import FLTT2RuleR (tf)
import PPTT2RuleR (tp)
import RuleRTypes
import Utils

type RuleSystem = (R, O, P, I, F)

t :: Formula -> Formula -> Either String RuleSystem
t phi psi = let Right (r_past, o_past, p_past, i_past, s_past, q_past)   = tp phi
                Right (r_future, o_future, p_future, i_future, f_future) = tf psi
                srnxip                                                   = q2rnr q_past $ \(Right x) _ -> Complex $ x `Impl` psi
                slii_g_phi_psi_x                                         = foldl1 S.union . map (S.map LiRu) $ [S.singleton rgpip, srnxip]
             in Right (foldl1 S.union [r_past, r_future, srgpip, srnxip],
                       o_past `S.union` o_future,
                       foldl1 S.union [p_past, p_future, S.singleton $ Rule rgpip S.empty (S.singleton slii_g_phi_psi_x), q2rnr q_past $ \(Right x) x' -> Rule (Complex (x `Impl` psi)) x' i_future],
                       S.singleton (slii_g_phi_psi_x `S.union` s_past) `cp` S.difference i_past (S.singleton (S.map LiRu r_past)),
                       f_future)
            where rgpip  = NComplex "g" $ phi `Impl` psi
                  srgpip = S.singleton rgpip

-- Test:
-- t ((Obsr "c") `Conj` ((Obsr "b") `S` (Obsr "a"))) (((Obsr "p") `Disj` (FTrue `U` (Obsr "p"))) `Conj` ((Obsr "q") `Disj` (FTrue `U` (Obsr "q"))))
