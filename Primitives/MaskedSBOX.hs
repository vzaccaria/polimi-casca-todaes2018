{-# LANGUAGE DataKinds #-}


module Primitives.MaskedSBOX where

import Prelude
import Language.Operators
-- #####
import Primitives.Math
import Primitives.Math22
import Primitives.MaskedGFInv
-- #####


------------------
-- Masked S-Box --
------------------

msbox :: (Symantics repr) => (repr 8, repr 4) -> repr 8 -> repr 8
msbox msk = aff_trans . (m_invGF msk)
