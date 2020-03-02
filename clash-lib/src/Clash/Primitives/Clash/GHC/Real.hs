module Clash.Primitives.Clash.GHC.Real where

import qualified Clash.GHC.Real as P

commontPrimitives =
  [ (blackbox 'P.divZeroError){template="~ERRORO", workInfo=Constant}
  , (blackbox 'P.ratioZeroDenominatorError){template="~ERRORO", workInfo=Constant}
  , (blackbox 'P.overflowError){template="~ERRORO", workInfo=Constant}
  ]