module Clash.Primitives.GHC.Base where

import qualified GHC.Base as P

commonPrimitives =
  [ (primitive 'P.$){primSort="Function", workInfo=Never}
  , (blackbox 'P.quotInt){template="~ARG[0] / ~ARG[1]"}
  ]

commonVerilogPrimitives =
  [ (blackbox 'P.remInt){template="~ARG[0] % ~ARG[1]"}
  ]