module Clash.Primitives.GHC.TypeNats where

import qualified GHC.TypeNats as P

commontPrimitives =
  [ (blackbox 'P.natVal){template="~ARG[0]", workInfo=Never}
  ]