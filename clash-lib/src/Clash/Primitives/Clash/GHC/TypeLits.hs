module Clash.Primitives.GHC.TypeLits where

import qualified GHC.TypeLits as P

commontPrimitives =
  [ (blackbox 'P.natVal){template="~ARG[0]", workInfo=Never}
  ]