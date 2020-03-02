module Clash.Primitives.GHC.Types where

import qualified GHC.Types as P

commontPrimitives =
  [ (primitive 'P.MkCoercible){workInfo=Never, primSort="Constructor"}
  , (blackbox 'P.C#){template="~ARG[0]", workInfo=Never}
  , (blackbox 'P.I#){template="~ARG[0]", workInfo=Never}
  , (blackbox 'P.W#){template="~ARG[0]", workInfo=Never}
  ]