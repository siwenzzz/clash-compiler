module Clash.Primitives.Clash.Transformations where

import qualified Clash.Transformations as P

commonPrimitives =
  [ (blackbox 'P.removedArg){template="~ERROR0", workInfo=Constant}
  , (blackbox 'P.undefined){template="~ERROR0", workInfo=Constant}
  ]

commonVerilogPrimitives =
  [ (blackbox 'P.eqInt){template="~ARG[0] == ~ARG[1]"}
  ]
