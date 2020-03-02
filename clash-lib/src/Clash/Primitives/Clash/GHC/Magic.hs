module Clash.Primitives.GHC.Magic where

import qualified GHC.Magic as P

commonPrimitives =
  [ (primitive 'P.lazy){primSort="Function", workInfo=Never}
  , (primitive 'P.noinline){primSort="Function", workInfo=Never}
  , (primitive 'P.runRW#){primSort="Function", workInfo=Never}
  ]
