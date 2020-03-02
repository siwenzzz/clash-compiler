module Clash.Primitives.Clash.Sized.RTree where

import qualified Clash.Sized.RTree as P

commontPrimitives =
  [ (primitive 'P.tdfold){primSort="Function", workInfo=Never}
  ]
