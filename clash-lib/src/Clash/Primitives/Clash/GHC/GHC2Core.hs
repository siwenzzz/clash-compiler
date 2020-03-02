module Clash.Primitives.Clash.GHC.GHC2Core where

import qualified Clash.GHC.GHC2Core as P

commonPrimitives =
  [ (blackbox' "EmptyCase"){workInfo=Constant, template="~ERROR0"}
  , (primitive' "_CO_"){workInfo=Constant, primSort="Constructor"}
  , (primitive' "_TY_"){workInfo=Constant, primSort="Constructor"}
  ]
