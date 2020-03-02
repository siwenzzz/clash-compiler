module Clash.Primitives.Clash.Explicit.Signal where

import qualified Clash.Explicit.Signal as P

commonPrimitives =
  [ (blackbox 'P.veryUnsafeSychronizer){workInfo=Never, template="~ARG[2]"}
  ]
