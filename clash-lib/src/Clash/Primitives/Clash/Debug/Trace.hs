module Clash.Primitives.Debug.Trace where

import qualified Debug.Trace as P

commonPrimitives =
  [ (blackbox P.trace){template="~ARG[1]"}
  ]
