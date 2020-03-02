module Clash.Primitives.Clash.Class.BitPack where

import qualified Clash.Class.BitPack as P

commonPrimitives =
  [ (blackbox 'P.packFloat#){workInfo=Never, template="~ARG[0]"}
  , (blackbox 'P.unpackFloat#){workInfo=Never, template="~ARG[0]"}
  , (blackbox 'P.packDouble#){workInfo=Never, template="~ARG[0]"}
  , (blackbox 'P.unpackDouble#){workInfo=Never, template="~ARG[0]"}
  ]
