module Clash.Primitives.Annotations.BitRepresentation.Deriving where

import qualified Clash.Annotations.BitRepresentation.Deriving as P

commonPrimitives =
  [ (blackbox 'P.dontApplyInHDL){workInfo=Never, template="~ARG[1]"}
  ]