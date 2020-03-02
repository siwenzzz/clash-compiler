module Clash.Primitives.Unsafe.Coerce where

import qualified Unsafe.Coerce as P

commontPrimitives =
  [ (blackbox 'P.unsafeCoerce){template="~ARG[0]"}
  ]
