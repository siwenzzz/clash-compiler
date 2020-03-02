module Clash.Primitives.Clash.Promoted.Nat.Unsafe where

import qualified Clash.Promoted.Nat.Unsafe as P

commontPrimitives =
  [ (blackbox 'P.unsafeSNat){workInfo=Never, template="~LIT[0]"}
  ]