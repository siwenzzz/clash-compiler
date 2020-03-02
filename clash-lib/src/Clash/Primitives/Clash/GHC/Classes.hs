module Clash.Primitives.GHC.Classes where

import qualified GHC.Classes as P

commonPrimitives =
  [ (blackbox 'P.gtInt){template="~ARG[0] > ~ARG[1]"}
  , (blackbox 'P.geInt){template="~ARG[0] >= ~ARG[1]"}
  , (blackbox 'P.ltInt){template="~ARG[0] < ~ARG[1]"}
  , (blackbox 'P.leInt){template="~ARG[0] <= ~ARG[1]"}
  ]

commonVerilogPrimitives =
  [ (blackbox 'P.eqInt){template="~ARG[0] == ~ARG[1]"}
  , (blackbox 'P.neInt){template="~ARG[0] != ~ARG[1]"}
  , (blackbox 'P.&&){template="~ARG[0] & ~ARG[1]"}
  , (blackbox 'P.||){template="~ARG[0] | ~ARG[1]"}
  , (blackbox 'P.not){template="~ ~ARG[0]"}
  ]