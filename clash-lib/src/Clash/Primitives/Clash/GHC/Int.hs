module Clash.Primitives.GHC.Int where

import qualified GHC.Int as P

commonVerilogPrimitives =
  [ (blackboxHaskellTF 'P.I8# 'Clash.Primitives.GHC.Int.intTF){workInfo=Never}
  , (blackboxHaskellTF 'P.I16# 'Clash.Primitives.GHC.Int.intTF){workInfo=Never}
  , (blackboxHaskellTF 'P.I32# 'Clash.Primitives.GHC.Int.intTF){workInfo=Never}
  , (blackboxHaskellTF 'P.I64# 'Clash.Primitives.GHC.Int.intTF){workInfo=Never}
  ]
