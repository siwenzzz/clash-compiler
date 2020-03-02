module Clash.Primitives.Clash.GHC.Word where

import qualified Clash.GHC.Word as P

commonVerilogPrimitives =
  [ (blackboxHaskellTF 'P.W8# 'Clash.Primitives.GHC.Word.wordTF){workInfo=Never}
  , (blackboxHaskellTF 'P.W16# 'Clash.Primitives.GHC.Word.wordTF){workInfo=Never}
  , (blackboxHaskellTF 'P.W32# 'Clash.Primitives.GHC.Word.wordTF){workInfo=Never}
  , (blackboxHaskellTF 'P.W64# 'Clash.Primitives.GHC.Word.wordTF){workInfo=Never}
  ]