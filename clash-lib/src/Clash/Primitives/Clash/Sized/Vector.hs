module Clash.Primitives.Clash.Sized.RTree where

import qualified Clash.Sized.Vector as P

commonPrimitives =
  [ (blackbox 'P.lazyV){template="~ARG[1]", workInfo=Never}
  , (blackbox 'P.seqV){template="~ARG[2]", workInfo=Never}
  , (blackbox 'P.seqVX){template="~ARG[2]", workInfo=Never}
  , (primitive 'P.dfold){primSort="Function", workInfo=Never}
  , (primitive 'P.dtfold){primSort="Function", workInfo=Never}
  , (primitive 'P.traverse#){primSort="Function", workInfo=Never}
  ]

commonVerilogPrimitives =
  [ (blackbox 'P.maxIndex){workInfo=Constant, template="~SIZE[~TYPO]'sd~LIT[0] - ~SIZE[~TYPO]'sd1"}
  , (blackbox 'P.length){workInfo=Constant, template="~SIZE[~TYPO]'sd~LIT[0]"}
  ]