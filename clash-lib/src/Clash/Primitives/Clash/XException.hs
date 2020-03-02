module Clash.Primitives.Clash.XException where

import qualified Clash.XEXception as P

commontPrimitives =
  [ (blackbox 'P.seqX){template="~ARG[1]", workInfo=Never}
  , (blackbox 'P.errorX){template="~ERRORO", workInfo=Constant}
  , (blackbox 'P.deepseqX){template="~ARG[2]", workInfo=Never}
  , (blackbox 'P.hwSeqX){template="~DEVNULL[~VAR[x][0]]~ARG[1]", workInfo=Never}
  ]
