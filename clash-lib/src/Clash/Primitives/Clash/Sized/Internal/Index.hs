module Clash.Primitives.Clash.Sized.Internal.Index where

import qualified Clash.Sized.Internal.Index as P

commonPrimitives =
  [ (blackbox 'P.quot#){template="~ARG[0] / ~ARG[1]"}
  ]

commonVerilogPrimitives =
  [ (blackbox 'P.pack#){workInfo=Never, template="~ARG[0]"}
  , (blackbox 'P.unpack#){workInfo=Never, template="~ARG[2]"}
  , (blackbox 'P.eq#){template="~IF~SIZE[~TYP[0]]~THEN~ARG[0] == ~ARG[1]~ELSE1'b1~FI"}
  , (blackbox 'P.neq#){template="~IF~SIZE[~TYP[0]]~THEN~ARG[0] != ~ARG[1]~ELSE1'b1~FI"}
  , (blackbox 'P.lt#){template="~IF~SIZE[~TYP[0]]~THEN~ARG[0] < ~ARG[1]~ELSE1'b1~FI"}
  , (blackbox 'P.ge#){template="~IF~SIZE[~TYP[0]]~THEN~ARG[0] >= ~ARG[1]~ELSE1'b1~FI"}
  , (blackbox 'P.gt#){template="~IF~SIZE[~TYP[0]]~THEN~ARG[0] > ~ARG[1]~ELSE1'b1~FI"}
  , (blackbox 'P.le#){template="~IF~SIZE[~TYP[0]]~THEN~ARG[0] <= ~ARG[1]~ELSE1'b1~FI"}
  , (blackbox 'P.maxBound#){workInfo=Constant, template="~ARG[0]-~SIZE[~TYPO]'d1"}
  , (blackbox 'P.+#){template="~ARG[1] + ~ARG[2]"}
  , (blackbox 'P.-#){template="~ARG[1] - ~ARG[2]"}
  , (blackbox 'P.*#){template="~ARG[1] * ~ARG[2]"}
  , (blackbox 'P.fromInteger#){workInfo=Never, template="~IF~CMPLE[~SIZE[~TYPO]][~SIZE[~TYP[1]]]~THEN$unsigned(~VAR[i][1][0+:~SIZE[~TYPO]])~ELSE$unsigned({{(~SIZE[~TYPO]-~SIZE[~TYP[1]]) {1'b0}},~VAR[i][1]})~FI"}
  , (blackbox 'P.plus#){kind=Declaration, template="assign ~RESULT = ~ARG[0] + ~ARG[1];"}
  , (blackbox 'P.minus#){kind=Declaration, template="assign ~RESULT = ~ARG[0] - ~ARG[1];"}
  , (blackbox 'P.times#){kind=Declaration, template="assign ~RESULT = ~ARG[0] * ~ARG[1];"}
  , (blackbox 'P.rem#){kind=Declaration, template="assign ~RESULT = ~ARG[0] % ~ARG[1];"}
  , (blackboxHaskellTF 'P.toInteger# 'P.indexToIntegerVerilog){workInfo=Never}
  , (blackbox 'P.resize#){workInfo=Never, template="~IF~SIZE[~TYP[1]]~THEN~IF~CMPLE[~SIZE[~TYPO]][~SIZE[~TYP[1]]]~THEN~VAR[bv][1][0+:~SIZE[~TYPO]]~ELSE{{(~SIZE[~TYPO]-~SIZE[~TYP[1]]) {1'b0}},~ARG[1]}~FI~ELSE~SIZE[~TYPO]'d0~FI"}
  ]
