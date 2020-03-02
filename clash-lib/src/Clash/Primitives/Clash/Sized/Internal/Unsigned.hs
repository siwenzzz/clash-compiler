module Clash.Primitives.Clash.Sized.Internal.Unsigned where

import qualified Clash.Sized.Internal.Unsigned as P

commontPrimitives =
  [ (blackbox 'P.+#){template="~ARG[1] + ~ARG[2]"}
  , (blackbox 'P.-#){template="~ARG[1] - ~ARG[2]"}
  , (blackbox 'P.quot#){template="~ARG[0] / ~ARG[1]"}
  ]

commonVerilogPrimitives =
  [ (blackbox 'P.eq#){template="~IF~SIZE[~TYP[0]]~THEN~ARG[0] == ~ARG[1]~ELSE1'b1~FI"}
  , (blackbox 'P.neq#){template="~IF~SIZE[~TYP[0]]~THEN~ARG[0] != ~ARG[1]~ELSE1'b0~FI"}
  , (blackbox 'P.lt#){template="~IF~SIZE[~TYP[0]]~THEN~ARG[0] < ~ARG[1]~ELSE1'b0~FI"}
  , (blackbox 'P.ge#){template="~IF~SIZE[~TYP[0]]~THEN~ARG[0] >= ~ARG[1]~ELSE1'b1~FI"}
  , (blackbox 'P.gt#){template="~IF~SIZE[~TYP[0]]~THEN~ARG[0] > ~ARG[1]~ELSE1'b0~FI"}
  , (blackbox 'P.le#){template="~IF~SIZE[~TYP[0]]~THEN~ARG[0] <= ~ARG[1]~ELSE1'b1~FI"}
  , (blackboxHaskellTF 'P.toInteger# 'P.unsignedToIntegerVerilog){workInfo=Never}
  , (blackbox 'P.size#){workInfo=Constant, template="~SIZE[~TYPO]'sd~LIT[0]"}
  , (blackbox 'P.pack#){workInfo=Never, template="~ARG[0]"}
  , (blackbox 'P.unpack#){workInfo=Never, template="~ARG[1]"}
  , (blackbox 'P.minBound#){workInfo=Constant, template="~SIZE[~TYPO]'d0"}
  , (blackbox 'P.maxBound#){workInfo=Constant, template="{~LIT[0] {1'b1}}"}
  , (blackbox 'P.*#){template="~ARG[1] * ~ARG[2]"}
  , (blackbox 'P.negate#){template="- ~ARG[1]"}
  , (blackbox 'P.fromInteger#){workInfo=Never, template="~IF~CMPLE[~SIZE[~TYPO]][~SIZE[~TYP[1]]]~THEN$unsigned(~VAR[i][1][0+:~SIZE[~TYPO]])~ELSE$unsigned({{(~SIZE[~TYPO]-~SIZE[~TYP[1]]) {1'b0}},~VAR[i][1]})~FI"}
  , (blackbox 'P.plus#){kind=Declaration, template="assign ~RESULT = ~IF~AND[~SIZE[~TYP[0]],~SIZE[~TYP[1]]]~THEN~ARG[0] + ~ARG[1]~ELSE~IF~SIZE[~TYP[0]]~THEN~ARG[0]~ELSE~ARG[1]~FI~FI;"}
  , (blackbox 'P.minus#){kind=Declaration, template="assign ~RESULT = ~IF~AND[~SIZE[~TYP[2]],~SIZE[~TYP[3]]]~THEN ~ARG[2] - ~ARG[3]~ELSE~IF~SIZE[~TYP[2]]~THEN ~ARG[2]~ELSE - ~ARG[3]~FI~FI;"}
  , (blackbox 'P.times#){kind=Declaration, template="assign ~RESULT = ~IF~AND[~SIZE[~TYP[0]],~SIZE[~TYP[1]]]~THEN~ARG[0] * ~ARG[1]~ELSE~SIZE[~TYPO]'d0~FI;"}
  , (blackbox 'P.rem#){template="~ARG[0] % ~ARG[1]"}
  , (blackbox 'P.and#){template="~ARG[1] & ~ARG[2]"}
  , (blackbox 'P.or#){template="~ARG[1] | ~ARG[2]"}
  , (blackbox 'P.xor#){template="~ARG[1] ^ ~ARG[2]"}
  , (blackbox 'P.complement#){template="- ~ARG[1]"}
  , (blackbox 'P.shiftL#){template="~ARG[1] << ~ARG[2]"}
  , (blackbox 'P.shiftR#){template="~ARG[1] >> ~ARG[2]"}
  , (blackbox 'P.resize#){workInfo=Never, template="~IF~SIZE[~TYP[1]]~THEN~IF~CMPLE[~SIZE[~TYPO]][~SIZE[~TYP[1]]]~THEN~VAR[bv][1][0+:~SIZE[~TYPO]]~ELSE{{(~SIZE[~TYPO]-~SIZE[~TYP[1]]) {1'b0}},~ARG[1]}~FI~ELSE~SIZE[~TYPO]'d0~FI"}
  ]