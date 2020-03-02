module Clash.Primitives.Clash.Sized.Internal.Signed where

import qualified Clash.Sized.Internal.Signed as P

commonPrimitives =
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
  , (blackboxHaskellTF 'P.toInteger# 'P.signedToIntegerVerilog){workInfo=Never}
  , (blackbox 'P.size#){workInfo=Constant, template="~SIZE[~TYPO]'sd~LIT[0]"}
  , (blackbox 'P.pack#){workInfo=Never, template="$unsigned(~ARG[1])"}
  , (blackbox 'P.unpack#){workInfo=Never, template="$signed(~ARG[1])"}
    -- Generates incorrect SV for ~LIT[0] == 0:
  , (blackbox 'P.minBound#){workInfo=Constant, template="$signed({1'b1, {(~LIT[0]-1) {1'b0}}})"}
    -- Generates incorrect SV for ~LIT[0] == 0:
  , (blackbox 'P.maxBound#){workInfo=Constant, template="$signed({1'b0, {(~LIT[0]-1) {1'b1}}})"}
  , (blackbox 'P.*#){template="~ARG[1] * ~ARG[2]"}
  , (blackbox 'P.negate#){template="-~ARG[1]"}
  , (blackbox 'P.abs#){template="(~ARG[1] < ~LIT[0]'sd0) ? -~ARG[1] : ~ARG[1]"}
  , (blackbox 'P.fromInteger#){workInfo=Never, template="~IF~CMPLE[~SIZE[~TYPO]][~SIZE[~TYP[1]]]~THEN$signed(~VAR[i][1][0+:~SIZE[~TYPO]])~ELSE$signed({{(~SIZE[~TYPO]-~SIZE[~TYP[1]]) {1'b0}},~VAR[i][1]})~FI"}
  , (blackbox 'P.plus#){kind=Declaration, template="assign ~RESULT = ~IF~AND[~SIZE[~TYP[0]],~SIZE[~TYP[1]]]~THEN~ARG[0] + ~ARG[1]~ELSE~IF~SIZE[~TYP[0]]~THEN~ARG[0]~ELSE~ARG[1]~FI~FI;"}
  , (blackbox 'P.minus#){kind=Declaration, template="assign ~RESULT = ~IF~AND[~SIZE[~TYP[0]],~SIZE[~TYP[1]]]~THEN ~ARG[0] - ~ARG[1]~ELSE~IF~SIZE[~TYP[0]]~THEN ~ARG[0]~ELSE - ~ARG[1] ~FI~FI;"}
  , (blackbox 'P.times#){kind=Declaration, template="assign ~RESULT = ~IF~AND[~SIZE[~TYP[0]],~SIZE[~TYP[1]]]~THEN~ARG[0] * ~ARG[1]~ELSE~SIZE[~TYPO]'d0~FI;"}
  , (blackbox 'P.rem#){template="~ARG[0] % ~ARG[1]"}
  , (blackbox 'P.and#){template="~ARG[1] & ~ARG[2]"}
  , (blackbox 'P.or#){template="~ARG[1] | ~ARG[2]"}
  , (blackbox 'P.xor#){template="~ARG[1] ^ ~ARG[2]"}
  , (blackbox 'P.complement#){template="- ~ARG[1]"}
  , (blackbox 'P.shiftL#){template="~ARG[1] << ~ARG[2]"}
  , (blackbox 'P.shiftR#){template="~ARG[1] >> ~ARG[2]"}
  , (blackbox 'P.truncateB#){workInfo=Never, template="$signed(~VAR[s][1][0+:~SIZE[~TYPO]])"}
  , (blackbox 'P.resize#){workInfo=Never, template="~IF~SIZE[~TYP[2]]~THEN~IF~CMPLE[~SIZE[~TYPO]][~SIZE[~TYP[2]]]~THEN$signed({~VAR[s][2][~LIT[0]-1],~VAR[s][2][0+:(~SIZE[~TYPO]-1)]})~ELSE$signed({{(~SIZE[~TYPO]-~SIZE[~TYP[2]]) {~VAR[s][2][~LIT[0]-1]}},~VAR[s][2]})~FI~ELSE~SIZE[~TYPO]'sd0~FI"}
  ]