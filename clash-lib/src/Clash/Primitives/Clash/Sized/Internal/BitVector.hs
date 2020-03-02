module Clash.Primitives.Clash.Sized.BitVector where

import qualified Clash.Sized.BitVector as P

commonPrimitives =
  [ (blackbox 'P.undefined#){template="~ERRORO", workInfo=Constant}
  , (primitive 'P.checkUnpackUndef#){primSort="Function", workInfo=Never}
  , (blackbox 'P.le##){template="~ARG[0] <= ~ARG[1]"}
  , (blackbox 'P.lt##){template="~ARG[0] < ~ARG[1]"}
  , (blackbox 'P.ge##){template="~ARG[0] >= ~ARG[1]"}
  , (blackbox 'P.gt##){template="~ARG[0] > ~ARG[1]"}
  ]

commonVerilogPrimitives =
  [ -- THIS IS ONLY USED WHEN WW EXPOSES BITVECTOR INTERNALS
    (blackbox 'P.BV){workInfo=Never, template="~IF~CMPLE[~SIZE[~TYPO]][~SIZE[~TYP[1]]]~THEN$unsigned(~VAR[i][1][0+:~SIZE[~TYPO]])~ELSE$unsigned({{(~SIZE[~TYPO]-~SIZE[~TYP[1]]) {1'b0}},~VAR[i][1]})~FI"}
    -- THIS IS ONLY USED WHEN WW EXPOSES BITVECTOR INTERNALS
  , (blackbox 'P.Bit){workInfo=Never, template="~VAR[i][1][0]"}

  , (blackbox 'P.high){workInfo=Never, template="1'b1"}
  , (blackbox 'P.low){workInfo=Never, template="0'b1"}
  , (blackbox 'P.pack#){workInfo=Never, template="~ARG[0]"}
  , (blackbox 'P.unpack#){workInfo=Never, template="~ARG[0]"}
  , (blackbox 'P.reduceAnd#){template="~IF~SIZE[~TYP[1]]~THEN& (~ARG[1])~ELSE1'b1~FI"}
  , (blackbox 'P.reduceOr#){template="~IF~SIZE[~TYP[1]]~THEN| (~ARG[1])~ELSE1'b0~FI"}
  , (blackbox 'P.reduceXOr#){template="~IF~SIZE[~TYP[1]]~THEN^ (~ARG[1])~ELSE1'b0~FI"}
  , (blackbox 'P.eq##){template="~ARG[0] == ~ARG[1]"}
  , (blackbox 'P.neq##){template="~ARG[0] != ~ARG[1]"}
  , (blackbox 'P.fromInteger##){workInfo=Never, template="~VAR[i][0][0] ? 1'bx : ~VAR[i][1][0]"}
  , (blackbox 'P.and##){template="~ARG[0] & ~ARG[1]"}
  , (blackbox 'P.or##){template="~ARG[0] | ~ARG[1]"}
  , (blackbox 'P.xor##){template="~ARG[0] ^ ~ARG[1]"}
  , (blackbox 'P.complement##){template="- ~ARG[0]"}
  , (blackbox 'P.eq#){template="~IF~SIZE[~TYP[1]]~THEN~ARG[1] == ~ARG[2]~ELSE1'b1~FI"}
  , (blackbox 'P.neq#){template="~IF~SIZE[~TYP[1]]~THEN~ARG[1] != ~ARG[2]~ELSE1'b1~FI"}
  , (blackbox 'P.lt#){template="~IF~SIZE[~TYP[1]]~THEN~ARG[1] < ~ARG[2]~ELSE1'b1~FI"}
  , (blackbox 'P.ge#){template="~IF~SIZE[~TYP[1]]~THEN~ARG[1] >= ~ARG[2]~ELSE1'b1~FI"}
  , (blackbox 'P.gt#){template="~IF~SIZE[~TYP[1]]~THEN~ARG[1] > ~ARG[2]~ELSE1'b1~FI"}
  , (blackbox 'P.le#){template="~IF~SIZE[~TYP[1]]~THEN~ARG[1] <= ~ARG[2]~ELSE1'b1~FI"}
  , (blackboxHaskell 'P.toInteger# 'P.bvToIntegerVerilog){workInfo=Never}
  , (blackbox 'P.size#){template="~SIZE[~TYPO]'sd~SIZE[~TYP[1]]", workInfo=Constant}
  , (blackbox 'P.maxIndex#){workInfo=Constant, template="~SIZE[~TYPO]'sd~SIZE[~TYP[1]] - ~SIZE[~TYPO]'sd1"}
  , (blackbox 'P.++#){workInfo=Never, template="~IF~AND[~SIZE[~TYP[1]],~SIZE[~TYP[2]]]~THEN{~ARG[1],~ARG[2]}~ELSE~IF~SIZE[~TYP[1]]~THEN~ARG[1]~ELSE~ARG[2]~FI~FI"}
  , (blackbox 'P.index#){template="~VAR[bv][1][~ARG[2]]"}
  , (blackbox 'P.slice#){workInfo=Never, template="~VAR[bv][0][~LIT[1] : ~LIT[2]]"}
  , (blackbox 'P.msb#){workInfo=Never, template="~IF ~SIZE[~TYP[1]] ~THEN ~VAR[bv][1][~SIZE[~TYP[1]]-1] ~ELSE 1'b0 ~FI"}
  , (blackbox 'P.lsb#){workInfo=Never, template="~IF ~SIZE[~TYP[0]] ~THEN ~VAR[bv][0][0] ~ELSE 1'b0 ~FI"}
  , (blackbox 'P.minBound#){workInfo=Constant, template="~SIZE[~TYPO]'d0"}
  , (blackbox 'P.maxBound#){workInfo=Constant, template="{~SIZE[~TYPO] {1'b1}}"}
  , (blackbox 'P.+#){template="~ARG[1] + ~ARG[2]"}
  , (blackbox 'P.-#){template="~ARG[1] - ~ARG[2]"}
  , (blackbox 'P.*#){template="~ARG[1] * ~ARG[2]"}
  , (blackbox 'P.negate#){template="-~ARG[1]"}
  , (blackbox 'P.fromInteger#){workInfo=Never, template="~IF~CMPLE[~SIZE[~TYPO]][~SIZE[~TYP[2]]]~THEN$unsigned(~VAR[i][2][0+:~SIZE[~TYPO]])~ELSE$unsigned({{(~SIZE[~TYPO]-~SIZE[~TYP[2]]) {1'b0}},~VAR[i][2]})~FI"}
  , (blackbox 'P.plus#){kind=Declaration, template="assign ~RESULT = ~IF~AND[~SIZE[~TYP[2]],~SIZE[~TYP[3]]]~THEN~ARG[2] + ~ARG[3]~ELSE~IF~SIZE[~TYP[2]]~THEN~ARG[2]~ELSE~ARG[3]~FI~FI;"}
  , (blackbox 'P.minus#){kind=Declaration, template="assign ~RESULT = ~IF~AND[~SIZE[~TYP[2]],~SIZE[~TYP[3]]]~THEN~ARG[2] - ~ARG[3]~ELSE~IF~SIZE[~TYP[2]]~THEN~ARG[2]~ELSE-~ARG[3]~FI~FI;"}
  , (blackbox 'P.times#){kind=Declaration, template="assign ~RESULT = ~IF~AND[~SIZE[~TYP[2]],~SIZE[~TYP[3]]]~THEN~ARG[2] * ~ARG[3]~ELSE~SIZE[~TYPO]'d0~FI;"}
  , (blackbox 'P.quot#){template="~ARG[1] / ~ARG[2]"}
  , (blackbox 'P.rem#){template="~ARG[1] % ~ARG[2]"}
  , (blackbox 'P.and#){template="~ARG[1] & ~ARG[2]"}
  , (blackbox 'P.or#){template="~ARG[1] | ~ARG[2]"}
  , (blackbox 'P.xor#){template="~ARG[1] ^ ~ARG[2]"}
  , (blackbox 'P.complement#){template="- ~ARG[1]"}
  , (blackbox 'P.shiftL#){template="~ARG[1] << ~ARG[2]"}
  , (blackbox 'P.shiftR#){template="~ARG[1] >> ~ARG[2]"}
  , (blackbox 'P.truncateB#){template="~VAR[bv][1][0+:~SIZE[~TYPO]]"}
  ]
