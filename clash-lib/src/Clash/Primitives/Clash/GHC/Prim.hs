module Clash.Primitives.Clash.GHC.Prim where

import qualified Clash.GHC.Prim as P

commonPrimitives =
  [ (primitive 'P.dataToTag#){primSort="Function"}
  , (primitive 'P.tagToEnum#){primSort="Function"}
  , (blackbox 'P.unsafeCoerce#){workInfo=Never, template="~ARG[0]"}
  , (blackbox 'P.+#){template="~ARG[0] + ~ARG[1]"}
  , (blackbox 'P.-#){template="~ARG[0] - ~ARG[1]"}
  , (blackbox 'P.quotInt#){template="~ARG[0] / ~ARG[1]"}
  , (blackbox 'P.plusWord#){template="~ARG[0] + ~ARG[1]"}
  , (blackbox 'P.minusWord#){template="~ARG[0] - ~ARG[1]"}
  , (blackbox 'P.quotWord#){template="~ARG[0] / ~ARG[1]"}
  ]

commonVerilogPrimitives =
  [ (blackbox 'P.gtChar#){template="(~ARG[0] > ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.geChar#){template="(~ARG[0] >= ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.eqChar#){template="(~ARG[0] == ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.neChar#){template="(~ARG[0] != ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.ltChar#){template="(~ARG[0] < ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.leChar#){template="(~ARG[0] <= ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.ord#){template="$signed({{(~SIZE[~TYPO]-~SIZE[~TYP[0]]) {1'b0}},~VAR[c][0]})"}
  , (blackbox 'P.*#){template="~ARG[0] * ~ARG[1]"}
  , (blackbox 'P.remInt#){template="~ARG[0] % ~ARG[1]"}
  , (blackbox 'P.andI#){template="~ARG[0] & ~ARG[1]"}
  , (blackbox 'P.orI#){template="~ARG[0] | ~ARG[1]"}
  , (blackbox 'P.xorI#){template="~ARG[0] ^ ~ARG[1]"}
  , (blackbox 'P.notI#){template="- ~ARG[0]"}
  , (blackbox 'P.negateInt#){template="-(~ARG[0])"}
  , (blackbox 'P.>#){template="(~ARG[0] > ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.>=#){template="(~ARG[0] >= ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.==#){template="(~ARG[0] == ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P./=#){template="(~ARG[0] /= ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.<#){template="(~ARG[0] < ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.<=#){template="(~ARG[0] <= ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.chr#){template="$unsigned(~VAR[i][0][0+:~SIZE[~TYPO]])"}
  , (blackbox 'P.int2Word#){workInfo=Never, template="$unsigned(~ARG[0])"}
  , (blackbox 'P.uncheckedIShiftL#){template="~ARG[0] <<< ~ARG[1]"}
  , (blackbox 'P.uncheckedIShiftRA#){template="~ARG[0] >>> ~ARG[1]"}
  , (blackbox 'P.uncheckedIShiftRL#){template="~ARG[0] >> ~ARG[1]"}
  , (blackbox 'P.timesWord#){template="~ARG[0] * ~ARG[1]"}
  , (blackbox 'P.remWord#){template="~ARG[0] % ~ARG[1]"}
  , (blackbox 'P.and#){template="~ARG[0] & ~ARG[1]"}
  , (blackbox 'P.or#){template="~ARG[0] | ~ARG[1]"}
  , (blackbox 'P.xor#){template="~ARG[0] ^ ~ARG[1]"}
  , (blackbox 'P.not#){template="- ~ARG[0]"}
  , (blackbox 'P.uncheckedShiftL#){template="~ARG[0] << ~ARG[1]"}
  , (blackbox 'P.word2Int#){workInfo=Never, template="$signed(~ARG[0])"}
  , (blackbox 'P.gtWord#){template=("~ARG[0] > ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.geWord#){template=("(~ARG[0] >= ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.eqWord#){template=("(~ARG[0] == ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.neWord#){template=("(~ARG[0] != ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.ltWord#){template=("(~ARG[0] < ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.leWord#){template=("(~ARG[0] <= ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.byteSwap16#){
      workInfo=Never
    , kind=Declaration
    , template=[I.i|
        // byteSwap16 begin~IF ~IW64 ~THEN
        assign ~RESULT = {~VAR[w][0][63:16],~VAR[w][0][7:0],~VAR[w][0][15:8]};~ELSE
        assign ~RESULT = {~VAR[w][0][31:16],~VAR[w][0][7:0],~VAR[w][0][15:8]};~FI
        // byteSwap16 end
      |]
    }
  , (blackbox 'P.byteSwap32#){
      workInfo=Never
    , kind=Declaration
    , template=[I.i|
        // byteSwap32 begin~IF ~IW64 ~THEN
        assign ~RESULT = {~VAR[w][0][63:32],~VAR[w][0][7:0],~VAR[w][0][15:8],~VAR[w][0][23:16],~VAR[w][0][31:24]};~ELSE
        assign ~RESULT = {~VAR[w][0][7:0],~VAR[w][0][15:8],~VAR[w][0][23:16],~VAR[w][0][31:24]};~FI
        // byteSwap32 end
      |]
    }
  , (blackbox 'P.byteSwap64#){
      workInfo=Never
    , kind=Declaration
    , template=[I.i|
        // byteSwap64 begin
        assign ~RESULT = {~VAR[w][0][7:0],~VAR[w][0][15:8],~VAR[w][0][23:16],~VAR[w][0][31:24]
                         ,~VAR[w][0][39:32],~VAR[w][0][47:40],~VAR[w][0][55:48],~VAR[w][0][63:56]};
        // byteSwap64 end
      |]
    }
  , (blackbox 'P.byteSwap#){
      workInfo=Never
    , kind=Declaration
    , template=[I.i|
        // byteSwap begin~IF ~IW64 ~THEN
        assign ~RESULT = {~VAR[w][0][7:0],~VAR[w][0][15:8],~VAR[w][0][23:16],~VAR[w][0][31:24]
                         ,~VAR[w][0][39:32],~VAR[w][0][47:40],~VAR[w][0][55:48],~VAR[w][0][63:56]};~ELSE
        assign ~RESULT = {~VAR[w][0][7:0],~VAR[w][0][15:8],~VAR[w][0][23:16],~VAR[w][0][31:24]};~FI
        // byteSwap end
      |]
    }
  , (blackbox 'P.narrow8Int#){
      workInfo=Never
    , kind=Declaration
    , template=[I.i|
        // narrow8Int begin
        assign ~RESULT = $signed(~VAR[i][0][7:0]);
        // narrow8Int end
      |]
    }
  , (blackbox 'P.narrow16Int#){
      workInfo=Never
    , kind=Declaration
    , template=[I.i|
        // narrow16Int begin
        assign ~RESULT = $signed(~VAR[i][0][15:0]);
        // narrow16Int en
      |]
    }
  , (blackbox 'P.narrow32Int#){
      workInfo=Never
    , kind=Declaration
    , template=[I.i|
        // narrow32Int begin
        assign ~RESULT = $signed(~VAR[i][0][31:0]);
        // narrow32Int end
      |]
    }
  , (blackbox 'P.narrow8Word#){
      workInfo=Never
    , kind=Declaration
    , template=[I.i|
        // narrow8Word begin
        assign ~RESULT = $unsigned(~VAR[w][0][7:0]);
        // narrow8Word end
      |]
    }
  , (blackbox 'P.narrow16Word#){
      workInfo=Never
    , kind=Declaration
    , template=[I.i|
        // narrow16Word begin
        assign ~RESULT = $unsigned(~VAR[w][0][15:0]);
        // narrow16Word end
      |]
    }
  , (blackbox 'P.narrow32Word#){
      workInfo=Never
    , kind=Declaration
    , template=[I.i|
        // narrow32Word begin
        assign ~RESULT = $unsigned(~VAR[w][0][31:0]);
        // narrow32Word end
      |]
    }
  ]