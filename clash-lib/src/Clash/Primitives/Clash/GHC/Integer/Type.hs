module Clash.Primitives.GHC.Integer.Type where

import qualified GHC.Integer.Type as P

commonPrimitives =
  [ (blackbox 'P.plusInteger){template="~ARG[0] + ~ARG[1]"}
  , (blackbox 'P.minusInteger){template="~ARG[0] - ~ARG[1]"}
  , (blackbox 'P.quotInteger){template="~ARG[0] / ~ARG[1]"}
  , (blackbox 'P.leInteger){template="~ARG[0] <= ~ARG[1]"}
  , (blackbox 'P.gtInteger){template="~ARG[0] > ~ARG[1]"}
  , (blackbox 'P.ltInteger){template="~ARG[0] < ~ARG[1]"}
  , (blackbox 'P.geInteger){template="~ARG[0] >= ~ARG[1]"}
  ]

commonVerilogPrimitives =
  [ (blackbox 'P.smallInteger){
      workInfo=Never
    , kind=Declaration
    , template="assign ~RESULT = $signed(~ARG[0]);"
    }
  , (blackbox 'P.integerToInt){
      workInfo=Never
    , kind=Declaration
    , template="assign ~RESULT = $signed(~ARG[0]);"
    }
  , (blackbox 'P.timesInteger){template="~ARG[0] * ~ARG[1]"}
  , (blackbox 'P.negateInteger){template="-~ARG[0]"}
  , (blackbox 'P.absInteger){template="(~ARG[0] < ~SIZE[~TYPO]'sd0) ? -~ARG[0] : ~ARG[0]"}
  , (blackbox 'P.remInteger){template="~ARG[0] % ~ARG[1]"}
  , (blackbox 'P.eqInteger){template="~ARG[0] == ~ARG[1]"}
  , (blackbox 'P.neqInteger){template="~ARG[0] != ~ARG[1]"}
  , (blackbox 'P.eqInteger#){template="(~ARG[0] == ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.neqInteger#){template="(~ARG[0] != ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.leInteger#){template="(~ARG[0] <= ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.gtInteger#){template="(~ARG[0] > ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.ltInteger#){template="(~ARG[0] < ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.geInteger#){template="(~ARG[0] >= ~ARG[1]) ? ~SIZE[~TYPO]'sd1 : ~SIZE[~TYPO]'sd0"}
  , (blackbox 'P.shiftRInteger){template="~ARG[0] >>> ~ARG[1]"}
  , (blackbox 'P.shiftLInteger){template="~ARG[0] <<< ~ARG[1]"}
  , (blackbox 'P.testBitInteger){template="~VAR[input][0][~ARG[1]] == 1'b1"}
  , (blackbox 'P.wordToInteger){
      kind=Declaration
    , workInfo=Never
    , template="assign ~RESULT = $signed(~ARG[0]);"
    }
  , (blackbox 'P.integerToWord){
      kind=Declaration
    , workInfo=Never
    , template="assign ~RESULT = $unsigned(~ARG[0]);"
    }
    -- only used by 32 bit GHC:
  , (blackbox 'P.integerToWord64){
      kind=Declaration
    , workInfo=Never
    , template="assign ~RESULT = $unsigned(~ARG[0]);"
    }
  , (blackbox 'P.bitInteger){template="1 << ~ARG[0]"}
  , (blackbox 'P.complementInteger){template="- ~ARG[0]"}
  , (blackbox 'P.xorInteger){template="~ARG[0] ^ ~ARG[1]"}
  , (blackbox 'P.orInteger){template="~ARG[0] | ~ARG[1]"}
  , (blackbox 'P.andInteger){template="~ARG[0] & ~ARG[1]"}
  , (blackbox' "GHC.Integer.Type.$wsignumInteger"){template="(~ARG[0] < ~SIZE[~TYPO]'sd0) ? -~SIZE[~TYPO]'sd1 : ((~ARG[0] == ~SIZE[~TYPO]'sd0) ? ~SIZE[~TYPO]'sd0 : ~SIZE[~TYPO]'sd1)"}
  ]
