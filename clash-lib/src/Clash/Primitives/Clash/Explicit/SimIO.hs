module Clash.Primitives.Clash.Explicit.SimIO where

import qualified Clash.Explicit.SimIO as P

commonVerilogPrimitives =
  [ (primitive 'P.mealyIO){primSort="Function"}
  , (blackbox 'P.display){
      renderVoid=RenderVoid
    , kind=Declaration
    , template="$display(~ARG[0]);"
    }
  , (blackbox 'P.finish){
      renderVoid=RenderVoid
    , kind=Declaration
    , template="$finish_and_return(~LIT[0]);"
    }
  , (blackbox 'P.reg){template="~ARG[0]"}
  , (blackbox 'P.readReg){template="~ARG[0]"}
  , (blackbox 'P.writeReg){
      renderVoid=RenderVoid
    , kind=Declaration
    , template="~ARG[0] = ~ARG[1];"
    }
  , (blackbox 'P.openFile){template="$fopen(~FILE[~LIT[0]],~LIT[1])"}
  , (blackbox 'P.closeFile){
      renderVoid=RenderVoid
    , kind=Declaration
    , template="$fclose(~ARG[0]);"
    }
  , (blackbox 'P.getChar){template="$fgetc(~ARG[0])"}
  , (blackbox 'P.putChar){
      renderVoid=RenderVoid
    , kind=Declaration
    , template="$ungetc(~ARG[0],~ARG[1]);"
    }
  , (blackbox 'P.getLine){
      kind=Declaration
    , template="~RESULT = $fgets(~ARG[2],~ARG[1]);"
    }
  , (blackbox 'P.isEOF){template="$feof(~ARG[0])"}
  , (blackbox 'P.flush){
      renderVoid=RenderVoid
    , kind=Declaration
    , template="$fflush(~ARG[0]);"
    }
  , (blackbox 'P.seek){
      kind=Declaration
    , template="~RESULT = $fseek(~ARG[0],~ARG[1],~ARG[2]);"
    }
  , (blackbox 'P.rewind){
      kind=Declaration
    , template="~RESULT = $rewind(~ARG[0]);"
    }
  , (blackbox 'P.tell){template="$ftell(~ARG[0])"}
  , (primitive 'P.fmapSimIO#){primSort="Function"}
  , (primitive 'P.pureSimIO#){primSort="Function"}
  , (primitive 'P.apSimIO#){primSort="Function"}
  , (primitive 'P.bindSimIO#){primSort="Function"}
  , (primitive 'P.unSimIO#){primSort="Function"}
  ]
