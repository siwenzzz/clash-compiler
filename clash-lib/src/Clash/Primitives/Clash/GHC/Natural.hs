module Clash.Primitives.GHC.Natural where

import qualified GHC.Natural as P


commonPrimitives =
  [ (primitive 'P.NatS#){primSort="Constructor", workInfo=Never}
  , (blackbox 'P.underflowError){workInfo=Constant, template="~ERRORO"}
  -- "warning": "GHC.Natural.plusNatural: Naturals are dynamically sized in simulation, but fixed-length after synthesization. Use carefully."
  , (blackbox 'P.plusNatural){template="~ARG[0] + ~ARG[1]"}
  , (blackbox 'P.minusNatural){template="~ARG[0] - ~ARG[1]"}
  , (primitive 'P.gcdNatural){primSort="Function", workInfo=Never}
  ]

commonVerilogPrimitives =
      -- TODO warning "GHC.Natural.naturalFromInteger: Naturals are dynamically sized in simulation, but fixed-length after synthesization. Use carefully."
  [ (blackbox 'P.naturalFromInteger){workInfo=Never, template="~ERRORO"}
  , (blackbox 'P.timesNatural){template="~ARG[0] * ~ARG[1]"}
  , (blackbox 'P.wordToNatural#){
      workInfo=Never
    , kind=Declaration
    , template="assign ~RESULT = $unsigned(~ARG[0]);"
    }
  ]