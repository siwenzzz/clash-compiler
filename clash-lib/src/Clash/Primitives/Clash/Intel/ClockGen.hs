module Clash.Primitives.Clash.Intel.ClockGen where

import qualified Clash.Intel.ClockGen as P

commonPrimitives =
  [ (blackbox 'P.altpll){
      workInfo=Always
    , kind=Declaration
    , includes=(("altpll", "qsys"), mkTemplateFunction 'P.altpllQsysTF)
    , template=templateFunction 'P.altpllTF
    }
  , (blackbox 'P.alteraPll){
      workInfo=Always
    , kind=Declaration
    , includes=(("altera_pll", "qsys"), mkTemplateFunction 'P.alteraPllQsysTF)
    , template=templateFunction 'P.alteraPllTF
    }
  ]
