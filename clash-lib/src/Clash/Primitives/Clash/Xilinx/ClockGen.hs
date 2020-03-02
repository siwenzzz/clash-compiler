module Clash.Primitives.Clash.Xilinx.ClockGen where

import qualified Clash.Xilinx.ClockGen as P

commonVerilogPrimitives =
  [ -- clockWizard
    --   :: ( KnownDomain domIn confIn       -- ARG[0]
    --      , KnownDomain domOut confOut )   -- ARG[1]
    --   => SSymbol name                    -- ARG[2]
    --   -> Clock  pllIn                    -- ARG[3]
    --   -> Reset pllIn                     -- ARG[4]
    --   -> (Clock pllOut, Enable pllOut)
    (blackbox 'P.clockWizard){
      workInfo=Always
    , kind=Declaration
    , template=[I.i|
        // clockWizard begin
        ~NAME[2] ~GENSYM[clockWizard_inst][2]
        (.CLK_IN1  (~ARG[3])
        ,.RESET    (~IF ~ISACTIVEHIGH[0] ~THEN ~ELSE ! ~FI ~ARG[4])
        ,.CLK_OUT1 (~RESULT[1])
        ,.LOCKED   (~RESULT[0]));
        // clockWizard end
      |]
    -- clockWizardDifferential
    --   :: ( KnownDomain domIn confIn       -- ARG[0]
    --      , KnownDomain domOut confOut )   -- ARG[1]
    --   :: SSymbol name                    -- ARG[2]
    --   -> Clock  pllIn                    -- ARG[3]
    --   -> Clock  pllIn                    -- ARG[4]
    --   -> Reset pllIn                     -- ARG[5]
    --   -> (Clock pllOut, Enable pllOut)
  , (blackbox 'P.clockWizardDifferential){
      workInfo=Always
    , kind=Declaration
    , template=[I.i|
        // clockWizardDifferential begin
        ~NAME[2] ~GENSYM[clockWizardDifferential_inst][2]
        (.CLK_IN1_D_clk_n (~ARG[3])
        ,.CLK_IN1_D_clk_n (~ARG[4])
        ,.RESET           (~IF ~ISACTIVEHIGH[0] ~THEN ~ELSE ! ~FI ~ARG[5])
        ,.CLK_OUT1        (~RESULT[1])
        ,.LOCKED          (~RESULT[0]));
        // clockWizardDifferential end
      |]
  ]
