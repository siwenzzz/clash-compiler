module Clash.Primitives.Clash.Explicit.ROM where

import qualified Clash.Explicit.ROM as P

systemverilogPrimitives =
  [ -- rom# :: ( KnownDomain dom        ARG[0]
    --         , KnownNat n    --       ARG[1]
    --         , Undefined a ) --       ARG[2]
    --      => Clock dom       -- clk,  ARG[3]
    --      => Enable dom      -- en,   ARG[4]
    --      -> Vec n a         -- init, ARG[5]
    --      -> Signal dom Int  -- rd,   ARG[6]
    --      -> Signal dom a
    (blackbox 'P.rom#){
      kind=Declaration
    , template=[I.i|
        // rom begin
        ~SIGD[~GENSYM[ROM][1]][5];
        assign ~SYM[1] = ~LIT[5];

        logic [~SIZE[~TYPO]-1:0] ~GENSYM[~RESULT_q][2];~IF ~ISACTIVEENABLE[4] ~THEN
        always @(~IF~ACTIVEEDGE[Rising][0]~THENposedge~ELSEnegedge~FI ~ARG[3]) begin : ~GENSYM[~COMPNAME_rom][3]
          if (~ARG[4]) begin
            ~SYM[2] <= ~SYM[1][~ARG[6]];
          end
        end~ELSE
        always @(~IF~ACTIVEEDGE[Rising][0]~THENposedge~ELSEnegedge~FI ~ARG[3]) begin : ~SYM[3]
          ~SYM[2] <= ~SYM[1][~ARG[6]];
        end~FI

        assign ~RESULT = ~FROMBV[~SYM[2]][~TYPO];
        // rom end
      |]
    }
    -- asyncRom# :: KnownNat n -- ^ ARG[0]
    --           => Vec n a    -- ^ ARG[1]
    --           -> Int        -- ^ ARG[2]
    --           -> a
  , (blackbox 'P.asyncRom#){
      kind=Declaration
    , template=[I.i|
        // asyncRom begin
        ~SIGD[~GENSYM[ROM][0]][1];
        assign ~SYM[0] = ~LIT[1];

        assign ~RESULT = ~FROMBV[~SYM[0][\\~ARG[2]\\]][~TYPO];
        // asyncRom end
      |]
  ]