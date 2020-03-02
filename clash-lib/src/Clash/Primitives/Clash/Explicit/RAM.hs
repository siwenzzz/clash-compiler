module Clash.Primitives.Clash.Explicit.RAM where

import qualified Clash.Explicit.RAM as P

systemverilogPrimitives =
  [ -- asyncRam#
    --  :: ( hascallstack              -- arg[0]
    --     , knowndomain wdom wconf    -- arg[1]
    --     , knowndomain rdom rconf )  -- arg[2]
    --  => clock wdom                  -- ^ wclk, arg[3]
    --  -> clock rdom                  -- ^ rclk, arg[4]
    --  -> enable wdom                 -- ^ wen,  arg[5]
    --  -> snat n                      -- ^ sz,   arg[6]
    --  -> signal rdom int             -- ^ rd,   arg[7]
    --  -> signal wdom bool            -- ^ en,   arg[8]
    --  -> signal wdom int             -- ^ wr,   arg[9]
    --  -> signal wdom a               -- ^ din,  arg[10]
    --  -> signal rdom a
    (blackbox 'P.asyncRam#){
      kind=Declaration
    , template=[I.i|
        // asyncRam begin
        logic [~SIZE[~TYP[10]]-1:0] ~GENSYM[RAM][0] [0:~LIT[6]-1];
        always @(~IF~ACTIVEEDGE[Rising][1]~THENposedge~ELSEnegedge~FI ~ARG[3]) begin : ~GENSYM[~COMPNAME_Ram][1]
          if (~IF ~ISACTIVEENABLE[5] ~THEN ~ARG[5] & ~ELSE ~FI ~ARG[8]) begin
            ~SYM[0][~ARG[9]] <= ~TOBV[~ARG[10]][~TYP[10]];
          end
        end

        assign ~RESULT = ~FROMBV[~SYM[0][\\~ARG[7]\\]][~TYPO];
        // asyncRam end
      |]
    }
  ]