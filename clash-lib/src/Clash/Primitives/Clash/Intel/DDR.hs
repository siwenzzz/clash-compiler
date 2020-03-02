module Clash.Primitives.Clash.Intel.DDR where

import qualified Clash.Intel.DDR as P

commonVerilogPrimitives =
  [-- "altddioOut#
    --   :: ( HasCallStack             -- ARG[0]
    --      , KnownConfi~ fast domf    -- ARG[1]
    --      , KnownConfi~ slow doms    -- ARG[2]
    --      , KnownNat m )             -- ARG[3]
    --   => SSymbol deviceFamily       -- ARG[4]
    --   -> Clock slow                 -- ARG[5]
    --   -> Reset slow                 -- ARG[6]
    --   -> Enable slow                -- ARG[7]
    --   -> Signal slow (BitVector m)  -- ARG[8]
    --   -> Signal slow (BitVector m)  -- ARG[9]
    --   -> Signal fast (BitVector m)
    (blackbox P.altddioOut#){
        libraries=["altera_mf"]
      , kind=Declaration
      , template=[I.i|
        // altddioOut begin
        altddio_out
          #(
            .extend_oe_disable (\"OFF\"),
            .intended_device_family (~LIT[4]),
            .invert_output (\"OFF\"),
            .lpm_hint (\"UNUSED\"),
            .lpm_type (\"altddio_out\"),
            .oe_reg (\"UNREGISTERED\"),
            .power_up_high (\"OFF\"),
            .width (~SIZE[~TYPO])
          )
          ~GENSYM[~COMPNAME_ALTDDIO_OUT][7] (~IF ~ISSYNC[2] ~THEN
            .sclr (~ARG[6]),
            .aclr (1'b0),~ELSE
            .aclr (~ARG[6]),
            .sclr (1'b0),~FI
            .datain_h (~ARG[8]),
            .datain_l (~ARG[9]),
            .outclock (~ARG[5]),
            .outclocken (~IF ~ISACTIVEENABLE[7] ~THEN ~ARG[7] ~ELSE 1'b1 ~FI),
            .dataout (~RESULT),
            .aset (1'b0),
            .sset (1'b0),
            .oe (1'b1),
            .oe_out ()
          );
        // altddioOut end
      |]
    }
  ]

systemverilogPrimitives =
  [ -- altddioIn
    --  :: ( HasCallStack               -- ARG[0]
    --     , KnownConfi~ fast domf      -- ARG[1]
    --     , KnownConfi~ slow doms      -- ARG[2]
    --     , KnownNat m )               -- ARG[3]
    --  => SSymbol deviceFamily         -- ARG[4]
    --  -> Clock slow                   -- ARG[5]
    --  -> Reset slow                   -- ARG[6]
    --  -> Enable slow                  -- ARG[7]
    --  -> Signal fast (BitVector m)    -- ARG[8]
    --  -> Signal slow (BitVector m,BitVector m)
    (blackbox P.altddioIn#){
        libraries=["altera_mf"]
      , kind=Declaration
      , template=[I.i|
          // altddioIn begin
          ~SIGD[~GENSYM[dataout_l][1]][8];
          ~SIGD[~GENSYM[dataout_h][2]][8];

          altddio_in
            #(
              .intended_device_family (~LIT[4]),
              .invert_input_clocks (\"OFF\"),
              .lpm_hint (\"UNUSED\"),
              .lpm_type (\"altddio_in\"),
              .power_up_high (\"OFF\"),
              .width (~SIZE[~TYP[8]])
            )
            ~GENSYM[~COMPNAME_ALTDDIO_IN][7] (~IF ~ISSYNC[2] ~THEN
              .sclr (~ARG[6]),
              .aclr (1'b0),~ELSE
              .aclr (~ARG[6]),
              .sclr (1'b0),~FI
              .datain (~ARG[8]),
              .inclock (~ARG[5]),
              .inclocken (~IF ~ISACTIVEENABLE[7] ~THEN~ARG[7]~ELSE1'b1,~FI),
              .dataout_h (~SYM[2]),
              .dataout_l (~SYM[1]),
              .aset (1'b0),
              .sset (1'b0)
            );
          assign ~RESULT = {~SYM[1],~SYM[2]};
          // altddioIn end
        |]
      }
  ]