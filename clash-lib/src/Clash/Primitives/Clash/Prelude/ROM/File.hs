module Clash.Primitives.Clash.Prelude.ROM.File where

import qualified Clash.Prelude.ROM.File as P

systemverilogPrimitives =
  [ -- asyncRomFile :: KnownNat m -- ARG[0]
    --              => SNat n     -- sz,   ARG[1]
    --              -> FilePath   -- file, ARG[2]
    --              -> Int        -- rd,   ARG[3]
    --              -> BitVector m
    (blackbox 'P.asyncRomFile#){
      kind=Declaration
    , template=[I.i|
        // asyncRomFile begin
        ~SIGDO[~GENSYM[ROM][0]] [0:~LIT[1]-1];

        initial begin
          $readmemb(~FILE[~LIT[2]],~SYM[0]);
        end

        assign ~RESULT = ~SYM[0][~ARG[3]];
        // asyncRomFile end
      |]
    }
  ]