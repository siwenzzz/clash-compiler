module Clash.Primitives.Clash.Promoted.Nat where

import qualified Clash.Promoted.Nat as P

commontPrimitives =
  [ (blackbox 'P.powSNat){workInfo=Never, template="~LIT[0] ** ~LIT[1]"}
  ]

commonVerilogPrimitives =
  [ -- :: (2 <= base, 1 <= x) => SNat base -> SNat x -> SNat (FLog base x)
    (blackbox 'P.flogBaseSNat){
      workInfo=Never
    , imports=["~INCLUDENAME[0].inc"]
    , includes=(("flogBase", "inc"), mkTemplate [I.i|
        // floor of logBase
        function integer ~INCLUDENAME[0];
          input integer base, value;
          begin
            for (~INCLUDENAME[0] = 0; value >= base; ~INCLUDENAME[0]=~INCLUDENAME[0]+1)
              value = value / base;
          end
        endfunction
      |])
    , template="~INCLUDENAME[0](~LIT[2],~LIT[3])"
    }
  -- :: (2 <= base, 1 <= x) => SNat base -> SNat x -> SNat (CLog base x)
  , (blackbox 'P.clogBaseSNat){
      workInfo=Never
    , imports=["~INCLUDENAME[0].inc"]
    , includes=(("clogBase", "inc"), mkTemplate [I.i|
        // ceiling of logBase
        function integer ~INCLUDENAME[0];
          input integer base, value;
          begin
            for (~INCLUDENAME[0] = 0; base ** ~INCLUDENAME[0] < value; ~~INCLUDENAME[0]=~INCLUDENAME[0]+1);
          end
        endfunction
    |]
    , template="~INCLUDENAME[0](~LIT[2],~LIT[3])"
    }
  -- :: (FLog base x ~ CLog base x) => SNat base -> SNat x -> SNat (Log base x)
  , (blackbox 'P.logBaseSNat){
      workInfo=Never
    , imports=["~INCLUDENAME[0].inc"]
    , includes=(("logBase", "inc"), mkTemplate [I.i|
        // logBaseSNat begin
        function integer ~INCLUDENAME[0];
          input integer base, value;
          begin
            for (~INCLUDENAME[0] = 0; value >= base; ~INCLUDENAME[0]=~INCLUDENAME[0]+1)
              value = value / base;
          end
        endfunction
        // logBaseSNat end
    |]
    , template="~INCLUDENAME[0](~LIT[1],~LIT[2])"
    }
  ]