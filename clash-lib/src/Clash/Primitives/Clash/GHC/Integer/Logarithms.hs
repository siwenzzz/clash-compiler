module Clash.Primitives.GHC.Integer.Logarithms where

import qualified GHC.Integer.Logarithms as P

commonVerilogPrimitives =
  [ (blackbox 'P.integerLogBase#){
      imports=["~INCLUDENAME[0].inc"]
    , includes=[(("integerLogBase", "inc"), mkTemplate [i|
        // integer logBase
        function integer ~INCLUDENAME[0];
          input integer base, value;
          begin
            for (~INCLUDENAME[0] = 0; value >= base; ~INCLUDENAME[0]=~INCLUDENAME[0]+1)
              value = value / base;
          end
        endfunction
      |])]
    , template="~INCLUDENAME[0](~ARG[0],~ARG[1])"
    }
  ]
