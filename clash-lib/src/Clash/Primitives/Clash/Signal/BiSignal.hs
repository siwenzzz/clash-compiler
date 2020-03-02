module Clash.Primitives.Clash.Signal.BiSignal where

import qualified Clash.Signal.BiSignal as P

commonPrimitives =
  [ (blackbox 'P.veryUnsafeToBiSignalIn){
      workInfo=Never
    , kind=Declaration
    , template="~DEVNULL[~ARG[3]]"
    }
  , (blackbox 'P.mergeBiSignalOuts){
      workInfo=Never
    , template="~DEVNULL[~ARG[2]]"
    }
  ]

systemverilogPrimitives =
  [ -- writeToBiSignal#
    --  :: HasCallStack                   -- ARG[0]
    --  => BiSignalIn ds d n              -- ARG[1]
    --  -> Signal d (Maybe (BitVector n)) -- ARG[2]
    --  -> Signal d Bool                  -- ARG[3]
    --  -> Signal d (BitVector n)         -- ARG[4]
    --  -> BiSignalOut ds d n
    (blackbox 'P.writeToBiSignal#){
      kind=Declaration
    , renderVoid=RenderVoid
    , template=[I.i|
        // writeToBiSignal# begin
        assign ~ARG[1] = (~ARG[3] == 1'b1) ? ~ARG[4] : {~SIZE[~TYP[1]] {1'bz}};
        // writeToBiSignal# end
      |]
    }
    -- readFromBiSignal#
    --  :: ( HasCallStack    -- ARG[0]
    --     , KnownNat n)     -- ARG[1]
    --  => BiSignalIn ds d n -- ARG[2]
    --  -> Signal d (BitVector n)
  , (blackbox 'P.writeToBiSignal#){
      kind=Declaration
    , workInfo=Never
    , template=[I.i|
        // readFromBiSignal begin
        assign ~RESULT = ~ARG[2];
        // readFromBiSignal end
      |]
    }
  ]