module Clash.Primitives.Clash.Signal.Internal where

import qualified Clash.Signal.Internal as P

commonPrimitives =
  [ (primitive 'P.signal#){primSort="Function", workInfo=Never}
  , (primitive 'P.mapSignal#){primSort="Function", workInfo=Never}
  , (primitive 'P.appSignal#){primSort="Function", workInfo=Never}
  , (primitive 'P.foldr#){primSort="Function", workInfo=Never}
  , (primitive 'P.traverse#){primSort="Function", workInfo=Never}
  , (primitive 'P.joinSignal#){primSort="Function", workInfo=Never}
  ]

commonVerilogPrimitives =
  -- TODO: Workinfo?
  [ (blackbox 'P.unsafeFromReset){workInfo=Never, template="~ARG[0]"}
  , (blackbox 'P.unsafeToReset){workInfo=Never, template="~ARG[0]"}
  , (blackbox 'P.tbEnableGen){
      workInfo=Always
    , kind=Declaration
    , template="assign ~RESULT = 1'b1;"
    }
  ]
