module Clash.Primitives.Clash.XException.Base where

import qualified Clash.XException.Base as P

commontPrimitives =
  [ (blackbox 'P.recSelError){template="~ERROR0", workInfo=Constant}
  , (blackbox 'P.recConError){template="~ERROR0", workInfo=Constant}
  , (blackbox 'P.irrefutPatError){template="~ERROR0", workInfo=Constant}
  , (blackbox 'P.runTimeError){template="~ERROR0", workInfo=Constant}
  , (blackbox 'P.nonExhaustiveGuardsError){template="~ERROR0", workInfo=Constant}
  , (blackbox 'P.patError){template="~ERROR0", workInfo=Constant}
  , (blackbox 'P.noMethodBindingError){template="~ERROR0", workInfo=Constant}
  , (blackbox 'P.absentError){template="~ERROR0", workInfo=Constant}
  , (blackbox 'P.typeError){template="~ERROR0", workInfo=Constant}
  ]
