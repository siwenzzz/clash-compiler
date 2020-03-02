module Clash.Primitives.GHC.Err where

import qualified GHC.Err as P

commonPrimitives =
  [ (blackbox 'P.error){template="~ERROR0", workInfo=Constant}
  , (blackbox 'P.errorWithoutStackTrace){template="~ERROR0", workInfo=Constant}
  , (blackbox 'P.undefined){template="~ERROR0", workInfo=Constant}
  ]
