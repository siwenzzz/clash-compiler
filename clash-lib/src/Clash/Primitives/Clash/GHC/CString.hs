module Clash.Primitives.GHC.CString where

import qualified GHC.CString as P

commonPrimitives =
  [ (blackbox 'P.unpackCString#){template="~LIT[0]", workInfo=Never}
  , (blackbox 'P.unpackFoldrCString#){template="~LIT[0]", workInfo=Never}
  , (primitive 'P.unpackCStringUtf8#){workInfo=Never, primSort="Function"}
  ]
