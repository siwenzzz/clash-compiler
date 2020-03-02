module Clash.Primitives.Clash.Magic where

import qualified Clash.Magic as P

commontPrimitives =
  [ (primitive 'P.prefixName){primSort="Function"}
  , (primitive 'P.suffixName){primSort="Function"}
  , (primitive 'P.suffixNameFromNat){primSort="Function"}
  , (primitive 'P.suffixNameP){primSort="Function"}
  , (primitive 'P.suffixNameFromNatP){primSort="Function"}
  , (primitive 'P.setName){primSort="Function"}
  ]
