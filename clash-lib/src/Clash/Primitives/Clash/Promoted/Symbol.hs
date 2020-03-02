module Clash.Primitives.Clash.Promoted.Symbol where

import qualified Clash.Promoted.Symbol as P

commontPrimitives =
  [ (blackbox 'P.SSymbol){workInfo=Never, template="~LIT[0]"}
  , (blackbox 'P.symbolToString){workInfo=Never, template="~LIT[0]"}
  ]