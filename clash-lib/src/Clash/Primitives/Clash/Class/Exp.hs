module Clash.Primitives.Clash.Class.Exp where

import qualified Clash.Class.Exp as P

commonVerilogPrimitives =
  [ -- "warning": "Exponentiation is only supported on relatively small constructs (< 32 bits). Ideally, Clash should have constant folded your expression. See https://github.com/clash-lang/clash-compiler/issues/593."
    (blackbox 'P.expIndex#){
      kind=Declaration
    , template="assign ~RESULT = ~DEVNULL[~ARG[0]]$signed(~ARG[1] ** ~LIT[2]);"
    }
  , (blackbox 'P.expSigned#){
      kind=Declaration
    , template="assign ~RESULT = ~DEVNULL[~ARG[0]]$signed(~ARG[1] ** ~LIT[2]);"
    }
  , (blackbox 'P.expUnsigned#){
      kind=Declaration
    , template="assign ~RESULT = ~DEVNULL[~ARG[0]]$unsigned(~ARG[1] ** ~LIT[2]);"
    }
  ]