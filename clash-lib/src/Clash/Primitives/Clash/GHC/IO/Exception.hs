module Clash.Primitives.GHC.IO.Exception where

import qualified GHC.IO.Exception as P

commonPrimitives =
  [ -- It would be nice if we could use a HDL assertion, however, because in
    -- HDL, case alternatives are evaluated concurrently, we would end up
    -- with the assertion being triggered, even when the result of that branch
    -- is not chosen in the multiplexer
    (blackbox 'P.assertError){template="~ARG[2]"}
  ]
