{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

-- {-# OPTIONS_GHC -ddump-splices #-}

module PortNames where

import qualified Prelude as P

import Clash.Prelude
import Clash.Netlist.Types
import qualified Clash.Netlist.Types as N
import Clash.Annotations.TH

import Clash.Class.HasDomain

import Test.Tasty.Clash
import Test.Tasty.Clash.NetlistTest

import qualified Data.Text as T

data SevenSegment dom (n :: Nat) = SevenSegment
    { anodes :: "AN" ::: Signal dom (Vec n Bool)
    , segments :: "SEG" ::: Signal dom (Vec 7 Bool)
    , dp :: "DP" ::: Signal dom Bool
    }

type instance TryDomain t (SevenSegment dom n) = Found dom

topEntity
    :: "CLK" ::: Clock System
    -> "SS" ::: SevenSegment System 8
topEntity clk = withClockResetEnable clk resetGen enableGen $
    SevenSegment{ anodes = pure $ repeat False
                , segments = pure $ repeat False
                , dp = pure False }
makeTopEntity 'topEntity

testPath :: FilePath
testPath = "tests/shouldwork/TopEntity/T1182B.hs"

assertInputs :: HWType-> HWType -> Component -> IO ()
assertInputs exp1 exp2 (Component _ [(clk, Clock _)]
  [ (Wire, (ssan, act1), Nothing)
  , (Wire, (ssseg, act2), Nothing)
  , (Wire, (ssdp, Bool), Nothing)
  ] ds)
  | exp1 == act1
  , exp2 == act2
  , clk == T.pack "CLK"
  , ssan == T.pack "SS_AN"
  , ssseg == T.pack "SS_SEG"
  , ssdp == T.pack "SS_DP"
  = pure ()
assertInputs _ _ c = error $ "Component mismatch: " P.++ show c

getComponent :: (a, b, c, d) -> d
getComponent (_, _, _, x) = x

mainVHDL :: IO ()
mainVHDL = do
  netlist <- runToNetlistStage SVHDL id testPath
  mapM_ (assertInputs (N.BitVector 8) (N.BitVector 7) . getComponent) netlist

mainVerilog :: IO ()
mainVerilog = do
  netlist <- runToNetlistStage SVerilog id testPath
  mapM_ (assertInputs (N.Vector 8 N.Bool) (N.Vector 7 N.Bool) . getComponent) netlist

mainSystemVerilog :: IO ()
mainSystemVerilog = do
  netlist <- runToNetlistStage SSystemVerilog id testPath
  mapM_ (assertInputs (N.BitVector 8) (N.BitVector 7) . getComponent) netlist

