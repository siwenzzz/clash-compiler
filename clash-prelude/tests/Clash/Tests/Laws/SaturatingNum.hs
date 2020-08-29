{-# LANGUAGE RankNTypes #-}

module Clash.Tests.Laws.SaturatingNum (tests) where

import Data.Proxy
import Test.Tasty
import Test.Tasty.HUnit
import Test.Tasty.QuickCheck

import Clash.Class.Num
import Clash.Sized.Index (Index)
import Clash.Sized.Signed (Signed)
import Clash.Sized.Fixed (SFixed, UFixed)
import Clash.Sized.Unsigned (Unsigned)

type TestWrap = Bool

type SaturationLaw a =
  (Arbitrary a, Ord a, Show a, Eq a, SaturatingNum a) =>
  Proxy a ->
  Property

isTotal :: forall a. (SaturationMode -> a -> a -> a) -> SaturationLaw a
isTotal f Proxy = property $ do
  satMode <- elements [SatWrap, SatBound, SatZero, SatSymmetric]
  a <- arbitrary @a
  b <- arbitrary @a
  let result = f satMode a b
  pure (result === result)

satWrapOverflowLaw :: forall a. SaturationLaw a
satWrapOverflowLaw Proxy = satSucc @a SatWrap maxBound === minBound

satWrapUnderflowLaw :: forall a. SaturationLaw a
satWrapUnderflowLaw Proxy = satPred @a SatWrap minBound === maxBound

satBoundOverflowLaw :: forall a. SaturationLaw a
satBoundOverflowLaw Proxy = satSucc @a SatBound maxBound === maxBound

satBoundUnderflowLaw :: forall a. SaturationLaw a
satBoundUnderflowLaw Proxy = satPred @a SatBound minBound === minBound

satZeroOverflowLaw :: forall a. SaturationLaw a
satZeroOverflowLaw Proxy = satSucc @a SatZero maxBound === 0

satZeroUnderflowLaw :: forall a. SaturationLaw a
satZeroUnderflowLaw Proxy = satPred @a SatZero minBound === 0

satSymmetricOverflow :: forall a. SaturationLaw a
satSymmetricOverflow Proxy = satSucc @a SatSymmetric maxBound === maxBound

satSymmetricUnderflow :: forall a. SaturationLaw a
satSymmetricUnderflow Proxy =
  if minBound @a < 0 then
    -- Signed number
    satPred @a SatSymmetric minBound === satSucc SatWrap minBound
  else
    -- Unsigned number (or zero-width)
    satPred @a SatSymmetric minBound === minBound

saturatingNumLaws ::
  (Arbitrary a, Ord a, Show a, Eq a, SaturatingNum a) =>
  TestWrap ->
  Proxy a ->
  [(String, Property)]
saturatingNumLaws testWrap proxy =
  (if testWrap then
    [ ("SatWrap: Wrap around on overflow", satWrapOverflowLaw proxy)
    , ("SatWrap: Wrap around on underflow", satWrapUnderflowLaw proxy) ]
  else
    []) <>
  [ ("SatBound: Become maxBound on overflow", satBoundOverflowLaw proxy)
  , ("SatBound: Become minBound on underflow", satBoundUnderflowLaw proxy)

  , ("SatZero: Become 0 on overflow", satZeroOverflowLaw proxy)
  , ("SatZero: Become 0 on underflow", satZeroUnderflowLaw proxy)

  , ("SatSymmetric: Become maxBound on overflow", satSymmetricOverflow proxy)
  , ("SatSymmetric: Become minBound or minBound+1 on underflow", satSymmetricUnderflow proxy)

  , ("satAddTotal", isTotal satAdd proxy)
  , ("satSubTotal", isTotal satSub proxy)
  , ("satMulTotal", isTotal satMul proxy)
  ]

testSaturationLaws ::
  (Arbitrary a, Ord a, Show a, Eq a, SaturatingNum a) =>
  TestWrap ->
  String ->
  Proxy a ->
  TestTree
testSaturationLaws testWrap typeName proxy =
  testGroup
    typeName
    (map (uncurry testProperty) (saturatingNumLaws testWrap proxy))

-- AFAIK there's no good way to override the default, so we just detect the
-- default value and change it.
setDefaultQuickCheckTests :: QuickCheckTests -> QuickCheckTests
setDefaultQuickCheckTests (QuickCheckTests 100) = 10000
setDefaultQuickCheckTests opt = opt

tests :: TestTree
tests = adjustOption setDefaultQuickCheckTests $ testGroup "SaturatingNum"
  [ testSaturationLaws True "Index 1" (Proxy @(Index 1))
  , testSaturationLaws True "Index 2" (Proxy @(Index 2))
  , testSaturationLaws True "Index 128" (Proxy @(Index 128))

  , testSaturationLaws True "Unsigned 0" (Proxy @(Unsigned 0))
  , testSaturationLaws True "Unsigned 1" (Proxy @(Unsigned 1))
  , testSaturationLaws True "Unsigned 32" (Proxy @(Unsigned 32))
  , testSaturationLaws True "Unsigned 127" (Proxy @(Unsigned 127))
  , testSaturationLaws True "Unsigned 128" (Proxy @(Unsigned 128))

  , testSaturationLaws True "Signed 0" (Proxy @(Signed 0))
  , testSaturationLaws True "Signed 1" (Proxy @(Signed 1))
  , testSaturationLaws True "Signed 32" (Proxy @(Signed 32))
  , testSaturationLaws True "Signed 127" (Proxy @(Signed 127))
  , testSaturationLaws True "Signed 128" (Proxy @(Signed 128))

  , testSaturationLaws False "SFixed 0 0" (Proxy @(SFixed 0 0))
  , testSaturationLaws False "SFixed 0 1" (Proxy @(SFixed 0 1))
  , testSaturationLaws False "SFixed 1 0" (Proxy @(SFixed 1 0))
  , testSaturationLaws False "SFixed 1 1" (Proxy @(SFixed 1 1))
  , testSaturationLaws False "SFixed 1 2" (Proxy @(SFixed 1 2))
  , testSaturationLaws False "SFixed 2 1" (Proxy @(SFixed 2 1))
  , testSaturationLaws False "SFixed 2 2" (Proxy @(SFixed 2 2))
  , testSaturationLaws False "SFixed 128 128" (Proxy @(SFixed 128 128))

  , testSaturationLaws False "UFixed 0 0" (Proxy @(UFixed 0 0))
  , testSaturationLaws False "UFixed 0 1" (Proxy @(UFixed 0 1))
  , testSaturationLaws False "UFixed 1 0" (Proxy @(UFixed 1 0))
  , testSaturationLaws False "UFixed 1 1" (Proxy @(UFixed 1 1))
  , testSaturationLaws False "UFixed 1 2" (Proxy @(UFixed 1 2))
  , testSaturationLaws False "UFixed 2 1" (Proxy @(UFixed 2 1))
  , testSaturationLaws False "UFixed 2 2" (Proxy @(UFixed 2 2))
  , testSaturationLaws False "UFixed 128 128" (Proxy @(UFixed 128 128))

  , testGroup "Manual"
    [ testCase "Index 10: succ 3 == 4" (succ @(Index 10) 3 @?= 4)
    , testProperty "Unsigned 10: satAdd SatWrap a b == satAdd SatWrap b a" $ \a b ->
        satAdd SatWrap a b === satAdd SatWrap b (a :: Unsigned 10)
    ]
  ]
