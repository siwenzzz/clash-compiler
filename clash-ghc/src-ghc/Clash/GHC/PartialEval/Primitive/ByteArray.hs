{-# LANGUAGE MagicHash #-}
{-# LANGUAGE OverloadedStrings #-}

module Clash.GHC.PartialEval.Primitive.ByteArray
  ( byteArrayPrims
  ) where

import Control.Monad (when)
import Control.Monad.Catch (throwM)
import Control.Monad.IO.Class
import Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as HashMap (fromList)
import Data.Maybe (fromJust)
import Data.Primitive.ByteArray as BA
import Data.Primitive.Types (Prim)
import Data.Text (Text)

import Clash.Core.Literal (Literal(..))
import Clash.Core.PartialEval.Monad
import Clash.Core.PartialEval.NormalForm

import Clash.GHC.PartialEval.Primitive.FromAst
import Clash.GHC.PartialEval.Primitive.Info
import Clash.GHC.PartialEval.Primitive.Strategy
import Clash.GHC.PartialEval.Primitive.ToAst
import Clash.GHC.PartialEval.Primitive.Unboxed

byteArrayPrims :: HashMap Text PrimImpl
byteArrayPrims = HashMap.fromList
  [ ("GHC.Prim.newByteArray#", primNewByteArray)
  , ("GHC.Prim.shrinkMutableByteArray#", primShrinkMutableByteArray)
  , ("GHC.Prim.resizeMutableByteArray#", primResizeMutableByteArray)
  , ("GHC.Prim.unsafeFreezeByteArray#", primUnsafeFreezeByteArray)
  , ("GHC.Prim.sizeofByteArray#", primSizeofByteArray)
  , ("GHC.Prim.getSizeofMutableByteArray#", primGetSizeofMutableByteArray)
  , ("GHC.Prim.indexCharArray#", primIndex UChar)
  , ("GHC.Prim.indexIntArray#", primIndex UInt)
  , ("GHC.Prim.indexWordArray#", primIndex UWord)
  , ("GHC.Prim.indexFloatArray#", primIndex UFloat)
  , ("GHC.Prim.indexDoubleArray#", primIndex UDouble)
  , ("GHC.Prim.readCharArray#", primRead UChar)
  , ("GHC.Prim.readIntArray#", primRead UInt)
  , ("GHC.Prim.readWordArray#", primRead UWord)
  , ("GHC.Prim.readFloatArray#", primRead UFloat)
  , ("GHC.Prim.readDoubleArray#", primRead UDouble)
  , ("GHC.Prim.writeCharArray#", primWrite boxChar)
  , ("GHC.Prim.writeIntArray#", primWrite boxInt)
  , ("GHC.Prim.writeWordArray#", primWrite boxWord)
  , ("GHC.Prim.writeFloatArray#", primWrite boxFloat)
  , ("GHC.Prim.writeDoubleArray#", primWrite boxDouble)
  , ("GHC.Prim.copyByteArray#", primCopyByteArray)
  , ("GHC.Prim.setByteArray#", primSetByteArray)
  ]

{-
NOTE [byteArray primitives]
~~~~~~~~~~~~~~~~~~~~~~~~~~~
In ghc-prim, some primitives are on MutableByteArray# instead of ByteArray#.
As there is currently no support for mutable byte array literals in clash,
these arguments are taken to be a reference to a ByteArray#. When implementing
new primtives on mutable byte arrays, it is therefore necessary to read the
byte array argument as a reference, and use unsafeThawByteArray to obtain a
mutable byte array.
-}

primNewByteArray :: PrimImpl
primNewByteArray e p args
  | [Right _s, Left x, Left y] <- args
  = do size  <- boxInt <$> fromValueForce e x
       resTy <- resultType p args

       ba <- liftIO $ do
         mba <- BA.newByteArray size
         BA.unsafeFreezeByteArray mba

       toValue (UTuple2 (y, Ref Nothing (UByteArray ba))) resTy

  | otherwise
  = empty

primResizeMutableByteArray :: PrimImpl
primResizeMutableByteArray eval pr args
  | [Right _s, Left x, Left y, Left rw] <- args
  = do !ref <- fromValueForce @(Ref UByteArray) eval x
       !len <- boxInt <$> fromValueForce eval y
       resTy <- resultType pr args

       ba <- liftIO $ do
         mba  <- BA.unsafeThawByteArray (boxByteArray (refValue ref))
         mba' <- BA.resizeMutableByteArray mba len
         BA.unsafeFreezeByteArray mba'

       let ref' = ref { refValue = VLiteral (ByteArrayLiteral ba) }
       toValue (UTuple2 (rw, ref')) resTy

  | otherwise
  = empty

primShrinkMutableByteArray :: PrimImpl
primShrinkMutableByteArray eval _pr args
  | [Right _s, Left x, Left y, Left rw] <- args
  = do !ref <- fromValueForce @(Ref UByteArray) eval x
       !len <- boxInt <$> fromValueForce eval y

       mba <- liftIO $ BA.unsafeThawByteArray (boxByteArray (refValue ref))

       -- Attempting to shrink to a larger value is undefined.
       case compare len (BA.sizeofMutableByteArray mba) of
         GT -> throwM ArgUndefined

         _  -> do
           liftIO (BA.shrinkMutableByteArray mba len)
           ba <- liftIO (BA.unsafeFreezeByteArray mba)

           setRef (fromJust (refAddr ref)) (VLiteral (ByteArrayLiteral ba))
           pure rw

  | otherwise
  = empty

primUnsafeFreezeByteArray :: PrimImpl
primUnsafeFreezeByteArray e p args
  | [Right _s, Left x, Left y] <- args
  = do !ba <- fromValueForce @(Ref UByteArray) e x
       resTy <- resultType p args

       toValue (UTuple2 (y, ba)) resTy

  | otherwise
  = empty

primSizeofByteArray :: PrimImpl
primSizeofByteArray eval pr args
  | [Left x] <- args
  = do !ba <- boxByteArray . refValue <$> fromValueForce eval x
       resTy <- resultType pr args

       toValue (UInt (BA.sizeofByteArray ba)) resTy

  | otherwise
  = empty

primGetSizeofMutableByteArray :: PrimImpl
primGetSizeofMutableByteArray eval pr args
  | [Right _s, Left x, Left rw] <- args
  = do !ba <- boxByteArray . refValue <$> fromValueForce eval x
       resTy <- resultType pr args

       size <- liftIO $ do
         mba <- BA.unsafeThawByteArray ba
         BA.getSizeofMutableByteArray mba

       toValue (UTuple2 (rw, UInt size)) resTy

  | otherwise
  = empty

primCopyByteArray :: PrimImpl
primCopyByteArray eval _pr args
  | [Right _s, Left x, Left xO, Left y, Left yO, Left n, Left rw] <- args
  = do srcRef <- fromValueForce @(Ref UByteArray) eval x
       srcOff <- boxInt <$> fromValueForce eval xO
       dstRef <- fromValueForce @(Ref UByteArray) eval y
       dstOff <- boxInt <$> fromValueForce eval yO
       len <- boxInt <$> fromValueForce eval n

       -- Undefined if the src and dst are the same.
       when (refAddr srcRef == refAddr dstRef) (throwM ArgUndefined)

       -- Undefined if offset + length are out of bounds for either array.
       let src = boxByteArray (refValue srcRef)
           dst = boxByteArray (refValue dstRef)

       when (BA.sizeofByteArray src <= srcOff + len) (throwM ArgUndefined)
       when (BA.sizeofByteArray dst <= dstOff + len) (throwM ArgUndefined)

       -- Otherwise, copy from src into dst.
       dst' <- liftIO $ do
         mba <- BA.unsafeThawByteArray dst
         BA.copyByteArray mba dstOff src srcOff len
         BA.unsafeFreezeByteArray mba

       setRef (fromJust (refAddr dstRef)) (VLiteral (ByteArrayLiteral dst'))
       pure rw

  | otherwise
  = empty

primSetByteArray :: PrimImpl
primSetByteArray eval _pr args
  | [Right _s, Left b, Left x, Left y, Left z, Left rw] <- args
  = do !ref <- fromValueForce @(Ref UByteArray) eval b
       !off <- boxInt <$> fromValueForce eval x
       !len <- boxInt <$> fromValueForce eval y
       !c <- boxInt <$> fromValueForce eval z

       ba <- liftIO $ do
         mba <- BA.unsafeThawByteArray (boxByteArray (refValue ref))
         BA.setByteArray mba off len c
         BA.unsafeFreezeByteArray mba

       -- The return type of setByteArray# is just State# s, so we need to
       -- update the ref here before returning that.
       --
       -- fromJust is safe here, we could only get this ref becuase it already
       -- existed in the heap.
       --
       setRef (fromJust (refAddr ref)) (VLiteral (ByteArrayLiteral ba))
       pure rw

  | otherwise
  = empty

primIndex :: (Prim a, ToAst b) => (a -> b) -> PrimImpl
primIndex f eval pr args
  | [Left x, Left y] <- args
  = do !ba <- boxByteArray . refValue <$> fromValueForce eval x
       !ix <- boxInt <$> fromValueForce eval y
       resTy <- resultType pr args

       toValue (f (BA.indexByteArray ba ix)) resTy

  | otherwise
  = empty

primRead :: (Prim a, ToAst b) => (a -> b) -> PrimImpl
primRead f eval pr args
  | [Right _s, Left x, Left y, Left rw] <- args
  = do !ba <- boxByteArray . refValue <$> fromValueForce eval x
       !ix <- boxInt <$> fromValueForce eval y
       resTy <- resultType pr args

       res <- liftIO $ do
         mba <- BA.unsafeThawByteArray ba
         BA.readByteArray mba ix

       toValue (UTuple2 (rw, f res)) resTy

  | otherwise
  = empty

primWrite :: (FromAst a, Prim b) => (a -> b) -> PrimImpl
primWrite f eval _pr args
  | [Right _s, Left x, Left y, Left z, Left rw] <- args
  = do !ref <- fromValueForce @(Ref UByteArray) eval x
       !ix  <- boxInt <$> fromValueForce eval y
       !val <- f <$> fromValueForce eval z

       !ba <- liftIO $ do
         mba <- BA.unsafeThawByteArray (boxByteArray (refValue ref))
         BA.writeByteArray mba ix val
         BA.unsafeFreezeByteArray mba

       setRef (fromJust (refAddr ref)) (VLiteral (ByteArrayLiteral ba))
       pure rw

  | otherwise
  = empty
