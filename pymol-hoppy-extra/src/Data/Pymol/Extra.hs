{-# LANGUAGE ViewPatterns #-}
module Data.Pymol.Extra where

import Foreign (IntPtr(..), Ptr, intPtrToPtr)
import Foreign.Storable (Storable(peek))
import Data.MonadicStreamFunction.Core (constM)
import Data.Pymol.Layer3.AtomIterators (SeleAtomIterator(..), getAtm, next)

foreign export ccall test :: IntPtr -> IO Int

-- test :: IntPtr -> IO Int
-- test = testTy . intPtrToPtr

test :: IntPtr -> IO Int
test (SeleAtomIterator . intPtrToPtr -> it) = do
--  it <- atomIterator
  next it
  getAtm it
