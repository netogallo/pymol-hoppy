{-# LANGUAGE MonoLocalBinds, TemplateHaskell, ViewPatterns #-}
module Data.Pymol.Extra where

import           Foreign (IntPtr(..), Ptr, intPtrToPtr)
import           Foreign.Hoppy.Abstractions.Record
import           Foreign.Hoppy.Runtime (CppPtr)
import           Foreign.Pymol.Layer2.AtomInfo (classAtomInfo)
import           Foreign.Storable (Storable(peek))
import           Data.MonadicStreamFunction.Core (constM)
import           Data.Pymol.Layer3.AtomIterators (SeleAtomIterator(..), getAtm, next)
import           Data.Pymol.Layer2.AtomInfo (AtomInfoType, AtomInfoTypeValue, anisou_get)

$(asRecord classAtomInfo)
-- foreign export ccall test :: IntPtr -> IO Int

instance Show AtomInfoTypeRec

value :: Float -> AtomInfoTypeRec
value = AtomInfoTypeRec

value' :: (AtomInfoTypeValue arg'1) => arg'1 -> IO AtomInfoTypeRec
value' = convert_AtomInfoTypeRec

test :: IntPtr -> IO Int
test (SeleAtomIterator . intPtrToPtr -> it) = do
--  it <- atomIterator
  next it
  getAtm it

