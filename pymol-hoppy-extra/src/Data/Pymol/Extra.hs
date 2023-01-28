{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module Data.Pymol.Extra where

import           Foreign (IntPtr(..), Ptr, intPtrToPtr)
import           Foreign.Hoppy.Abstractions.Record (asRecord)
import           Foreign.Hoppy.Runtime (CppPtr)
import           Foreign.Pymol.Layer2.AtomInfo (classAtomInfo)
import           Foreign.Storable (Storable(peek))
import           Data.MonadicStreamFunction.Core (constM)
import           Data.Pymol.Layer3.AtomIterators (SeleAtomIterator(..), getAtm, next)
import           Data.Pymol.Layer2.AtomInfo (anisou_get)
import Data.Pymol.Layer2.AtomInfo (AtomInfoType)

$(asRecord classAtomInfo)

foreign export ccall test :: IntPtr -> IO Int

value :: Float -> AtomInfoTypeRec
value = AtomInfoTypeRec

test :: IntPtr -> IO Int
test (SeleAtomIterator . intPtrToPtr -> it) = do
--  it <- atomIterator
  next it
  getAtm it

