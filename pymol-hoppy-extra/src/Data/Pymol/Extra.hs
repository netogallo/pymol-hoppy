{-# LANGUAGE TemplateHaskell, ViewPatterns #-}
module Data.Pymol.Extra where

import           Foreign (IntPtr(..), Ptr, intPtrToPtr)
import           Foreign.Storable (Storable(peek))
import           Data.MonadicStreamFunction.Core (constM)
import           Data.Pymol.Layer3.AtomIterators (SeleAtomIterator(..), getAtm, next)

import           Data.Pymol.Extra.TemplateHaskell (simple)

$(simple ''SeleAtomIterator)

foreign export ccall test :: IntPtr -> IO Int

value :: SeleAtomIterator -> Kaiser
value = Kaiser

test :: IntPtr -> IO Int
test (SeleAtomIterator . intPtrToPtr -> it) = do
--  it <- atomIterator
  next it
  getAtm it

