module Catamorphile (test) where

import Foreign (IntPtr(..), Ptr, intPtrToPtr)
import Foreign.Pymol.AtomIterators (SeleAtomIterator)

foreign export ccall test :: IntPtr -> IO ()

test :: IntPtr -> IO ()
test = testTy . intPtrToPtr

testTy :: Ptr SeleAtomIterator -> IO ()
testTy _ = do
  print "haskell"
  pure ()
