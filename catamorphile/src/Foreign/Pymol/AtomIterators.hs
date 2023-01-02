{-# LANGUAGE FlexibleContexts, FlexibleInstances, ForeignFunctionInterface, MonoLocalBinds, MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances, UndecidableInstances #-}
---------- GENERATED FILE, EDITS WILL BE LOST ----------

module Foreign.Pymol.AtomIterators (
  SeleAtomIteratorValue (..),
  SeleAtomIteratorConstPtr (..),
  SeleAtomIteratorPtr (..),
  SeleAtomIteratorConst (..),
  castSeleAtomIteratorToConst,
  SeleAtomIterator (..),
  castSeleAtomIteratorToNonconst,
  SeleAtomIteratorSuper (..),
  SeleAtomIteratorSuperConst (..),
  ) where

import qualified Foreign as HoppyF
import qualified Foreign.Hoppy.Runtime as HoppyFHR
import Prelude (($), (.), (==))
import qualified Prelude as HoppyP

foreign import ccall "gendel__SeleAtomIterator" delete'SeleAtomIterator :: HoppyF.Ptr SeleAtomIteratorConst -> HoppyP.IO ()
foreign import ccall "&gendel__SeleAtomIterator" deletePtr'SeleAtomIterator :: HoppyF.FunPtr (HoppyF.Ptr SeleAtomIteratorConst -> HoppyP.IO ())

class SeleAtomIteratorValue a where
  withSeleAtomIteratorPtr :: a -> (SeleAtomIteratorConst -> HoppyP.IO b) -> HoppyP.IO b

instance {-# OVERLAPPABLE #-} SeleAtomIteratorConstPtr a => SeleAtomIteratorValue a where
  withSeleAtomIteratorPtr = HoppyP.flip ($) . toSeleAtomIteratorConst

class (HoppyFHR.CppPtr this) => SeleAtomIteratorConstPtr this where
  toSeleAtomIteratorConst :: this -> SeleAtomIteratorConst

class (SeleAtomIteratorConstPtr this) => SeleAtomIteratorPtr this where
  toSeleAtomIterator :: this -> SeleAtomIterator

data SeleAtomIteratorConst =
    SeleAtomIteratorConst (HoppyF.Ptr SeleAtomIteratorConst)
  | SeleAtomIteratorConstGc (HoppyF.ForeignPtr ()) (HoppyF.Ptr SeleAtomIteratorConst)
  deriving (HoppyP.Show)

instance HoppyP.Eq SeleAtomIteratorConst where
  x == y = HoppyFHR.toPtr x == HoppyFHR.toPtr y

instance HoppyP.Ord SeleAtomIteratorConst where
  compare x y = HoppyP.compare (HoppyFHR.toPtr x) (HoppyFHR.toPtr y)

castSeleAtomIteratorToConst :: SeleAtomIterator -> SeleAtomIteratorConst
castSeleAtomIteratorToConst (SeleAtomIterator ptr') = SeleAtomIteratorConst $ HoppyF.castPtr ptr'
castSeleAtomIteratorToConst (SeleAtomIteratorGc fptr' ptr') = SeleAtomIteratorConstGc fptr' $ HoppyF.castPtr ptr'

instance HoppyFHR.CppPtr SeleAtomIteratorConst where
  nullptr = SeleAtomIteratorConst HoppyF.nullPtr
  
  withCppPtr (SeleAtomIteratorConst ptr') f' = f' ptr'
  withCppPtr (SeleAtomIteratorConstGc fptr' ptr') f' = HoppyF.withForeignPtr fptr' $ \_ -> f' ptr'
  
  toPtr (SeleAtomIteratorConst ptr') = ptr'
  toPtr (SeleAtomIteratorConstGc _ ptr') = ptr'
  
  touchCppPtr (SeleAtomIteratorConst _) = HoppyP.return ()
  touchCppPtr (SeleAtomIteratorConstGc fptr' _) = HoppyF.touchForeignPtr fptr'

instance HoppyFHR.Deletable SeleAtomIteratorConst where
  delete (SeleAtomIteratorConst ptr') = delete'SeleAtomIterator ptr'
  delete (SeleAtomIteratorConstGc _ _) = HoppyP.fail $ HoppyP.concat ["Deletable.delete: Asked to delete a GC-managed ", "SeleAtomIteratorConst", " object."]
  
  toGc this'@(SeleAtomIteratorConst ptr') = if ptr' == HoppyF.nullPtr then HoppyP.return this' else HoppyP.fmap (HoppyP.flip SeleAtomIteratorConstGc ptr') $ HoppyF.newForeignPtr (HoppyF.castFunPtr deletePtr'SeleAtomIterator :: HoppyF.FunPtr (HoppyF.Ptr () -> HoppyP.IO ())) (HoppyF.castPtr ptr' :: HoppyF.Ptr ())
  toGc this'@(SeleAtomIteratorConstGc {}) = HoppyP.return this'

instance SeleAtomIteratorConstPtr SeleAtomIteratorConst where
  toSeleAtomIteratorConst = HoppyP.id

data SeleAtomIterator =
    SeleAtomIterator (HoppyF.Ptr SeleAtomIterator)
  | SeleAtomIteratorGc (HoppyF.ForeignPtr ()) (HoppyF.Ptr SeleAtomIterator)
  deriving (HoppyP.Show)

instance HoppyP.Eq SeleAtomIterator where
  x == y = HoppyFHR.toPtr x == HoppyFHR.toPtr y

instance HoppyP.Ord SeleAtomIterator where
  compare x y = HoppyP.compare (HoppyFHR.toPtr x) (HoppyFHR.toPtr y)

castSeleAtomIteratorToNonconst :: SeleAtomIteratorConst -> SeleAtomIterator
castSeleAtomIteratorToNonconst (SeleAtomIteratorConst ptr') = SeleAtomIterator $ HoppyF.castPtr ptr'
castSeleAtomIteratorToNonconst (SeleAtomIteratorConstGc fptr' ptr') = SeleAtomIteratorGc fptr' $ HoppyF.castPtr ptr'

instance HoppyFHR.CppPtr SeleAtomIterator where
  nullptr = SeleAtomIterator HoppyF.nullPtr
  
  withCppPtr (SeleAtomIterator ptr') f' = f' ptr'
  withCppPtr (SeleAtomIteratorGc fptr' ptr') f' = HoppyF.withForeignPtr fptr' $ \_ -> f' ptr'
  
  toPtr (SeleAtomIterator ptr') = ptr'
  toPtr (SeleAtomIteratorGc _ ptr') = ptr'
  
  touchCppPtr (SeleAtomIterator _) = HoppyP.return ()
  touchCppPtr (SeleAtomIteratorGc fptr' _) = HoppyF.touchForeignPtr fptr'

instance HoppyFHR.Deletable SeleAtomIterator where
  delete (SeleAtomIterator ptr') = delete'SeleAtomIterator $ (HoppyF.castPtr ptr' :: HoppyF.Ptr SeleAtomIteratorConst)
  delete (SeleAtomIteratorGc _ _) = HoppyP.fail $ HoppyP.concat ["Deletable.delete: Asked to delete a GC-managed ", "SeleAtomIterator", " object."]
  
  toGc this'@(SeleAtomIterator ptr') = if ptr' == HoppyF.nullPtr then HoppyP.return this' else HoppyP.fmap (HoppyP.flip SeleAtomIteratorGc ptr') $ HoppyF.newForeignPtr (HoppyF.castFunPtr deletePtr'SeleAtomIterator :: HoppyF.FunPtr (HoppyF.Ptr () -> HoppyP.IO ())) (HoppyF.castPtr ptr' :: HoppyF.Ptr ())
  toGc this'@(SeleAtomIteratorGc {}) = HoppyP.return this'

instance SeleAtomIteratorConstPtr SeleAtomIterator where
  toSeleAtomIteratorConst (SeleAtomIterator ptr') = SeleAtomIteratorConst $ (HoppyF.castPtr :: HoppyF.Ptr SeleAtomIterator -> HoppyF.Ptr SeleAtomIteratorConst) ptr'
  toSeleAtomIteratorConst (SeleAtomIteratorGc fptr' ptr') = SeleAtomIteratorConstGc fptr' $ (HoppyF.castPtr :: HoppyF.Ptr SeleAtomIterator -> HoppyF.Ptr SeleAtomIteratorConst) ptr'

instance SeleAtomIteratorPtr SeleAtomIterator where
  toSeleAtomIterator = HoppyP.id

class SeleAtomIteratorSuper a where
  downToSeleAtomIterator :: a -> SeleAtomIterator


class SeleAtomIteratorSuperConst a where
  downToSeleAtomIteratorConst :: a -> SeleAtomIteratorConst


instance HoppyFHR.Assignable (HoppyF.Ptr (HoppyF.Ptr SeleAtomIterator)) SeleAtomIterator where
  assign ptr' value' = HoppyF.poke ptr' $ HoppyFHR.toPtr value'

instance HoppyFHR.Decodable (HoppyF.Ptr (HoppyF.Ptr SeleAtomIterator)) SeleAtomIterator where
  decode = HoppyP.fmap SeleAtomIterator . HoppyF.peek