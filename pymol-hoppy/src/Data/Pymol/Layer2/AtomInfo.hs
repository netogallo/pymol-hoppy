{-# LANGUAGE FlexibleContexts, FlexibleInstances, ForeignFunctionInterface, MonoLocalBinds, MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances, UndecidableInstances #-}
---------- GENERATED FILE, EDITS WILL BE LOST ----------

module Data.Pymol.Layer2.AtomInfo (
  AtomInfoTypeValue (..),
  AtomInfoTypeConstPtr (..),
  AtomInfoTypePtr (..),
  AtomInfoTypeConst (..),
  castAtomInfoTypeToConst,
  AtomInfoType (..),
  castAtomInfoTypeToNonconst,
  anisou_get,
  anisou_set,
  AtomInfoTypeSuper (..),
  AtomInfoTypeSuperConst (..),
  ) where

import qualified Foreign as HoppyF
import qualified Foreign.C as HoppyFC
import qualified Foreign.Hoppy.Runtime as HoppyFHR
import Prelude (($), (.), (==))
import qualified Prelude as HoppyP

foreign import ccall "genpop__AtomInfoType_anisou_get" anisou_get' ::  HoppyF.Ptr AtomInfoTypeConst -> HoppyP.IO (HoppyF.Ptr HoppyFC.CFloat)
foreign import ccall "genpop__AtomInfoType_anisou_set" anisou_set' ::  HoppyF.Ptr AtomInfoType -> HoppyF.Ptr HoppyFC.CFloat -> HoppyP.IO ()
foreign import ccall "gendel__AtomInfoType" delete'AtomInfoType :: HoppyF.Ptr AtomInfoTypeConst -> HoppyP.IO ()
foreign import ccall "&gendel__AtomInfoType" deletePtr'AtomInfoType :: HoppyF.FunPtr (HoppyF.Ptr AtomInfoTypeConst -> HoppyP.IO ())

class AtomInfoTypeValue a where
  withAtomInfoTypePtr :: a -> (AtomInfoTypeConst -> HoppyP.IO b) -> HoppyP.IO b

instance {-# OVERLAPPABLE #-} AtomInfoTypeConstPtr a => AtomInfoTypeValue a where
  withAtomInfoTypePtr = HoppyP.flip ($) . toAtomInfoTypeConst

class (HoppyFHR.CppPtr this) => AtomInfoTypeConstPtr this where
  toAtomInfoTypeConst :: this -> AtomInfoTypeConst

class (AtomInfoTypeConstPtr this) => AtomInfoTypePtr this where
  toAtomInfoType :: this -> AtomInfoType

data AtomInfoTypeConst =
    AtomInfoTypeConst (HoppyF.Ptr AtomInfoTypeConst)
  | AtomInfoTypeConstGc (HoppyF.ForeignPtr ()) (HoppyF.Ptr AtomInfoTypeConst)
  deriving (HoppyP.Show)

instance HoppyP.Eq AtomInfoTypeConst where
  x == y = HoppyFHR.toPtr x == HoppyFHR.toPtr y

instance HoppyP.Ord AtomInfoTypeConst where
  compare x y = HoppyP.compare (HoppyFHR.toPtr x) (HoppyFHR.toPtr y)

castAtomInfoTypeToConst :: AtomInfoType -> AtomInfoTypeConst
castAtomInfoTypeToConst (AtomInfoType ptr') = AtomInfoTypeConst $ HoppyF.castPtr ptr'
castAtomInfoTypeToConst (AtomInfoTypeGc fptr' ptr') = AtomInfoTypeConstGc fptr' $ HoppyF.castPtr ptr'

instance HoppyFHR.CppPtr AtomInfoTypeConst where
  nullptr = AtomInfoTypeConst HoppyF.nullPtr
  
  withCppPtr (AtomInfoTypeConst ptr') f' = f' ptr'
  withCppPtr (AtomInfoTypeConstGc fptr' ptr') f' = HoppyF.withForeignPtr fptr' $ \_ -> f' ptr'
  
  toPtr (AtomInfoTypeConst ptr') = ptr'
  toPtr (AtomInfoTypeConstGc _ ptr') = ptr'
  
  touchCppPtr (AtomInfoTypeConst _) = HoppyP.return ()
  touchCppPtr (AtomInfoTypeConstGc fptr' _) = HoppyF.touchForeignPtr fptr'

instance HoppyFHR.Deletable AtomInfoTypeConst where
  delete (AtomInfoTypeConst ptr') = delete'AtomInfoType ptr'
  delete (AtomInfoTypeConstGc _ _) = HoppyP.fail $ HoppyP.concat ["Deletable.delete: Asked to delete a GC-managed ", "AtomInfoTypeConst", " object."]
  
  toGc this'@(AtomInfoTypeConst ptr') = if ptr' == HoppyF.nullPtr then HoppyP.return this' else HoppyP.fmap (HoppyP.flip AtomInfoTypeConstGc ptr') $ HoppyF.newForeignPtr (HoppyF.castFunPtr deletePtr'AtomInfoType :: HoppyF.FunPtr (HoppyF.Ptr () -> HoppyP.IO ())) (HoppyF.castPtr ptr' :: HoppyF.Ptr ())
  toGc this'@(AtomInfoTypeConstGc {}) = HoppyP.return this'

instance AtomInfoTypeConstPtr AtomInfoTypeConst where
  toAtomInfoTypeConst = HoppyP.id

data AtomInfoType =
    AtomInfoType (HoppyF.Ptr AtomInfoType)
  | AtomInfoTypeGc (HoppyF.ForeignPtr ()) (HoppyF.Ptr AtomInfoType)
  deriving (HoppyP.Show)

instance HoppyP.Eq AtomInfoType where
  x == y = HoppyFHR.toPtr x == HoppyFHR.toPtr y

instance HoppyP.Ord AtomInfoType where
  compare x y = HoppyP.compare (HoppyFHR.toPtr x) (HoppyFHR.toPtr y)

castAtomInfoTypeToNonconst :: AtomInfoTypeConst -> AtomInfoType
castAtomInfoTypeToNonconst (AtomInfoTypeConst ptr') = AtomInfoType $ HoppyF.castPtr ptr'
castAtomInfoTypeToNonconst (AtomInfoTypeConstGc fptr' ptr') = AtomInfoTypeGc fptr' $ HoppyF.castPtr ptr'

instance HoppyFHR.CppPtr AtomInfoType where
  nullptr = AtomInfoType HoppyF.nullPtr
  
  withCppPtr (AtomInfoType ptr') f' = f' ptr'
  withCppPtr (AtomInfoTypeGc fptr' ptr') f' = HoppyF.withForeignPtr fptr' $ \_ -> f' ptr'
  
  toPtr (AtomInfoType ptr') = ptr'
  toPtr (AtomInfoTypeGc _ ptr') = ptr'
  
  touchCppPtr (AtomInfoType _) = HoppyP.return ()
  touchCppPtr (AtomInfoTypeGc fptr' _) = HoppyF.touchForeignPtr fptr'

instance HoppyFHR.Deletable AtomInfoType where
  delete (AtomInfoType ptr') = delete'AtomInfoType $ (HoppyF.castPtr ptr' :: HoppyF.Ptr AtomInfoTypeConst)
  delete (AtomInfoTypeGc _ _) = HoppyP.fail $ HoppyP.concat ["Deletable.delete: Asked to delete a GC-managed ", "AtomInfoType", " object."]
  
  toGc this'@(AtomInfoType ptr') = if ptr' == HoppyF.nullPtr then HoppyP.return this' else HoppyP.fmap (HoppyP.flip AtomInfoTypeGc ptr') $ HoppyF.newForeignPtr (HoppyF.castFunPtr deletePtr'AtomInfoType :: HoppyF.FunPtr (HoppyF.Ptr () -> HoppyP.IO ())) (HoppyF.castPtr ptr' :: HoppyF.Ptr ())
  toGc this'@(AtomInfoTypeGc {}) = HoppyP.return this'

instance AtomInfoTypeConstPtr AtomInfoType where
  toAtomInfoTypeConst (AtomInfoType ptr') = AtomInfoTypeConst $ (HoppyF.castPtr :: HoppyF.Ptr AtomInfoType -> HoppyF.Ptr AtomInfoTypeConst) ptr'
  toAtomInfoTypeConst (AtomInfoTypeGc fptr' ptr') = AtomInfoTypeConstGc fptr' $ (HoppyF.castPtr :: HoppyF.Ptr AtomInfoType -> HoppyF.Ptr AtomInfoTypeConst) ptr'

instance AtomInfoTypePtr AtomInfoType where
  toAtomInfoType = HoppyP.id

anisou_get :: (AtomInfoTypeValue arg'1) => (arg'1) -> (HoppyP.IO (HoppyF.Ptr HoppyFC.CFloat))
anisou_get arg'1 =
  withAtomInfoTypePtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  (anisou_get' arg'1')

anisou_set :: (AtomInfoTypePtr arg'1) => (arg'1) -> (HoppyF.Ptr HoppyFC.CFloat) -> (HoppyP.IO ())
anisou_set arg'1 arg'2 =
  HoppyFHR.withCppPtr (toAtomInfoType arg'1) $ \arg'1' ->
  let arg'2' = arg'2 in
  (anisou_set' arg'1' arg'2')

class AtomInfoTypeSuper a where
  downToAtomInfoType :: a -> AtomInfoType


class AtomInfoTypeSuperConst a where
  downToAtomInfoTypeConst :: a -> AtomInfoTypeConst


instance HoppyFHR.Assignable (HoppyF.Ptr (HoppyF.Ptr AtomInfoType)) AtomInfoType where
  assign ptr' value' = HoppyF.poke ptr' $ HoppyFHR.toPtr value'

instance HoppyFHR.Decodable (HoppyF.Ptr (HoppyF.Ptr AtomInfoType)) AtomInfoType where
  decode = HoppyP.fmap AtomInfoType . HoppyF.peek