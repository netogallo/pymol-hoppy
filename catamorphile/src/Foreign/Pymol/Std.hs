{-# LANGUAGE FlexibleContexts, FlexibleInstances, ForeignFunctionInterface, MonoLocalBinds, MultiParamTypeClasses, ScopedTypeVariables, TypeSynonymInstances, UndecidableInstances #-}
---------- GENERATED FILE, EDITS WILL BE LOST ----------

module Foreign.Pymol.Std (
  StdStringValue (..),
  StdStringConstPtr (..),
  stdString_get,
  stdString_c_str,
  stdString_data,
  stdString_size,
  stdString_EQ,
  stdString_NE,
  stdString_LT,
  stdString_LE,
  stdString_GT,
  stdString_GE,
  StdStringPtr (..),
  stdString_at,
  stdString_ASSIGN,
  StdStringConst (..),
  castStdStringToConst,
  StdString (..),
  castStdStringToNonconst,
  stdString_new,
  stdString_newFromCString,
  stdString_newFromCStringLen_raw,
  stdString_newCopy,
  StdStringSuper (..),
  StdStringSuperConst (..),
  stdString_newFromCStringLen,
  StdWstringValue (..),
  StdWstringConstPtr (..),
  stdWstring_get,
  stdWstring_c_str,
  stdWstring_data,
  stdWstring_size,
  stdWstring_EQ,
  stdWstring_NE,
  stdWstring_LT,
  stdWstring_LE,
  stdWstring_GT,
  stdWstring_GE,
  StdWstringPtr (..),
  stdWstring_at,
  stdWstring_ASSIGN,
  StdWstringConst (..),
  castStdWstringToConst,
  StdWstring (..),
  castStdWstringToNonconst,
  stdWstring_new,
  stdWstring_newFromCWString,
  stdWstring_newFromCWStringLen_raw,
  stdWstring_newCopy,
  StdWstringSuper (..),
  StdWstringSuperConst (..),
  stdWstring_newFromCWStringLen,
  ) where

import Control.Applicative ((<*))
import qualified Foreign as HoppyF
import qualified Foreign.C as HoppyFC
import qualified Foreign.Hoppy.Runtime as HoppyFHR
import Prelude (($), (.), (/=), (=<<), (==), (>>), (>>=))
import qualified Prelude as HoppyP

foreign import ccall "genpop__StdString_new" stdString_new' ::  HoppyP.IO (HoppyF.Ptr StdString)
foreign import ccall "genpop__StdString_newFromCString" stdString_newFromCString' ::  HoppyF.Ptr HoppyFC.CChar -> HoppyP.IO (HoppyF.Ptr StdString)
foreign import ccall "genpop__StdString_newFromCStringLen_raw" stdString_newFromCStringLen_raw' ::  HoppyF.Ptr HoppyFC.CChar -> HoppyFC.CSize -> HoppyP.IO (HoppyF.Ptr StdString)
foreign import ccall "genpop__StdString_newCopy" stdString_newCopy' ::  HoppyF.Ptr StdStringConst -> HoppyP.IO (HoppyF.Ptr StdString)
foreign import ccall "genpop__StdString_at" stdString_at' ::  HoppyF.Ptr StdString -> HoppyFC.CInt -> HoppyP.IO (HoppyF.Ptr HoppyFC.CChar)
foreign import ccall "genpop__StdString_get" stdString_get' ::  HoppyF.Ptr StdStringConst -> HoppyFC.CInt -> HoppyP.IO HoppyFC.CChar
foreign import ccall "genpop__StdString_c_str" stdString_c_str' ::  HoppyF.Ptr StdStringConst -> HoppyP.IO (HoppyF.Ptr HoppyFC.CChar)
foreign import ccall "genpop__StdString_data" stdString_data' ::  HoppyF.Ptr StdStringConst -> HoppyP.IO (HoppyF.Ptr HoppyFC.CChar)
foreign import ccall "genpop__StdString_size" stdString_size' ::  HoppyF.Ptr StdStringConst -> HoppyP.IO HoppyFC.CSize
foreign import ccall "genpop__StdString_EQ" stdString_EQ' ::  HoppyF.Ptr StdStringConst -> HoppyF.Ptr StdStringConst -> HoppyP.IO HoppyFC.CBool
foreign import ccall "genpop__StdString_NE" stdString_NE' ::  HoppyF.Ptr StdStringConst -> HoppyF.Ptr StdStringConst -> HoppyP.IO HoppyFC.CBool
foreign import ccall "genpop__StdString_LT" stdString_LT' ::  HoppyF.Ptr StdStringConst -> HoppyF.Ptr StdStringConst -> HoppyP.IO HoppyFC.CBool
foreign import ccall "genpop__StdString_LE" stdString_LE' ::  HoppyF.Ptr StdStringConst -> HoppyF.Ptr StdStringConst -> HoppyP.IO HoppyFC.CBool
foreign import ccall "genpop__StdString_GT" stdString_GT' ::  HoppyF.Ptr StdStringConst -> HoppyF.Ptr StdStringConst -> HoppyP.IO HoppyFC.CBool
foreign import ccall "genpop__StdString_GE" stdString_GE' ::  HoppyF.Ptr StdStringConst -> HoppyF.Ptr StdStringConst -> HoppyP.IO HoppyFC.CBool
foreign import ccall "genpop__StdString_ASSIGN" stdString_ASSIGN' ::  HoppyF.Ptr StdString -> HoppyF.Ptr StdStringConst -> HoppyP.IO (HoppyF.Ptr StdString)
foreign import ccall "gendel__StdString" delete'StdString :: HoppyF.Ptr StdStringConst -> HoppyP.IO ()
foreign import ccall "&gendel__StdString" deletePtr'StdString :: HoppyF.FunPtr (HoppyF.Ptr StdStringConst -> HoppyP.IO ())
foreign import ccall "genpop__StdWstring_new" stdWstring_new' ::  HoppyP.IO (HoppyF.Ptr StdWstring)
foreign import ccall "genpop__StdWstring_newFromCWString" stdWstring_newFromCWString' ::  HoppyF.Ptr HoppyFC.CWchar -> HoppyP.IO (HoppyF.Ptr StdWstring)
foreign import ccall "genpop__StdWstring_newFromCWStringLen_raw" stdWstring_newFromCWStringLen_raw' ::  HoppyF.Ptr HoppyFC.CWchar -> HoppyFC.CSize -> HoppyP.IO (HoppyF.Ptr StdWstring)
foreign import ccall "genpop__StdWstring_newCopy" stdWstring_newCopy' ::  HoppyF.Ptr StdWstringConst -> HoppyP.IO (HoppyF.Ptr StdWstring)
foreign import ccall "genpop__StdWstring_at" stdWstring_at' ::  HoppyF.Ptr StdWstring -> HoppyFC.CInt -> HoppyP.IO (HoppyF.Ptr HoppyFC.CWchar)
foreign import ccall "genpop__StdWstring_get" stdWstring_get' ::  HoppyF.Ptr StdWstringConst -> HoppyFC.CInt -> HoppyP.IO HoppyFC.CWchar
foreign import ccall "genpop__StdWstring_c_str" stdWstring_c_str' ::  HoppyF.Ptr StdWstringConst -> HoppyP.IO (HoppyF.Ptr HoppyFC.CWchar)
foreign import ccall "genpop__StdWstring_data" stdWstring_data' ::  HoppyF.Ptr StdWstringConst -> HoppyP.IO (HoppyF.Ptr HoppyFC.CWchar)
foreign import ccall "genpop__StdWstring_size" stdWstring_size' ::  HoppyF.Ptr StdWstringConst -> HoppyP.IO HoppyFC.CSize
foreign import ccall "genpop__StdWstring_EQ" stdWstring_EQ' ::  HoppyF.Ptr StdWstringConst -> HoppyF.Ptr StdWstringConst -> HoppyP.IO HoppyFC.CBool
foreign import ccall "genpop__StdWstring_NE" stdWstring_NE' ::  HoppyF.Ptr StdWstringConst -> HoppyF.Ptr StdWstringConst -> HoppyP.IO HoppyFC.CBool
foreign import ccall "genpop__StdWstring_LT" stdWstring_LT' ::  HoppyF.Ptr StdWstringConst -> HoppyF.Ptr StdWstringConst -> HoppyP.IO HoppyFC.CBool
foreign import ccall "genpop__StdWstring_LE" stdWstring_LE' ::  HoppyF.Ptr StdWstringConst -> HoppyF.Ptr StdWstringConst -> HoppyP.IO HoppyFC.CBool
foreign import ccall "genpop__StdWstring_GT" stdWstring_GT' ::  HoppyF.Ptr StdWstringConst -> HoppyF.Ptr StdWstringConst -> HoppyP.IO HoppyFC.CBool
foreign import ccall "genpop__StdWstring_GE" stdWstring_GE' ::  HoppyF.Ptr StdWstringConst -> HoppyF.Ptr StdWstringConst -> HoppyP.IO HoppyFC.CBool
foreign import ccall "genpop__StdWstring_ASSIGN" stdWstring_ASSIGN' ::  HoppyF.Ptr StdWstring -> HoppyF.Ptr StdWstringConst -> HoppyP.IO (HoppyF.Ptr StdWstring)
foreign import ccall "gendel__StdWstring" delete'StdWstring :: HoppyF.Ptr StdWstringConst -> HoppyP.IO ()
foreign import ccall "&gendel__StdWstring" deletePtr'StdWstring :: HoppyF.FunPtr (HoppyF.Ptr StdWstringConst -> HoppyP.IO ())

class StdStringValue a where
  withStdStringPtr :: a -> (StdStringConst -> HoppyP.IO b) -> HoppyP.IO b

instance {-# OVERLAPPABLE #-} StdStringConstPtr a => StdStringValue a where
  withStdStringPtr = HoppyP.flip ($) . toStdStringConst

instance {-# OVERLAPPING #-} StdStringValue (HoppyP.String) where
  withStdStringPtr = HoppyFHR.withCppObj

class (HoppyFHR.CppPtr this) => StdStringConstPtr this where
  toStdStringConst :: this -> StdStringConst

stdString_get :: (StdStringValue this) => (this) {- ^ this -} -> (HoppyP.Int) -> (HoppyP.IO HoppyFC.CChar)
stdString_get arg'1 arg'2 =
  withStdStringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  (
    HoppyP.return . HoppyFHR.coerceIntegral
  ) arg'2 >>= \arg'2' ->
  (stdString_get' arg'1' arg'2')

stdString_c_str :: (StdStringValue this) => (this) {- ^ this -} -> (HoppyP.IO (HoppyF.Ptr HoppyFC.CChar))
stdString_c_str arg'1 =
  withStdStringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  (stdString_c_str' arg'1')

stdString_data :: (StdStringValue this) => (this) {- ^ this -} -> (HoppyP.IO (HoppyF.Ptr HoppyFC.CChar))
stdString_data arg'1 =
  withStdStringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  (stdString_data' arg'1')

stdString_size :: (StdStringValue this) => (this) {- ^ this -} -> (HoppyP.IO HoppyFC.CSize)
stdString_size arg'1 =
  withStdStringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  (stdString_size' arg'1')

stdString_EQ :: (StdStringValue this, StdStringValue arg'2) => (this) {- ^ this -} -> (arg'2) -> (HoppyP.IO HoppyP.Bool)
stdString_EQ arg'1 arg'2 =
  withStdStringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  withStdStringPtr arg'2 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'2' ->
  (
    (HoppyP.return . (/= 0))
  ) =<<
  (stdString_EQ' arg'1' arg'2')

stdString_NE :: (StdStringValue this, StdStringValue arg'2) => (this) {- ^ this -} -> (arg'2) -> (HoppyP.IO HoppyP.Bool)
stdString_NE arg'1 arg'2 =
  withStdStringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  withStdStringPtr arg'2 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'2' ->
  (
    (HoppyP.return . (/= 0))
  ) =<<
  (stdString_NE' arg'1' arg'2')

stdString_LT :: (StdStringValue this, StdStringValue arg'2) => (this) {- ^ this -} -> (arg'2) -> (HoppyP.IO HoppyP.Bool)
stdString_LT arg'1 arg'2 =
  withStdStringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  withStdStringPtr arg'2 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'2' ->
  (
    (HoppyP.return . (/= 0))
  ) =<<
  (stdString_LT' arg'1' arg'2')

stdString_LE :: (StdStringValue this, StdStringValue arg'2) => (this) {- ^ this -} -> (arg'2) -> (HoppyP.IO HoppyP.Bool)
stdString_LE arg'1 arg'2 =
  withStdStringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  withStdStringPtr arg'2 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'2' ->
  (
    (HoppyP.return . (/= 0))
  ) =<<
  (stdString_LE' arg'1' arg'2')

stdString_GT :: (StdStringValue this, StdStringValue arg'2) => (this) {- ^ this -} -> (arg'2) -> (HoppyP.IO HoppyP.Bool)
stdString_GT arg'1 arg'2 =
  withStdStringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  withStdStringPtr arg'2 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'2' ->
  (
    (HoppyP.return . (/= 0))
  ) =<<
  (stdString_GT' arg'1' arg'2')

stdString_GE :: (StdStringValue this, StdStringValue arg'2) => (this) {- ^ this -} -> (arg'2) -> (HoppyP.IO HoppyP.Bool)
stdString_GE arg'1 arg'2 =
  withStdStringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  withStdStringPtr arg'2 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'2' ->
  (
    (HoppyP.return . (/= 0))
  ) =<<
  (stdString_GE' arg'1' arg'2')

class (StdStringConstPtr this) => StdStringPtr this where
  toStdString :: this -> StdString

stdString_at :: (StdStringPtr this) => (this) {- ^ this -} -> (HoppyP.Int) -> (HoppyP.IO (HoppyF.Ptr HoppyFC.CChar))
stdString_at arg'1 arg'2 =
  HoppyFHR.withCppPtr (toStdString arg'1) $ \arg'1' ->
  (
    HoppyP.return . HoppyFHR.coerceIntegral
  ) arg'2 >>= \arg'2' ->
  (stdString_at' arg'1' arg'2')

stdString_ASSIGN :: (StdStringPtr this, StdStringValue arg'2) => (this) {- ^ this -} -> (arg'2) -> (HoppyP.IO StdString)
stdString_ASSIGN arg'1 arg'2 =
  HoppyFHR.withCppPtr (toStdString arg'1) $ \arg'1' ->
  withStdStringPtr arg'2 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'2' ->
  HoppyP.fmap StdString
  (stdString_ASSIGN' arg'1' arg'2')

data StdStringConst =
    StdStringConst (HoppyF.Ptr StdStringConst)
  | StdStringConstGc (HoppyF.ForeignPtr ()) (HoppyF.Ptr StdStringConst)
  deriving (HoppyP.Show)

instance HoppyP.Eq StdStringConst where
  x == y = HoppyFHR.toPtr x == HoppyFHR.toPtr y

instance HoppyP.Ord StdStringConst where
  compare x y = HoppyP.compare (HoppyFHR.toPtr x) (HoppyFHR.toPtr y)

castStdStringToConst :: StdString -> StdStringConst
castStdStringToConst (StdString ptr') = StdStringConst $ HoppyF.castPtr ptr'
castStdStringToConst (StdStringGc fptr' ptr') = StdStringConstGc fptr' $ HoppyF.castPtr ptr'

instance HoppyFHR.CppPtr StdStringConst where
  nullptr = StdStringConst HoppyF.nullPtr
  
  withCppPtr (StdStringConst ptr') f' = f' ptr'
  withCppPtr (StdStringConstGc fptr' ptr') f' = HoppyF.withForeignPtr fptr' $ \_ -> f' ptr'
  
  toPtr (StdStringConst ptr') = ptr'
  toPtr (StdStringConstGc _ ptr') = ptr'
  
  touchCppPtr (StdStringConst _) = HoppyP.return ()
  touchCppPtr (StdStringConstGc fptr' _) = HoppyF.touchForeignPtr fptr'

instance HoppyFHR.Deletable StdStringConst where
  delete (StdStringConst ptr') = delete'StdString ptr'
  delete (StdStringConstGc _ _) = HoppyP.fail $ HoppyP.concat ["Deletable.delete: Asked to delete a GC-managed ", "StdStringConst", " object."]
  
  toGc this'@(StdStringConst ptr') = if ptr' == HoppyF.nullPtr then HoppyP.return this' else HoppyP.fmap (HoppyP.flip StdStringConstGc ptr') $ HoppyF.newForeignPtr (HoppyF.castFunPtr deletePtr'StdString :: HoppyF.FunPtr (HoppyF.Ptr () -> HoppyP.IO ())) (HoppyF.castPtr ptr' :: HoppyF.Ptr ())
  toGc this'@(StdStringConstGc {}) = HoppyP.return this'

instance HoppyFHR.Copyable StdStringConst StdString where copy = stdString_newCopy

instance StdStringConstPtr StdStringConst where
  toStdStringConst = HoppyP.id

data StdString =
    StdString (HoppyF.Ptr StdString)
  | StdStringGc (HoppyF.ForeignPtr ()) (HoppyF.Ptr StdString)
  deriving (HoppyP.Show)

instance HoppyP.Eq StdString where
  x == y = HoppyFHR.toPtr x == HoppyFHR.toPtr y

instance HoppyP.Ord StdString where
  compare x y = HoppyP.compare (HoppyFHR.toPtr x) (HoppyFHR.toPtr y)

castStdStringToNonconst :: StdStringConst -> StdString
castStdStringToNonconst (StdStringConst ptr') = StdString $ HoppyF.castPtr ptr'
castStdStringToNonconst (StdStringConstGc fptr' ptr') = StdStringGc fptr' $ HoppyF.castPtr ptr'

instance HoppyFHR.CppPtr StdString where
  nullptr = StdString HoppyF.nullPtr
  
  withCppPtr (StdString ptr') f' = f' ptr'
  withCppPtr (StdStringGc fptr' ptr') f' = HoppyF.withForeignPtr fptr' $ \_ -> f' ptr'
  
  toPtr (StdString ptr') = ptr'
  toPtr (StdStringGc _ ptr') = ptr'
  
  touchCppPtr (StdString _) = HoppyP.return ()
  touchCppPtr (StdStringGc fptr' _) = HoppyF.touchForeignPtr fptr'

instance HoppyFHR.Deletable StdString where
  delete (StdString ptr') = delete'StdString $ (HoppyF.castPtr ptr' :: HoppyF.Ptr StdStringConst)
  delete (StdStringGc _ _) = HoppyP.fail $ HoppyP.concat ["Deletable.delete: Asked to delete a GC-managed ", "StdString", " object."]
  
  toGc this'@(StdString ptr') = if ptr' == HoppyF.nullPtr then HoppyP.return this' else HoppyP.fmap (HoppyP.flip StdStringGc ptr') $ HoppyF.newForeignPtr (HoppyF.castFunPtr deletePtr'StdString :: HoppyF.FunPtr (HoppyF.Ptr () -> HoppyP.IO ())) (HoppyF.castPtr ptr' :: HoppyF.Ptr ())
  toGc this'@(StdStringGc {}) = HoppyP.return this'

instance HoppyFHR.Copyable StdString StdString where copy = stdString_newCopy

instance StdStringConstPtr StdString where
  toStdStringConst (StdString ptr') = StdStringConst $ (HoppyF.castPtr :: HoppyF.Ptr StdString -> HoppyF.Ptr StdStringConst) ptr'
  toStdStringConst (StdStringGc fptr' ptr') = StdStringConstGc fptr' $ (HoppyF.castPtr :: HoppyF.Ptr StdString -> HoppyF.Ptr StdStringConst) ptr'

instance StdStringPtr StdString where
  toStdString = HoppyP.id

stdString_new :: (HoppyP.IO StdString)
stdString_new =
  HoppyP.fmap StdString
  (stdString_new')

stdString_newFromCString :: (HoppyF.Ptr HoppyFC.CChar) -> (HoppyP.IO StdString)
stdString_newFromCString arg'1 =
  let arg'1' = arg'1 in
  HoppyP.fmap StdString
  (stdString_newFromCString' arg'1')

stdString_newFromCStringLen_raw :: (HoppyF.Ptr HoppyFC.CChar) -> (HoppyFC.CSize) -> (HoppyP.IO StdString)
stdString_newFromCStringLen_raw arg'1 arg'2 =
  let arg'1' = arg'1 in
  let arg'2' = arg'2 in
  HoppyP.fmap StdString
  (stdString_newFromCStringLen_raw' arg'1' arg'2')

stdString_newCopy :: (StdStringValue arg'1) => (arg'1) -> (HoppyP.IO StdString)
stdString_newCopy arg'1 =
  withStdStringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  HoppyP.fmap StdString
  (stdString_newCopy' arg'1')

class StdStringSuper a where
  downToStdString :: a -> StdString


class StdStringSuperConst a where
  downToStdStringConst :: a -> StdStringConst


instance HoppyFHR.Assignable (HoppyF.Ptr (HoppyF.Ptr StdString)) StdString where
  assign ptr' value' = HoppyF.poke ptr' $ HoppyFHR.toPtr value'

instance StdStringValue a => HoppyFHR.Assignable StdString a where
  assign x' y' = stdString_ASSIGN x' y' >> HoppyP.return ()

instance HoppyFHR.Decodable (HoppyF.Ptr (HoppyF.Ptr StdString)) StdString where
  decode = HoppyP.fmap StdString . HoppyF.peek

instance HoppyFHR.Encodable StdString (HoppyP.String) where
  encode =
    HoppyP.flip HoppyFC.withCStringLen stdString_newFromCStringLen

instance HoppyFHR.Encodable StdStringConst (HoppyP.String) where
  encode = HoppyP.fmap (toStdStringConst) . HoppyFHR.encodeAs (HoppyP.undefined :: StdString)

instance HoppyFHR.Decodable StdString (HoppyP.String) where
  decode = HoppyFHR.decode . toStdStringConst

instance HoppyFHR.Decodable StdStringConst (HoppyP.String) where
  decode =
    \s -> do
      p <- stdString_data s
      n <- stdString_size s
      HoppyFC.peekCStringLen (p, HoppyP.fromIntegral n) <* HoppyFHR.touchCppPtr s
stdString_newFromCStringLen :: HoppyFC.CStringLen -> HoppyP.IO StdString
stdString_newFromCStringLen (p,n) =
  stdString_newFromCStringLen_raw p (HoppyP.fromIntegral n)

class StdWstringValue a where
  withStdWstringPtr :: a -> (StdWstringConst -> HoppyP.IO b) -> HoppyP.IO b

instance {-# OVERLAPPABLE #-} StdWstringConstPtr a => StdWstringValue a where
  withStdWstringPtr = HoppyP.flip ($) . toStdWstringConst

instance {-# OVERLAPPING #-} StdWstringValue (HoppyP.String) where
  withStdWstringPtr = HoppyFHR.withCppObj

class (HoppyFHR.CppPtr this) => StdWstringConstPtr this where
  toStdWstringConst :: this -> StdWstringConst

stdWstring_get :: (StdWstringValue this) => (this) {- ^ this -} -> (HoppyP.Int) -> (HoppyP.IO HoppyFC.CWchar)
stdWstring_get arg'1 arg'2 =
  withStdWstringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  (
    HoppyP.return . HoppyFHR.coerceIntegral
  ) arg'2 >>= \arg'2' ->
  (stdWstring_get' arg'1' arg'2')

stdWstring_c_str :: (StdWstringValue this) => (this) {- ^ this -} -> (HoppyP.IO (HoppyF.Ptr HoppyFC.CWchar))
stdWstring_c_str arg'1 =
  withStdWstringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  (stdWstring_c_str' arg'1')

stdWstring_data :: (StdWstringValue this) => (this) {- ^ this -} -> (HoppyP.IO (HoppyF.Ptr HoppyFC.CWchar))
stdWstring_data arg'1 =
  withStdWstringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  (stdWstring_data' arg'1')

stdWstring_size :: (StdWstringValue this) => (this) {- ^ this -} -> (HoppyP.IO HoppyFC.CSize)
stdWstring_size arg'1 =
  withStdWstringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  (stdWstring_size' arg'1')

stdWstring_EQ :: (StdWstringValue this, StdWstringValue arg'2) => (this) {- ^ this -} -> (arg'2) -> (HoppyP.IO HoppyP.Bool)
stdWstring_EQ arg'1 arg'2 =
  withStdWstringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  withStdWstringPtr arg'2 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'2' ->
  (
    (HoppyP.return . (/= 0))
  ) =<<
  (stdWstring_EQ' arg'1' arg'2')

stdWstring_NE :: (StdWstringValue this, StdWstringValue arg'2) => (this) {- ^ this -} -> (arg'2) -> (HoppyP.IO HoppyP.Bool)
stdWstring_NE arg'1 arg'2 =
  withStdWstringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  withStdWstringPtr arg'2 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'2' ->
  (
    (HoppyP.return . (/= 0))
  ) =<<
  (stdWstring_NE' arg'1' arg'2')

stdWstring_LT :: (StdWstringValue this, StdWstringValue arg'2) => (this) {- ^ this -} -> (arg'2) -> (HoppyP.IO HoppyP.Bool)
stdWstring_LT arg'1 arg'2 =
  withStdWstringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  withStdWstringPtr arg'2 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'2' ->
  (
    (HoppyP.return . (/= 0))
  ) =<<
  (stdWstring_LT' arg'1' arg'2')

stdWstring_LE :: (StdWstringValue this, StdWstringValue arg'2) => (this) {- ^ this -} -> (arg'2) -> (HoppyP.IO HoppyP.Bool)
stdWstring_LE arg'1 arg'2 =
  withStdWstringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  withStdWstringPtr arg'2 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'2' ->
  (
    (HoppyP.return . (/= 0))
  ) =<<
  (stdWstring_LE' arg'1' arg'2')

stdWstring_GT :: (StdWstringValue this, StdWstringValue arg'2) => (this) {- ^ this -} -> (arg'2) -> (HoppyP.IO HoppyP.Bool)
stdWstring_GT arg'1 arg'2 =
  withStdWstringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  withStdWstringPtr arg'2 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'2' ->
  (
    (HoppyP.return . (/= 0))
  ) =<<
  (stdWstring_GT' arg'1' arg'2')

stdWstring_GE :: (StdWstringValue this, StdWstringValue arg'2) => (this) {- ^ this -} -> (arg'2) -> (HoppyP.IO HoppyP.Bool)
stdWstring_GE arg'1 arg'2 =
  withStdWstringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  withStdWstringPtr arg'2 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'2' ->
  (
    (HoppyP.return . (/= 0))
  ) =<<
  (stdWstring_GE' arg'1' arg'2')

class (StdWstringConstPtr this) => StdWstringPtr this where
  toStdWstring :: this -> StdWstring

stdWstring_at :: (StdWstringPtr this) => (this) {- ^ this -} -> (HoppyP.Int) -> (HoppyP.IO (HoppyF.Ptr HoppyFC.CWchar))
stdWstring_at arg'1 arg'2 =
  HoppyFHR.withCppPtr (toStdWstring arg'1) $ \arg'1' ->
  (
    HoppyP.return . HoppyFHR.coerceIntegral
  ) arg'2 >>= \arg'2' ->
  (stdWstring_at' arg'1' arg'2')

stdWstring_ASSIGN :: (StdWstringPtr this, StdWstringValue arg'2) => (this) {- ^ this -} -> (arg'2) -> (HoppyP.IO StdWstring)
stdWstring_ASSIGN arg'1 arg'2 =
  HoppyFHR.withCppPtr (toStdWstring arg'1) $ \arg'1' ->
  withStdWstringPtr arg'2 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'2' ->
  HoppyP.fmap StdWstring
  (stdWstring_ASSIGN' arg'1' arg'2')

data StdWstringConst =
    StdWstringConst (HoppyF.Ptr StdWstringConst)
  | StdWstringConstGc (HoppyF.ForeignPtr ()) (HoppyF.Ptr StdWstringConst)
  deriving (HoppyP.Show)

instance HoppyP.Eq StdWstringConst where
  x == y = HoppyFHR.toPtr x == HoppyFHR.toPtr y

instance HoppyP.Ord StdWstringConst where
  compare x y = HoppyP.compare (HoppyFHR.toPtr x) (HoppyFHR.toPtr y)

castStdWstringToConst :: StdWstring -> StdWstringConst
castStdWstringToConst (StdWstring ptr') = StdWstringConst $ HoppyF.castPtr ptr'
castStdWstringToConst (StdWstringGc fptr' ptr') = StdWstringConstGc fptr' $ HoppyF.castPtr ptr'

instance HoppyFHR.CppPtr StdWstringConst where
  nullptr = StdWstringConst HoppyF.nullPtr
  
  withCppPtr (StdWstringConst ptr') f' = f' ptr'
  withCppPtr (StdWstringConstGc fptr' ptr') f' = HoppyF.withForeignPtr fptr' $ \_ -> f' ptr'
  
  toPtr (StdWstringConst ptr') = ptr'
  toPtr (StdWstringConstGc _ ptr') = ptr'
  
  touchCppPtr (StdWstringConst _) = HoppyP.return ()
  touchCppPtr (StdWstringConstGc fptr' _) = HoppyF.touchForeignPtr fptr'

instance HoppyFHR.Deletable StdWstringConst where
  delete (StdWstringConst ptr') = delete'StdWstring ptr'
  delete (StdWstringConstGc _ _) = HoppyP.fail $ HoppyP.concat ["Deletable.delete: Asked to delete a GC-managed ", "StdWstringConst", " object."]
  
  toGc this'@(StdWstringConst ptr') = if ptr' == HoppyF.nullPtr then HoppyP.return this' else HoppyP.fmap (HoppyP.flip StdWstringConstGc ptr') $ HoppyF.newForeignPtr (HoppyF.castFunPtr deletePtr'StdWstring :: HoppyF.FunPtr (HoppyF.Ptr () -> HoppyP.IO ())) (HoppyF.castPtr ptr' :: HoppyF.Ptr ())
  toGc this'@(StdWstringConstGc {}) = HoppyP.return this'

instance HoppyFHR.Copyable StdWstringConst StdWstring where copy = stdWstring_newCopy

instance StdWstringConstPtr StdWstringConst where
  toStdWstringConst = HoppyP.id

data StdWstring =
    StdWstring (HoppyF.Ptr StdWstring)
  | StdWstringGc (HoppyF.ForeignPtr ()) (HoppyF.Ptr StdWstring)
  deriving (HoppyP.Show)

instance HoppyP.Eq StdWstring where
  x == y = HoppyFHR.toPtr x == HoppyFHR.toPtr y

instance HoppyP.Ord StdWstring where
  compare x y = HoppyP.compare (HoppyFHR.toPtr x) (HoppyFHR.toPtr y)

castStdWstringToNonconst :: StdWstringConst -> StdWstring
castStdWstringToNonconst (StdWstringConst ptr') = StdWstring $ HoppyF.castPtr ptr'
castStdWstringToNonconst (StdWstringConstGc fptr' ptr') = StdWstringGc fptr' $ HoppyF.castPtr ptr'

instance HoppyFHR.CppPtr StdWstring where
  nullptr = StdWstring HoppyF.nullPtr
  
  withCppPtr (StdWstring ptr') f' = f' ptr'
  withCppPtr (StdWstringGc fptr' ptr') f' = HoppyF.withForeignPtr fptr' $ \_ -> f' ptr'
  
  toPtr (StdWstring ptr') = ptr'
  toPtr (StdWstringGc _ ptr') = ptr'
  
  touchCppPtr (StdWstring _) = HoppyP.return ()
  touchCppPtr (StdWstringGc fptr' _) = HoppyF.touchForeignPtr fptr'

instance HoppyFHR.Deletable StdWstring where
  delete (StdWstring ptr') = delete'StdWstring $ (HoppyF.castPtr ptr' :: HoppyF.Ptr StdWstringConst)
  delete (StdWstringGc _ _) = HoppyP.fail $ HoppyP.concat ["Deletable.delete: Asked to delete a GC-managed ", "StdWstring", " object."]
  
  toGc this'@(StdWstring ptr') = if ptr' == HoppyF.nullPtr then HoppyP.return this' else HoppyP.fmap (HoppyP.flip StdWstringGc ptr') $ HoppyF.newForeignPtr (HoppyF.castFunPtr deletePtr'StdWstring :: HoppyF.FunPtr (HoppyF.Ptr () -> HoppyP.IO ())) (HoppyF.castPtr ptr' :: HoppyF.Ptr ())
  toGc this'@(StdWstringGc {}) = HoppyP.return this'

instance HoppyFHR.Copyable StdWstring StdWstring where copy = stdWstring_newCopy

instance StdWstringConstPtr StdWstring where
  toStdWstringConst (StdWstring ptr') = StdWstringConst $ (HoppyF.castPtr :: HoppyF.Ptr StdWstring -> HoppyF.Ptr StdWstringConst) ptr'
  toStdWstringConst (StdWstringGc fptr' ptr') = StdWstringConstGc fptr' $ (HoppyF.castPtr :: HoppyF.Ptr StdWstring -> HoppyF.Ptr StdWstringConst) ptr'

instance StdWstringPtr StdWstring where
  toStdWstring = HoppyP.id

stdWstring_new :: (HoppyP.IO StdWstring)
stdWstring_new =
  HoppyP.fmap StdWstring
  (stdWstring_new')

stdWstring_newFromCWString :: (HoppyF.Ptr HoppyFC.CWchar) -> (HoppyP.IO StdWstring)
stdWstring_newFromCWString arg'1 =
  let arg'1' = arg'1 in
  HoppyP.fmap StdWstring
  (stdWstring_newFromCWString' arg'1')

stdWstring_newFromCWStringLen_raw :: (HoppyF.Ptr HoppyFC.CWchar) -> (HoppyFC.CSize) -> (HoppyP.IO StdWstring)
stdWstring_newFromCWStringLen_raw arg'1 arg'2 =
  let arg'1' = arg'1 in
  let arg'2' = arg'2 in
  HoppyP.fmap StdWstring
  (stdWstring_newFromCWStringLen_raw' arg'1' arg'2')

stdWstring_newCopy :: (StdWstringValue arg'1) => (arg'1) -> (HoppyP.IO StdWstring)
stdWstring_newCopy arg'1 =
  withStdWstringPtr arg'1 $ HoppyP.flip HoppyFHR.withCppPtr $ \arg'1' ->
  HoppyP.fmap StdWstring
  (stdWstring_newCopy' arg'1')

class StdWstringSuper a where
  downToStdWstring :: a -> StdWstring


class StdWstringSuperConst a where
  downToStdWstringConst :: a -> StdWstringConst


instance HoppyFHR.Assignable (HoppyF.Ptr (HoppyF.Ptr StdWstring)) StdWstring where
  assign ptr' value' = HoppyF.poke ptr' $ HoppyFHR.toPtr value'

instance StdWstringValue a => HoppyFHR.Assignable StdWstring a where
  assign x' y' = stdWstring_ASSIGN x' y' >> HoppyP.return ()

instance HoppyFHR.Decodable (HoppyF.Ptr (HoppyF.Ptr StdWstring)) StdWstring where
  decode = HoppyP.fmap StdWstring . HoppyF.peek

instance HoppyFHR.Encodable StdWstring (HoppyP.String) where
  encode =
    HoppyP.flip HoppyFC.withCWStringLen stdWstring_newFromCWStringLen

instance HoppyFHR.Encodable StdWstringConst (HoppyP.String) where
  encode = HoppyP.fmap (toStdWstringConst) . HoppyFHR.encodeAs (HoppyP.undefined :: StdWstring)

instance HoppyFHR.Decodable StdWstring (HoppyP.String) where
  decode = HoppyFHR.decode . toStdWstringConst

instance HoppyFHR.Decodable StdWstringConst (HoppyP.String) where
  decode =
    \s -> do
      p <- stdWstring_data s
      n <- stdWstring_size s
      HoppyFC.peekCWStringLen (p, HoppyP.fromIntegral n) <* HoppyFHR.touchCppPtr s
stdWstring_newFromCWStringLen :: HoppyFC.CWStringLen -> HoppyP.IO StdWstring
stdWstring_newFromCWStringLen (p,n) =
  stdWstring_newFromCWStringLen_raw p (HoppyP.fromIntegral n)