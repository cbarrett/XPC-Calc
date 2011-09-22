{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface, FlexibleInstances #-}

module XPC
  ( Int64
  , withNewXPCPtr
  , XPC
  , XPCable(..)
  , XPCObject
  , XPCType
  , xpc_get_type -- should have our own ADT for this instead of just exporting the function
  , xpc_type_int64
  , xpc_type_array
  , xpc_type_dictionary
  ) where

import Control.Monad
import Data.Functor
import Data.Int (Int64)
import qualified Data.Map as M
import Foreign
import Foreign.C.Types 
import Foreign.C.String
import Foreign.Storable
import Text.Printf

data XPC
type XPCObject = Ptr XPC
type XPCType = Ptr XPC

foreign import ccall unsafe "main.h hsxpc_dictionary_get_keys_and_values"
  hsxpc_dictionary_get_keys_and_values :: XPCObject -> Ptr CString -> Ptr XPCObject -> IO ()

foreign import ccall unsafe "xpc/xpc.h xpc_array_append_value"
  xpc_array_append_value :: XPCObject -> XPCObject -> IO ()

foreign import ccall unsafe "xpc/xpc.h xpc_array_create"
  xpc_array_create :: Ptr XPCObject -> CSize -> IO XPCObject

foreign import ccall unsafe "xpc/xpc.h xpc_array_get_count"
  xpc_array_get_count :: XPCObject -> CSize

foreign import ccall unsafe "xpc/xpc.h xpc_array_get_value"
  xpc_array_get_value ::  XPCObject -> CSize -> XPCObject

foreign import ccall unsafe "xpc/xpc.h xpc_copy_description"
  xpc_copy_description :: XPCObject -> CString

foreign import ccall unsafe "xpc/xpc.h xpc_dictionary_create"
  xpc_dictionary_create :: Ptr CString -> Ptr XPCObject -> CSize -> IO XPCObject

foreign import ccall unsafe "xpc/xpc.h xpc_dictionary_get_count"
  xpc_dictionary_get_count :: XPCObject -> CSize

foreign import ccall unsafe "xpc/xpc.h xpc_dictionary_set_value"
  xpc_dictionary_set_value :: XPCObject -> CString -> XPCObject -> IO ()

foreign import ccall unsafe "xpc/xpc.h xpc_get_type"
  xpc_get_type :: XPCObject -> XPCType

foreign import ccall unsafe "xpc/xpc.h xpc_int64_create"
  xpc_int64_create :: Int64 -> IO XPCObject

foreign import ccall unsafe "xpc/xpc.h xpc_int64_get_value"
  xpc_int64_get_value :: XPCObject -> Int64

foreign import ccall unsafe "xpc/xpc.h &xpc_release"
  finalizerXPCRelease :: FinalizerPtr XPC
  
foreign import ccall unsafe "xpc/xpc.h xpc_release"
  xpc_release :: XPCObject -> IO ()

foreign import ccall unsafe "xpc/xpc.h xpc_retain"
  xpc_retain :: XPCObject -> IO ()

foreign import ccall unsafe "xpc/xpc.h &_xpc_type_array"
  xpc_type_array :: XPCType

foreign import ccall unsafe "xpc/xpc.h &_xpc_type_dictionary"
  xpc_type_dictionary :: XPCType

foreign import ccall unsafe "xpc/xpc.h &_xpc_type_int64"
  xpc_type_int64 :: XPCType

xpcDescription :: XPCObject -> String
xpcDescription x = unsafePerformIO $ do
  fp <- newForeignPtr finalizerFree (xpc_copy_description x)
  withForeignPtr fp peekCString

withForeignPtrArray :: Storable a => Int -> (Ptr a -> IO b) -> IO b
withForeignPtrArray c f = mallocForeignPtrArray c >>= \p -> withForeignPtr p f

withNewXPCPtr :: IO XPCObject -> (XPCObject -> IO a) -> IO a
withNewXPCPtr xpcObjIO f = do
  x  <- xpcObjIO 
  fp <- newForeignPtr finalizerXPCRelease x
  withForeignPtr fp f

class XPCable a where
  fromXPC :: XPCObject -> IO a
  withXPC :: a -> (XPCObject -> IO b) -> IO b

-- xpc_int64
instance XPCable Int64 where
  fromXPC x | rightType = return $ xpc_int64_get_value x
            | otherwise = error $ printf "fromXPC: invalid type. Expecting %s (int64), got %s" (show xpc_type_int64) (show $ xpc_get_type x)
    where rightType = xpc_get_type x == xpc_type_int64
  withXPC i = withNewXPCPtr (xpc_int64_create i)

-- xpc_array
instance XPCable a => XPCable [a] where
  fromXPC x | rightType = fromXPC `mapM` (xpc_array_get_value x <$> idxRange)
            | otherwise = error $ printf "fromXPC: invalid type. Expecting %s (array), got %s" (show xpc_type_array) (show $ xpc_get_type x)
    where rightType = xpc_get_type x == xpc_type_array
          idxRange | len == 0  = []
                   | otherwise = [0 .. len - 1]
          len = xpc_array_get_count x

  withXPC xs f = 
    withNewXPCPtr (xpc_array_create nullPtr 0) $ \arr -> do
     forM xs $ \x -> withXPC x $ \value -> xpc_array_append_value arr value
     f arr
    where idxRange = [0 .. length xs - 1]


-- xpc_dict
instance XPCable a => XPCable (M.Map String a) where
  fromXPC x | rightType = go
            | otherwise = error $ printf "fromXPC: invalid type. Expecting %s (dict), got %s" (show xpc_type_dictionary) (show $ xpc_get_type x)
    where rightType = xpc_get_type x == xpc_type_dictionary
          go = withForeignPtrArray count $ \keysPtr -> do
               withForeignPtrArray count $ \valuesPtr -> do
                 hsxpc_dictionary_get_keys_and_values x keysPtr valuesPtr
                 keys <- mapM peekCString =<< peekArray count keysPtr
                 values <- mapM fromXPC =<< peekArray count valuesPtr
                 return $ M.fromList $ zip keys values
          count = fromIntegral $ xpc_dictionary_get_count x

  withXPC m f =
    withNewXPCPtr (xpc_dictionary_create nullPtr nullPtr 0) $ \dict -> do
      forM (M.assocs m) $ \(k, v) -> withCString k $ \key -> withXPC v $ \value ->
        xpc_dictionary_set_value dict key value
      f dict

testArr :: [Int64] -> IO [Int64]
testArr xs = withXPC xs fromXPC

testDict :: M.Map String Int64 -> IO (M.Map String Int64)
testDict m = withXPC m fromXPC
