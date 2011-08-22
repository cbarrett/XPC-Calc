{-# LANGUAGE EmptyDataDecls, ForeignFunctionInterface, FlexibleInstances #-}

module XPC
  ( Int64
  , withNewXPCPtr
  , XPC
  , XPCable(..)
  , XPCObject
  , XPCType
  , xpc_get_type -- should have our own ADT for this instead of just exporting the function
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

foreign import ccall unsafe "xpc/xpc.h xpc_array_create"
  xpc_array_create :: Ptr XPCObject -> CSize -> IO XPCObject

foreign import ccall unsafe "xpc/xpc.h xpc_array_get_count"
  xpc_array_get_count :: XPCObject -> CSize

foreign import ccall unsafe "xpc/xpc.h xpc_array_get_value"
  xpc_array_get_value ::  XPCObject -> CSize -> XPCObject

foreign import ccall unsafe "xpc/xpc.h xpc_array_set_value"
  xpc_array_set_value :: XPCObject -> CSize -> XPCObject -> IO ()

foreign import ccall unsafe "xpc/xpc.h xpc_dictionary_create"
  xpc_dictionary_create :: Ptr CString -> Ptr XPCObject -> CSize -> IO XPCObject

foreign import ccall unsafe "xpc/xpc.h xpc_dictionary_get_count"
  xpc_dictionary_get_count :: XPCObject -> CSize

foreign import ccall unsafe "xpc/xpc.h xpc_get_type"
  xpc_get_type :: XPCObject -> XPCType

foreign import ccall unsafe "xpc/xpc.h xpc_int64_create"
  xpc_int64_create :: Int64 -> IO XPCObject

foreign import ccall unsafe "xpc/xpc.h xpc_int64_get_value"
  xpc_int64_get_value :: XPCObject -> Int64

foreign import ccall unsafe "xpc/xpc.h &xpc_release"
  finalizerXPCRelease :: FunPtr (XPCObject -> IO ())

foreign import ccall unsafe "xpc/xpc.h &_xpc_type_array"
  xpc_type_array :: XPCType

foreign import ccall unsafe "xpc/xpc.h &_xpc_type_dictionary"
  xpc_type_dictionary :: XPCType

foreign import ccall unsafe "xpc/xpc.h &_xpc_type_int64"
  xpc_type_int64 :: XPCType

withNewXPCPtr :: IO XPCObject -> (XPCObject -> IO a) -> IO a
withNewXPCPtr xpcObjIO f = xpcObjIO >>= newForeignPtr finalizerXPCRelease >>= (flip withForeignPtr) f

class XPCable a where
  fromXPC :: XPCObject -> a
  withXPC :: a -> (XPCObject -> IO b) -> IO b

-- xpc_int64
instance XPCable Int64 where
  fromXPC x | rightType = xpc_int64_get_value x
            | otherwise = error $ printf "fromXPC: invalid type. Expecting %s (int64), got %s" (show xpc_type_int64) (show $ xpc_get_type x)
    where rightType = xpc_get_type x == xpc_type_int64
  withXPC i = withNewXPCPtr (xpc_int64_create i)

-- xpc_array
instance (XPCable a) => XPCable [a] where
  fromXPC x | rightType = fromXPC . xpc_array_get_value x <$> idxRange
            | otherwise = error $ printf "fromXPC: invalid type. Expecting %s (array), got %s" (show xpc_type_array) (show $ xpc_get_type x)
    where rightType = xpc_get_type x == xpc_type_array
          idxRange | len == 0  = []
                   | otherwise = [0 .. len - 1]
          len = xpc_array_get_count x

  withXPC xs f = allocaArray (length xs) $ \buf -> do
      zipWithM (\x idx -> withXPC x $ pokeElemOff buf idx) xs idxRange
      withNewXPCPtr (xpc_array_create buf (fromIntegral $ length xs)) f
    where idxRange = [0 .. length xs - 1]

-- xpc_dict
instance (XPCable a) => XPCable (M.Map String a) where
  fromXPC x | rightType = unsafePerformIO go
            | otherwise = error $ printf "fromXPC: invalid type. Expecting %s (dict), got %s" (show xpc_type_dictionary) (show $ xpc_get_type x)
    where rightType = xpc_get_type x == xpc_type_dictionary
          go = allocaArray count $ \keysPtr -> do
               allocaArray count $ \valuesPtr -> do
                 hsxpc_dictionary_get_keys_and_values x keysPtr valuesPtr
                 keys <- mapM peekCString =<< peekArray count keysPtr
                 values <- map fromXPC <$> peekArray count valuesPtr
                 return $ M.fromList $ zip keys values
          count = fromIntegral $ xpc_dictionary_get_count x

  withXPC m f =
    allocaArray count $ \keyBuf -> do
    allocaArray count $ \valBuf -> do
      zipWithM (\x idx -> withCString x $ pokeElemOff keyBuf idx) (M.keys m) idxs
      zipWithM (\x idx -> withXPC x $ pokeElemOff valBuf idx) (M.elems m) idxs
      withNewXPCPtr (xpc_dictionary_create keyBuf valBuf (fromIntegral count)) f
    where count = M.size m
          idxs  = [0 .. count - 1]

testArr :: [Int64] -> IO [Int64]
testArr xs = withXPC xs (return . fromXPC)

testDict :: M.Map String Int64 -> IO (M.Map String Int64)
testDict m = withXPC m (return . fromXPC)
