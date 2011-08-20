{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, TypeSynonymInstances #-}

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
import Foreign
import Foreign.C.Types 
import Foreign.C.String
import Foreign.Storable
import Text.Printf

data XPC
type XPCObject = Ptr XPC
type XPCType = Ptr XPC

foreign import ccall unsafe "xpc/xpc.h xpc_array_create"
  xpc_array_create :: Ptr XPCObject -> CSize -> IO XPCObject

foreign import ccall unsafe "xpc/xpc.h xpc_array_get_count"
  xpc_array_get_count :: XPCObject -> CSize

foreign import ccall unsafe "xpc/xpc.h xpc_array_get_value"
  xpc_array_get_value ::  XPCObject -> CSize -> XPCObject

foreign import ccall unsafe "xpc/xpc.h xpc_array_set_value"
  xpc_array_set_value :: XPCObject -> CSize -> XPCObject -> IO ()

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
      mapM (\(idx, p) -> p buf idx) (zip idxRange pokes)
      withNewXPCPtr (xpc_array_create buf (fromIntegral $ length xs)) f
    where idxRange = [0 .. length xs - 1]
          pokes = (\x -> (\buf idx -> withXPC x (pokeElemOff buf idx))) <$> xs

test :: [Int64] -> IO [Int64]
test xs = withXPC xs (return . fromXPC)