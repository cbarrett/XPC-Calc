{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls, TypeSynonymInstances #-}

module XPC
  ( Int64
  , withNewXPCPtr
  , XPC
  , XPCable(..)
  , XPCObject
  ) where

import Control.Monad
import Data.Functor
import Data.Int (Int64)
import Foreign
import Foreign.C.Types 
import Foreign.C.String
import Foreign.Storable

data XPC
type XPCObject = Ptr XPC

foreign import ccall unsafe "xpc/xpc.h xpc_array_create"
  xpc_array_create :: Ptr XPCObject -> CSize -> IO XPCObject

foreign import ccall unsafe "xpc/xpc.h xpc_array_get_count"
  xpc_array_get_count :: XPCObject -> CSize

foreign import ccall unsafe "xpc/xpc.h xpc_array_get_value"
  xpc_array_get_value ::  XPCObject -> CSize -> XPCObject

foreign import ccall unsafe "xpc/xpc.h xpc_array_set_value"
  xpc_array_set_value :: XPCObject -> CSize -> XPCObject -> IO ()

foreign import ccall unsafe "xpc/xpc.h xpc_int64_create"
  xpc_int64_create :: Int64 -> IO XPCObject

foreign import ccall unsafe "xpc/xpc.h xpc_int64_get_value"
  xpc_int64_get_value :: XPCObject -> Int64

foreign import ccall unsafe "xpc/xpc.h &xpc_release"
  finalizerXPCRelease :: FunPtr (XPCObject -> IO ())

withNewXPCPtr xpcObjIO f = xpcObjIO >>= newForeignPtr finalizerXPCRelease >>= (flip withForeignPtr) f

class XPCable a where
  fromXPC :: XPCObject -> a
  withXPC :: a -> (XPCObject -> IO b) -> IO b

-- xpc_int64
instance XPCable Int64 where
  fromXPC   = xpc_int64_get_value
  withXPC i = withNewXPCPtr (xpc_int64_create i)

-- xpc_array
instance (XPCable a) => XPCable [a] where
  fromXPC x = fromXPC . xpc_array_get_value x <$> idxRange
    where idxRange | len == 0  = []
                   | otherwise = [0 .. len - 1]
          len = xpc_array_get_count x

  withXPC xs f = allocaArray (length xs) $ \buf -> do
      mapM (\(idx, p) -> p buf idx) (zip idxRange pokes)
      withNewXPCPtr (xpc_array_create buf (fromIntegral $ length xs)) f
    where idxRange = [0 .. length xs - 1]
          pokes = (\x -> (\buf idx -> withXPC x (pokeElemOff buf idx))) <$> xs

test :: [Int64] -> IO [Int64]
test xs = withXPC xs (return . fromXPC)