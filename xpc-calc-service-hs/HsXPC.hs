{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module HsXPC
  ( hsEventHandler
  ) where

import Control.Monad
import Data.Functor
import Foreign
import Foreign.C.Types 
import Foreign.C.String

data XPC
type XPCConnection = Ptr XPC 
type XPCObject = Ptr XPC
type XPCType = Ptr XPC

--foreign export ccall hsEventHandler :: XPCConnection -> XPCObject -> IO ()

foreign import ccall unsafe "xpc/xpc.h xpc_get_type"
  xpc_get_type :: XPCObject -> XPCType
  
foreign import ccall unsafe "xpc/xpc.h &_xpc_type_error"
  xpc_type_error :: XPCType

foreign import ccall unsafe "xpc/xpc.h xpc_array_create"
  xpc_array_create :: Ptr XPCObject -> CSize -> IO XPCObject

foreign import ccall unsafe "xpc/xpc.h xpc_array_get_count"
  xpc_array_get_count :: XPCObject -> CSize

foreign import ccall unsafe "xpc/xpc.h xpc_array_get_int64"
  xpc_array_get_int64 ::  XPCObject -> CSize -> CInt

foreign import ccall unsafe "xpc/xpc.h xpc_array_set_int64"
  xpc_array_set_int64 :: XPCObject -> CSize -> CInt -> IO ()

foreign import ccall unsafe "xpc/xpc.h xpc_dictionary_create_reply"
  xpc_dictionary_create_reply :: XPCObject -> IO XPCObject

foreign import ccall unsafe "xpc/xpc.h xpc_dictionary_get_int64"
  xpc_dictionary_get_int64 :: XPCObject -> CString -> CInt

foreign import ccall unsafe "xpc/xpc.h xpc_dictionary_get_value"
  xpc_dictionary_get_value :: XPCObject -> CString -> XPCObject

foreign import ccall unsafe "xpc/xpc.h xpc_dictionary_set_value"
  xpc_dictionary_set_value :: XPCObject -> CString -> XPCObject -> IO ()

foreign import ccall unsafe "xpc/connection.h xpc_connection_send_message"
  xpc_connection_send_message :: XPCConnection -> XPCObject -> IO ()

foreign import ccall unsafe "xpc/xpc.h &xpc_release"
  finalizerXPCRelease :: FunPtr (XPCObject -> IO ())

xpcArrayToList xa = fromIntegral . xpc_array_get_int64 xa <$> [0 .. xpc_array_get_count xa]

withNewXPCPtr xpcObjIO f = xpcObjIO >>= newForeignPtr finalizerXPCRelease >>= (flip withForeignPtr) f

withXPCArray xs f = do
  withNewXPCPtr (xpc_array_create nullPtr 0) $ \stack -> do
    forM xs $ xpc_array_set_int64 stack (-1) . fromIntegral
    f stack

sendReply :: XPCConnection -> XPCObject -> (Int -> [Int] -> [Int]) -> IO ()
sendReply peer event f = do
  op <- withCString "op" $ return . fromIntegral . xpc_dictionary_get_int64 event
  stack <- withCString "stack" $ return . xpcArrayToList . xpc_dictionary_get_value event

  withNewXPCPtr (xpc_dictionary_create_reply event) $ \reply -> do
  withCString "stack" $ \stackStr -> do
  withXPCArray (f op stack) $ \newStack -> do
    xpc_dictionary_set_value reply stackStr newStack
    xpc_connection_send_message peer reply        

hsEventHandler :: XPCConnection -> XPCObject -> IO ()
hsEventHandler peer event = 
  if eventType == xpc_type_error then return ()
  else do 
    sendReply peer event $ \op xs -> [1..5]
  where eventType = xpc_get_type event
