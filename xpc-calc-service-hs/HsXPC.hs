{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module HsXPC
  ( hsEventHandler
  ) where

import Control.Monad
import Foreign
import Foreign.C.Types 
import Foreign.C.String

data XPC
type XPCConnection = Ptr XPC 
type XPCObject = Ptr XPC
type XPCType = Ptr XPC

foreign export ccall hsEventHandler :: XPCConnection -> XPCObject -> IO ()

foreign import ccall unsafe "xpc/xpc.h xpc_get_type"
  xpc_get_type :: XPCObject -> XPCType
  
foreign import ccall unsafe "xpc/xpc.h &_xpc_type_error"
  xpc_type_error :: XPCType

foreign import ccall unsafe "xpc/xpc.h xpc_dictionary_create_reply"
  xpc_dictionary_create_reply :: XPCObject -> IO XPCObject

foreign import ccall unsafe "xpc/xpc.h xpc_dictionary_set_string"
  xpc_dictionary_set_string :: XPCObject -> CString -> CString -> IO ()

foreign import ccall unsafe "xpc/connection.h xpc_connection_send_message"
  xpc_connection_send_message :: XPCConnection -> XPCObject -> IO ()

foreign import ccall unsafe "xpc/xpc.h &xpc_release"
  finalizerXPCRelease :: FunPtr (XPCObject -> IO ())

sendReply :: XPCConnection -> XPCObject -> (XPCObject -> [(String, String)]) -> IO ()
sendReply peer event f =
  if eventType == xpc_type_error then return ()
  else do
    replyF <- newForeignPtr finalizerXPCRelease =<< xpc_dictionary_create_reply event
    withForeignPtr replyF $ \reply -> do
      mapM (buildReplyDict reply) (f reply)
      xpc_connection_send_message peer reply
  where eventType = xpc_get_type event

buildReplyDict :: XPCObject -> (String, String) -> IO ()
buildReplyDict reply (k, v) = do
  withCString k $ \key   -> do
  withCString v $ \value -> do
    xpc_dictionary_set_string reply key value

hsEventHandler :: XPCConnection -> XPCObject -> IO ()
hsEventHandler peer event = sendReply peer event $ \_ -> [("message", "Hello World")]