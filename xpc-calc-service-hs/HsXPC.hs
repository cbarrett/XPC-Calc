{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module HsXPC where

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

sendReply :: XPCConnection -> XPCObject -> (XPCObject -> IO ()) -> IO ()
sendReply peer event f =
  if eventType == xpc_type_error then return ()
  else do
    replyIO <- xpc_dictionary_create_reply event
    replyF  <- newForeignPtr finalizerXPCRelease $ replyIO
    withForeignPtr replyF $ \reply -> do
      f reply
      xpc_connection_send_message peer reply
  where eventType = xpc_get_type event

hsEventHandler :: XPCConnection -> XPCObject -> IO ()
hsEventHandler peer event = sendReply peer event $ \reply -> do
  withCString "message" $ \key       -> do
  withCString "Hello World" $ \value -> do
    xpc_dictionary_set_string reply key value
