{-# LANGUAGE ForeignFunctionInterface, EmptyDataDecls #-}

module HsXPC where

import Foreign
import Foreign.C.Types 
import Foreign.C.String

data XPC
type XPCConnection = Ptr XPC
type XPCObject = Ptr XPC

foreign export ccall hsEventHandler :: XPCConnection -> XPCObject -> IO ()

foreign import ccall unsafe "xpc/xpc.h xpc_dictionary_create_reply"
    xpc_dictionary_create_reply :: XPCObject -> IO XPCObject

foreign import ccall unsafe "xpc/xpc.h xpc_dictionary_set_string"
    xpc_dictionary_set_string :: XPCObject -> CString -> CString -> IO ()

foreign import ccall unsafe "xpc/connection.h xpc_connection_send_message"
    xpc_connection_send_message :: XPCConnection -> XPCObject -> IO ()

foreign import ccall unsafe "xpc/xpc.h xpc_release"
    xpc_release :: XPCObject -> IO ()

hsEventHandler :: XPCConnection -> XPCObject -> IO ()
hsEventHandler peer event = do
    reply <- xpc_dictionary_create_reply event

    withCString "message" $ \key -> do
        withCString "Hello World" $ \value -> do
            xpc_dictionary_set_string reply key value
    
    xpc_connection_send_message peer reply
    xpc_release reply