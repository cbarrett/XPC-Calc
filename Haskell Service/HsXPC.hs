{-# LANGUAGE ForeignFunctionInterface #-}

module HsXPC
  ( hsEventHandler
  ) where

import Control.Monad
import Data.Functor
import Foreign
import Foreign.C.Types 
import Foreign.C.String
import XPC

type XPCConnection = Ptr XPC 

foreign export ccall hsEventHandler :: XPCConnection -> XPCObject -> IO ()
  
foreign import ccall unsafe "xpc/xpc.h &_xpc_type_error"
  xpc_type_error :: XPCType

foreign import ccall unsafe "xpc/xpc.h xpc_dictionary_create_reply"
  xpc_dictionary_create_reply :: XPCObject -> IO XPCObject

foreign import ccall unsafe "xpc/xpc.h xpc_dictionary_get_value"
  xpc_dictionary_get_value :: XPCObject -> CString -> XPCObject

foreign import ccall unsafe "xpc/xpc.h xpc_dictionary_set_value"
  xpc_dictionary_set_value :: XPCObject -> CString -> XPCObject -> IO ()

foreign import ccall unsafe "xpc/connection.h xpc_connection_send_message"
  xpc_connection_send_message :: XPCConnection -> XPCObject -> IO ()

sendReply :: XPCConnection -> XPCObject -> (Int64 -> [Int64] -> [Int64]) -> IO ()
sendReply peer event f = do
  op <- withCString "op" $ return . fromXPC . xpc_dictionary_get_value event
  stack <- withCString "stack" $ return . fromXPC . xpc_dictionary_get_value event

  withNewXPCPtr (xpc_dictionary_create_reply event) $ \reply -> do
  withCString "stack" $ \stackStr -> do
  withXPC (f op stack) $ \newStack -> do
    xpc_dictionary_set_value reply stackStr newStack
    xpc_connection_send_message peer reply

consumeBinary :: (Int64 -> Int64 -> Int64) -> [Int64] -> [Int64]
consumeBinary f xs
  | len >= 2  = xs' ++ [lhs `f` rhs]
  | otherwise = xs
  where (xs', rhs:lhs:_) = splitAt (len - 2) xs
        len = length xs

-- TODO ops should be bound to the values in the enum in Shared.h
calc :: Int64 -> [Int64] -> [Int64]
calc 0 = consumeBinary (+)
calc 1 = consumeBinary (-)
calc 2 = consumeBinary (*)
calc 3 = consumeBinary div
  
hsEventHandler :: XPCConnection -> XPCObject -> IO ()
hsEventHandler peer event = 
  if eventType == xpc_type_error then return ()
  else do
    sendReply peer event calc
  where eventType = xpc_get_type event
