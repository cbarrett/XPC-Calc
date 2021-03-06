{-# LANGUAGE ForeignFunctionInterface #-}

module HsXPC
  ( hsEventHandler
  ) where

import Control.Monad
import Data.Map ((!))
import qualified Data.Map as M
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

updateXPCDict :: (XPCable a) => XPCObject -> M.Map String a -> IO ()
updateXPCDict x m = forM_ (M.keys m) $ \k -> do
    withCString k $ \keyStr -> do
    withXPC (m ! k) $ \valXPC -> do
      xpc_dictionary_set_value x keyStr valXPC

sendReply :: XPCConnection -> XPCObject -> (Int64 -> [Int64] -> [Int64]) -> IO ()
sendReply peer eventX f = 
  let event :: (XPCable a) => M.Map String a
      event = fromXPC eventX 
  in withNewXPCPtr (xpc_dictionary_create_reply eventX) $ \reply -> do
       updateXPCDict reply $ M.singleton "stack" $ f (event ! "op") (event ! "stack")
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
