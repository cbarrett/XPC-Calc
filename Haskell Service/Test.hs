module Main where
import Control.Monad
import qualified Data.ByteString.Lazy as L
import Data.Functor
import Data.Int
import qualified Data.Map as M
import Test.QuickCheck
import Test.QuickCheck.Property
import Text.PrettyPrint
import Text.Printf
import System.IO
import System.IO.Unsafe
import XPC

{-
instance Show AnyXPC where
  show (XPCInt64 x) = show x
  show (XPCString x) = show x
  show xs@(XPCList _) = render (go xs)
    where go (XPCList xs) = brackets $ vcat $ punctuate (text ",") $ map go xs
          go x        = (text . show) x
  show (XPCMap m) = show m
-}


-- temp
instance XPCable L.ByteString where
  fromXPC = undefined
  withXPC = undefined

instance Arbitrary L.ByteString where
  arbitrary = L.pack <$> arbitrary 

newtype NoNulStr = MkNoNul String deriving (Show, Eq, Ord)

unNoNul :: NoNulStr -> String
unNoNul (MkNoNul s) = s

instance Arbitrary NoNulStr where
  arbitrary = ((liftM MkNoNul) . listOf . elements . concat) [['a'..'z'], ['A'..'Z'], ['0'..'9']]

data AnyXPC = XPCInt64 Int64
            | XPCString L.ByteString
            | XPCList [AnyXPC]
            | XPCMap (M.Map String AnyXPC)
            deriving (Show, Eq)

instance Arbitrary AnyXPC where
  arbitrary = sized arbitrary'

arbitrary' 0 = scalars
arbitrary' n = frequency [ (length scalars', scalars)
                         , (1, liftM XPCList listItems)
                         , (1, liftM (XPCMap . M.mapKeys unNoNul . M.fromList) assocListItems)
                         ]
  where listItems = do len <- choose (0, n)
                       replicateM len $ arbitrary' (sqrtDown n)
        assocListItems :: Gen [(NoNulStr, AnyXPC)]
        assocListItems = do len <- choose (0, n)
                            keys <- vector len
                            vals <- replicateM len $ arbitrary' (sqrtDown n)
                            return $ zip keys vals
    
        sqrtDown = floor . sqrt . fromIntegral
          
scalars  = oneof scalars'
scalars' = [ liftM XPCInt64 arbitrary
           --, liftM XPCString arbitrary
           ]

instance XPCable AnyXPC where
  fromXPC x 
    | t == xpc_type_int64 = XPCInt64 (fromXPC x)
{-    | t == xpc_type_string = XPCString (fromXPC x) -}
    | t == xpc_type_array = XPCList (fromXPC x)
    | t == xpc_type_dictionary = XPCMap (fromXPC x)
    | otherwise = error $ printf "unexpected type %s" (show t)
    where t = xpc_get_type x
  
  withXPC (XPCInt64   x) = withXPC x
  --withXPC (XPCString  x) = withXPC x
  withXPC (XPCList   xs) = withXPC xs
  withXPC (XPCMap     m) = withXPC m

prop_roundtrip :: AnyXPC -> Property
prop_roundtrip x = morallyDubiousIOProperty $ do
                     putStrLn $ "Trying: " ++ (show x)
                     x' <- roundtrip x
                     return (x == x')

roundtrip :: (XPCable a) => a -> IO a
roundtrip x = withXPC x (return . fromXPC)

main = hSetBuffering stdout NoBuffering >> hSetBuffering stderr NoBuffering >>
       verboseCheck prop_roundtrip
