{-# LANGUAGE OverloadedStrings #-}
module JsonAccessR.JsonAccess
  where
import           Control.Lens                  ((^?))
import           Data.Aeson                    (encode)
import           Data.Aeson.Lens
import           Data.Aeson.Types
import           Data.ByteString               (ByteString)
import           Data.ByteString.Lazy.Internal (unpackChars)
import           Data.Text                     (Text)
import qualified Data.Vector as V
import Data.Maybe (fromMaybe)

sencode :: Maybe Value -> String
sencode = (unpackChars . encode)

x = ("{ \"a\": { \"b\": 8} }" :: String) ^? (foldl (.) _Value $ map key ["a", "b"])

jsonAccess :: String -> [Text] -> String
jsonAccess json path = sencode $ json ^? (foldl (.) _Value $ map key path)

jsonElemAt_raw :: String -> [Int] -> Array
jsonElemAt_raw json indices =
  V.fromList $ map (\i -> fromMaybe Null (json ^? nth i)) indices

-- String -> Int c'est mieux pour chaining avec %>%
-- faire [Int] pour extraire plusieurs indices

jsonElemAt :: String -> Int -> String
jsonElemAt json index = (unpackChars . encode) $ json ^? nth index

jsonElemsAt :: String -> [Int] -> String
jsonElemsAt json indices = (unpackChars . encode) $ map (\i -> json ^? nth i) indices
--
-- jsonElemAtif_number :: String -> [Int] -> String
-- jsonElemAtif_number json index = (unpackChars . encode) $ json ^? nth index . _Number
--
-- jsonElemAtif_notNull :: String -> [Int] -> String
-- jsonElemAtif_notNull json index = (unpackChars . encode) $ json ^? nth index . _Object
--
-- jsonElemAtif_array :: String -> [Int] -> String
-- jsonElemAtif_array json index = (unpackChars . encode) $ json ^? nth index . _Array

tests :: [Bool]
tests = [test1, test2, test3, test4, test5, test6, test7, test8]
  where json = "{\"a\":{\"b\":8,\"c\":[1,2]},\"b\":null}"
        test1 = jsonAccess json ["a"] == "{\"b\":8,\"c\":[1,2]}"
        test2 = jsonAccess json ["b"] == "null"
        test3 = jsonAccess json ["a","b"] == "8"
        test4 = jsonAccess json ["z"] == "null"
        y = "[\"a\",1,null]"
        test5 = jsonElemAt y 1 == "1"
        test6 = jsonElemAt y 5 == "null"
        test7 = jsonElemsAt y [0,1] == "[\"a\",1]"
        test8 = jsonElemAt (jsonAccess json ["a","c"]) 1 == "2"
