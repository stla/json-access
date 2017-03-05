{-# LANGUAGE OverloadedStrings #-}
module JsonAccessR.JsonAccess
  where
import           Control.Lens                  ((^?), (^@..))
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

jsonMembers :: String -> String
jsonMembers json = (unpackChars . encode) $ json ^@.. members

-- not used
jsonElemAt_raw :: String -> [Int] -> Array
jsonElemAt_raw json indices =
  V.fromList $ map (\i -> fromMaybe Null (json ^? nth i)) indices

jsonElemAt :: String -> Int -> String
jsonElemAt json index = (unpackChars . encode) $ json ^? nth index

jsonElemsAt :: String -> [Int] -> String
jsonElemsAt json indices = (unpackChars . encode) $ map (\i -> json ^? nth i) indices
--
jsonElemAt_ifNumber :: String -> Int -> String
jsonElemAt_ifNumber json index = (unpackChars . encode) $ json ^? nth index . _Number

jsonElemAt_ifObject :: String -> Int -> String
jsonElemAt_ifObject json index = (unpackChars . encode) $ json ^? nth index . _Object

jsonElemAt_ifArray :: String -> Int -> String
jsonElemAt_ifArray json index = (unpackChars . encode) $ json ^? nth index . _Array

jsonElemAt_ifString :: String -> Int -> String
jsonElemAt_ifString json index = (unpackChars . encode) $ json ^? nth index . _String

jsonElemAt_ifBool :: String -> Int -> String
jsonElemAt_ifBool json index = (unpackChars . encode) $ json ^? nth index . _Bool

tests :: [Bool]
tests = [test1, test2, test3, test4, test5, test6, test7, test8,
         test9, test10, test11, test12, test13, test14, test15,
         test16, test17, test18]
  where json = "{\"a\":{\"b\":8,\"c\":[1,2]},\"b\":null}"
        test1 = jsonAccess json ["a"] == "{\"b\":8,\"c\":[1,2]}"
        test2 = jsonAccess json ["b"] == "null"
        test3 = jsonAccess json ["a","b"] == "8"
        test4 = jsonAccess json ["z"] == "null"
        y = "[\"a\",1,null,[0,1],null,{},true]"
        test5 = jsonElemAt y 1 == "1"
        test6 = jsonElemAt y 15 == "null"
        test7 = jsonElemsAt y [0,1] == "[\"a\",1]"
        test8 = jsonElemAt (jsonAccess json ["a","c"]) 1 == "2"
        test9 = jsonElemAt_ifNumber y 0 == "null"
        test10 = jsonElemAt_ifNumber y 1 == "1"
        test11 = jsonElemAt_ifObject y 5 == "{}"
        test12 = jsonElemAt_ifObject y 1 == "null"
        test13 = jsonElemAt_ifArray y 3 == "[0,1]"
        test14 = jsonElemAt_ifArray y 0 == "null"
        test15 = jsonElemAt_ifString y 0 == "\"a\""
        test16 = jsonElemAt_ifString y 1 == "null"
        test17 = jsonElemAt_ifBool y 0 == "null"
        test18 = jsonElemAt_ifBool y 6 == "true"
