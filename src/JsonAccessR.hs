{-# LANGUAGE DataKinds                #-}
{-# LANGUAGE ForeignFunctionInterface #-}
module JsonAccessR
  where
import           Data.Text              (pack)
import qualified Data.Vector.SEXP       as DV
import           Foreign
import           Foreign.C
import           Foreign.R              (SEXP)
import qualified Foreign.R.Type         as R
import           JsonAccessR.JsonAccess

foreign export ccall jsonAccessR :: Ptr CString -> Ptr CString -> Ptr CInt -> Ptr CString -> IO ()
jsonAccessR :: Ptr CString -> Ptr CString -> Ptr CInt -> Ptr CString -> IO ()
jsonAccessR json path lpath result = do
  json <- (>>=) (peek json) peekCString
  lpath <- peek lpath
  pathC <- peekArray (fromIntegral lpath :: Int) path
  path <- mapM peekCString pathC
  let out = jsonAccess json (map pack path)
  (>>=) (newCString out) (poke result)

foreign export ccall jsonElemsAtR :: Ptr CString -> Ptr CInt -> Ptr CInt -> Ptr CString -> IO ()
jsonElemsAtR :: Ptr CString -> Ptr CInt -> Ptr CInt -> Ptr CString -> IO ()
jsonElemsAtR json indices lindices result = do
  json <- (>>=) (peek json) peekCString
  lindices <- peek lindices
  indices <- peekArray (fromIntegral lindices :: Int) indices
  let out = jsonElemsAt json (map cintToInt indices)
  (>>=) (newCString out) (poke result)

foreign export ccall jsonElemAtR :: Ptr CString -> Ptr CInt -> Ptr CString -> IO ()
jsonElemAtR :: Ptr CString -> Ptr CInt -> Ptr CString -> IO ()
jsonElemAtR json index result = do
  json <- (>>=) (peek json) peekCString
  index <- peek index
  let out = jsonElemAt json (fromIntegral index :: Int)
  (>>=) (newCString out) (poke result)

foreign export ccall jsonElemAt_ifNumberR :: Ptr CString -> Ptr CInt -> Ptr CString -> IO ()
jsonElemAt_ifNumberR :: Ptr CString -> Ptr CInt -> Ptr CString -> IO ()
jsonElemAt_ifNumberR json index result = do
  json <- (>>=) (peek json) peekCString
  index <- peek index
  let out = jsonElemAt_ifNumber json (fromIntegral index :: Int)
  (>>=) (newCString out) (poke result)

foreign export ccall jsonElemAt_ifObjectR :: Ptr CString -> Ptr CInt -> Ptr CString -> IO ()
jsonElemAt_ifObjectR :: Ptr CString -> Ptr CInt -> Ptr CString -> IO ()
jsonElemAt_ifObjectR json index result = do
  json <- (>>=) (peek json) peekCString
  index <- peek index
  let out = jsonElemAt_ifObject json (fromIntegral index :: Int)
  (>>=) (newCString out) (poke result)

foreign export ccall jsonElemAt_ifArrayR :: Ptr CString -> Ptr CInt -> Ptr CString -> IO ()
jsonElemAt_ifArrayR :: Ptr CString -> Ptr CInt -> Ptr CString -> IO ()
jsonElemAt_ifArrayR json index result = do
  json <- (>>=) (peek json) peekCString
  index <- peek index
  let out = jsonElemAt_ifArray json (fromIntegral index :: Int)
  (>>=) (newCString out) (poke result)

foreign export ccall jsonElemAt_ifStringR :: Ptr CString -> Ptr CInt -> Ptr CString -> IO ()
jsonElemAt_ifStringR :: Ptr CString -> Ptr CInt -> Ptr CString -> IO ()
jsonElemAt_ifStringR json index result = do
  json <- (>>=) (peek json) peekCString
  index <- peek index
  let out = jsonElemAt_ifString json (fromIntegral index :: Int)
  (>>=) (newCString out) (poke result)

foreign export ccall jsonElemAt_ifBoolR :: Ptr CString -> Ptr CInt -> Ptr CString -> IO ()
jsonElemAt_ifBoolR :: Ptr CString -> Ptr CInt -> Ptr CString -> IO ()
jsonElemAt_ifBoolR json index result = do
  json <- (>>=) (peek json) peekCString
  index <- peek index
  let out = jsonElemAt_ifBool json (fromIntegral index :: Int)
  (>>=) (newCString out) (poke result)

foreign export ccall jsonMembersR :: Ptr CString -> Ptr CString -> IO ()
jsonMembersR :: Ptr CString -> Ptr CString -> IO ()
jsonMembersR json result = do
  json <- (>>=) (peek json) peekCString
  let out = jsonMembers json
  (>>=) (newCString out) (poke result)

foreign export ccall jsonPrettyR :: Ptr CString -> Ptr CString -> Ptr CString -> Ptr CString -> IO ()
jsonPrettyR :: Ptr CString -> Ptr CString -> Ptr CString -> Ptr CString -> IO ()
jsonPrettyR json indent format result = do
  json <- (>>=) (peek json) peekCString
  indent <- (>>=) (peek indent) peekCString
  format <- (>>=) (peek format) peekCString
  let out = jsonPretty json indent format
  (>>=) (newCString out) (poke result)


cintToInt :: CInt -> Int
cintToInt i = (fromIntegral i :: Int)

intToInt32 :: Int -> Int32
intToInt32 i = fromIntegral (i :: Int) :: Int32
