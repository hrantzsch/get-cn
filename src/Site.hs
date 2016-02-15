{-# LANGUAGE OverloadedStrings #-}

module Site where

import           Control.Exception
import qualified Data.ByteString.Lazy.Internal as B
import qualified Data.Text                     as T
import           Network.HTTP.Conduit          (HttpException, simpleHttp)
import           Text.HTML.DOM                 (parseLBS)
import           Text.XML.Cursor

getLinksMatching :: String -> (String -> Bool) -> IO [String]
getLinksMatching url filterPattern = do
  c <- cursorFor url
  let links = c $// attribute "href"
  return $ filter filterPattern $ map T.unpack links

cursorFor :: String -> IO Cursor
cursorFor url = do
     page <- try (simpleHttp url) :: IO (Either HttpException B.ByteString)
     case page of
       Left _  -> cursorFor $ sanitize url
       Right p -> return (fromDocument $ parseLBS p)

sanitize :: String -> String
-- just removes trailing space atm
sanitize = reverse . maybeRmSpace . reverse
 where maybeRmSpace (x:xs) = case x of
        ' ' -> xs
        _   -> x:xs
