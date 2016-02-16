{-# LANGUAGE OverloadedStrings #-}

module Site where

import           Control.Exception
import qualified Data.ByteString.Lazy.Internal as B
import qualified Data.Text                     as T
import           Network.HTTP.Conduit          (HttpException, simpleHttp)
import           Text.HTML.DOM                 (parseLBS)
import           Text.XML.Cursor
import           Text.Regex.Posix

getLinksMatching :: String -> (String -> Bool) -> IO [String]
getLinksMatching url filterPattern = do
  putStrLn $ "opening " ++ url
  c <- try (cursorFor url) :: IO (Either HttpException Cursor)
  case c of
   Left _  -> do
      putStrLn $ "Error: could not open url '" ++ url ++ "'"
      return []
   Right cursor -> do
      let links = cursor $// attribute "href"
      return $ filter filterPattern $ map (sanitize . T.unpack) links

cursorFor :: String -> IO Cursor
cursorFor url = do
  doc <- simpleHttp url
  return (fromDocument $ parseLBS doc)

sanitize :: String -> String
sanitize url = url =~ ("http.*html" :: B.ByteString)
