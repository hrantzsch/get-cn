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
  c <- cursorFor url
  case c of
    Just cursor -> do
      let links = cursor $// attribute "href"
      return $ filter filterPattern $ map (sanitize . T.unpack) links
    Nothing     -> return []

cursorFor :: String -> IO (Maybe Cursor)
cursorFor url = do
  doc <- try (simpleHttp url) :: IO (Either HttpException B.ByteString)
  case doc of
    Left _ -> do
      putStrLn $ "Warning: could not open url '" ++ url ++ "'"
      return Nothing
    Right d -> return (Just $ fromDocument $ parseLBS d)

sanitize :: String -> String
sanitize url = url =~ ("http.*html" :: B.ByteString)
