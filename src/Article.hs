{-# LANGUAGE OverloadedStrings #-}

module Article where

import           Control.Exception
import qualified Data.ByteString.Lazy.Internal as B
import qualified Data.Text                     as T
import           Network.HTTP.Conduit          (HttpException, simpleHttp)
import           Text.HTML.DOM                 (parseLBS)
import           Text.XML.Cursor

scrapeArticle :: String -> String -> IO [String]
scrapeArticle targetId url = do
  c <- cursorFor url
  let artibody = c $// attributeIs "id" (T.pack targetId)
  return $ map T.unpack (head artibody $// content)

cursorFor :: String -> IO Cursor
cursorFor u = do
     page <- try (simpleHttp u) :: IO (Either HttpException B.ByteString)
     case page of
       Left _  -> cursorFor $ sanitize u
       Right p -> return (fromDocument $ parseLBS p)

sanitize :: String -> String
-- just removes trailing space atm
sanitize = reverse . maybeRmSpace . reverse
 where maybeRmSpace (x:xs) = case x of
        ' ' -> xs
        _   -> x:xs
