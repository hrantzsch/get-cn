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
  return $ concatMap (splitParagraph . T.unpack) (head artibody $// content)

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

splitParagraph :: String -> [String]
splitParagraph "" = []
splitParagraph s = a : splitParagraph b
  where
    seperators = ['。', '，']
    (a, b) = break (`elem` seperators) $ rmSep s
    rmSep "" = ""
    rmSep (b:bs)
      | b `elem` seperators = bs
      | otherwise = b : bs
