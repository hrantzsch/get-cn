module Lib where

import           Codec.Binary.UTF8.String (decodeString)
import           Control.Monad
import           Network.HTTP
import           Text.HTML.TagSoup

openURL :: String -> IO String
openURL x = liftM decodeString $ getResponseBody =<< simpleHTTP (getRequest x)

getArticleTags :: String -> String -> IO [Tag String]
getArticleTags url articleId = do
  src <- openURL url
  return $ takeWhileNotClosed $
           dropWhile (~/= TagOpen "div" [("id", articleId)]) (parseTags src)

takeWhileNotClosed :: [Tag String] -> [Tag String]
takeWhileNotClosed = findClosingTag 0
  where
    findClosingTag n (s:section)
      | isTagOpen s = s : findClosingTag (n+1) section
      | isTagClose s = if n <= 1 then [s]
                                 else s : findClosingTag (n-1) section
      | otherwise = s : findClosingTag n section

splitTags :: [Tag String] -> [String]
splitTags = undefined

getArticle :: String -> String -> IO [String]
getArticle url articleId = do
  tags <- getArticleTags url articleId
  return $ splitTags tags
