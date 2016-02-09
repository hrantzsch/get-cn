module Article where

import           Control.Monad
import           Data.Char
import           Data.List
import           Network.HTTP
import           Site                     (openURL)
import           Text.HTML.TagSoup

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

splitParagraph :: String -> [String]
splitParagraph "" = []
splitParagraph s = a : splitParagraph (rmDot b)
  where
    (a, b) = break (=='。') s
    rmDot "" = ""
    rmDot (b:bs) = case b of
      '。' -> bs
      _    -> b:bs

sentences :: [Tag String] -> [String]
sentences = splitParagraphs . hasText . map fromTagText . filter isTagText
  where
    hasText = filter $ any isLetter
    splitParagraphs = foldr ((++) . splitParagraph) []

scrapeArticle :: String -> String -> IO [String]
scrapeArticle articleId url = do
  tags <- getArticleTags url articleId
  return $ sentences tags
