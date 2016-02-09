module Main where

import           Article          (scrapeArticle)
import           Site             (getLinksMatching)
import           Text.Regex.Posix

-- main :: IO ()
main = do
  links <- getLinksMatching "http://news.sina.com.cn" (=~ "news.sina.com.cn/c/nd")
  mapM (scrapeArticle "artibody") links
