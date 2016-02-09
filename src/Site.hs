module Site where

import           Codec.Binary.UTF8.String (decodeString)
import           Control.Monad
import           Data.List                as L
import           Network.HTTP
import           Text.HTML.TagSoup

openURL :: String -> IO String
openURL x = liftM decodeString $ getResponseBody =<< simpleHTTP (getRequest x)

getLinks :: [Tag String] -> [Tag String]
getLinks = filter (~== TagOpen "a" [])

getLinksMatching :: String -> (String -> Bool) -> IO [String]
getLinksMatching url filterPattern = do
  site <- openURL url
  let links = map (fromAttrib "href") (getLinks $ parseTags site)
  return $ filter filterPattern links
