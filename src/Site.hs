module Site where

import           Codec.Binary.UTF8.String (decodeString)
import           Control.Exception
import           Control.Monad
import           Data.List                as L
import           Network.HTTP
import           Network.Stream
import           Text.HTML.TagSoup

openURL :: String -> IO String
openURL x = do
  r <- try (simpleHTTP $ getRequest x) :: IO (Either SomeException (Result (Response String)))
  case r of
    Left _    -> openURL $ sanitize x
    Right req -> liftM decodeString $ getResponseBody req

getLinks :: [Tag String] -> [Tag String]
getLinks = filter (~== TagOpen "a" [])

getLinksMatching :: String -> (String -> Bool) -> IO [String]
getLinksMatching url filterPattern = do
  site <- openURL url
  let links = map (fromAttrib "href") (getLinks $ parseTags site)
  return $ filter filterPattern links

sanitize :: String -> String
-- just removes trailing space atm
sanitize = reverse . maybeRmSpace . reverse
  where maybeRmSpace (x:xs) = case x of
          ' ' -> xs
          _   -> x:xs
