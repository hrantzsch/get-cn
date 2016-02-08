module Site where

import           Codec.Binary.UTF8.String (decodeString)
import           Control.Monad
import           Network.HTTP
import           Text.HTML.TagSoup

openURL :: String -> IO String
openURL x = liftM decodeString $ getResponseBody =<< simpleHTTP (getRequest x)

getLinks = undefined
