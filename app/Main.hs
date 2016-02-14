module Main where

import           Article             (scrapeArticle)
import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.Char           as C
import qualified Data.HashMap.Strict as M
import           Data.Time.Clock
import           Site                (getLinksMatching)
import           System.IO
import           Text.Regex.Posix

type LinkMap = M.HashMap String Char


main :: IO ()
main = do
  let linksFile = "links.txt"
  let dataFile = "data"
  l <- try (readFile linksFile) :: IO (Either SomeException String)
  case l of
    Left _      -> loop dataFile linksFile M.empty
    Right links -> loop dataFile linksFile $ linkMapFromList $ lines links


loop :: String -> String -> LinkMap -> IO ()
loop dataFile linksFile known = do
  putStrLn $ replicate 40 '='

  -- find and process new links
  new <- run dataFile known

  -- update links file
  lf <- openFile linksFile AppendMode
  mapM_ (hPutStrLn lf) (M.keys new)
  hClose lf

  -- wait, then start over
  let delaySec = 3600
  putStrLn $ "Running again in " ++ show (fromIntegral delaySec / 60) ++ " minutes."
  threadDelay $ delaySec * 1000000
  loop dataFile linksFile (M.union new known)


run :: String -> LinkMap -> IO LinkMap
run d known = do
  links <- liftM linkMapFromList $ getLinksMatching "http://news.sina.com.cn" (=~ "news.sina.com.cn/c/nd")
  let unknown = M.filterWithKey (\ k _ -> not $ M.member k known) links
  putStrLn $ "Found " ++ show (length links) ++ " links -- " ++ show (length unknown) ++ " new."

  date <- liftM utctDay getCurrentTime
  let filename = d ++ "_" ++ show date ++ ".txt"
  forM_ (M.keys unknown) (\link -> do
    articles <- scrapeArticle "artibody" link
    df <- openFile filename AppendMode
    mapM_ (hPutStrLn df) (curate articles)
    hClose df
    )

  return unknown


curate :: [String] -> [String]
curate = filter conditions . map (filter $ not . C.isSpace)
 where conditions s =
        "" /= s
        && all C.isPrint s
        && all (not . C.isSymbol) s


linkMapFromList :: [String] -> LinkMap
linkMapFromList l = M.fromList $ zip l $ repeat '0'
