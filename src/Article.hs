{-# LANGUAGE OverloadedStrings #-}

module Article where

import           Control.Exception
import qualified Data.Text         as T
import           Site              (cursorFor)
import           Text.XML.Cursor

scrapeArticle :: String -> String -> IO [String]
scrapeArticle targetId url = do
  c <- cursorFor url
  let artibody = c $// attributeIs "id" (T.pack targetId)
  case artibody of
    [] -> do
      putStrLn $ "Warning: " ++ targetId ++ " not found in article " ++ url
      return []
    _  -> return $ concatMap (splitParagraph . T.unpack) (head artibody $// content)

splitParagraph :: String -> [String]
splitParagraph "" = []
splitParagraph s = a : splitParagraph b
  where
    seperators = ['。', '，', '、', '；']
    (a, b) = break (`elem` seperators) $ rmSep s
    rmSep "" = ""
    rmSep (b:bs)
      | b `elem` seperators = bs
      | otherwise = b : bs
