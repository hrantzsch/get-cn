module Support (Config(..), getConfig) where

import           Options.Applicative


data Config = Config
  { datafile  :: Maybe String
  , linksfile :: Maybe String
  , interval  :: Maybe Int }


config :: Parser Config
config = Config
      <$> optional (strOption
          ( long "data"
         <> short 'd'
         <> metavar "DATAFILE"
         <> help "Filename for data output. Will be extended by '_<date>.txt'."))
      <*> optional (strOption
          ( long "links"
         <> short 'l'
         <> metavar "LINKSFILE"
         <> help "Filename to look for and store processed links in."))
      <*> optional (option auto
          ( long "interval"
         <> short 'i'
         <> metavar "MINUTES"
         <> help "Interval to rerun the scraper in minutes."))


getConfig :: IO Config
getConfig = execParser opts
  where opts = info (helper <*> config)
          ( fullDesc
         <> progDesc "Scrape news articles from news.sina.com.cn")
