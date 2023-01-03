{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeOperators              #-}

module Main where

import           BasicPrelude                         hiding (FilePath)
import           Control.Arrow                        ((>>>))
import           Control.Lens
import           Control.Monad.Trans.Maybe
import           Data.Aeson
import qualified Data.ByteString.Lazy                 as BS
import           Data.Ratio
import           Data.Text                            (pack)
import           Loggia.APIType
import           Loggia.Core
import           Loggia.IO.Parser
import           Loggia.IO.Render
import           Loggia.Logger
import           Loggia.Mockup
import           Loggia.Types
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Network.Wai.Middleware.Gzip          (def, gzip)
import           Network.Wai.Middleware.RequestLogger
import           Options.Applicative                  as OptA
import           Servant
import           Servant.API
import           Servant.Server
import           System.Directory
import           System.FilePath
import           System.Random

--------------------------------------------------------------------------------

getFilesByExtensions :: [FilePath] -> FilePath -> IO [FilePath]
getFilesByExtensions exts folder = go <$> getDirectoryContents folder where
  go = (fmap $ (<>) folder ) . filter (\x -> takeExtension x `elem` exts)

getMockups :: Text -> IO [Mockup1]
getMockups path = textToString >>> (getFilesByExtensions [".svg"] >=> go) $ path
  where
    go paths = catMaybes <$> ( sequence $ loadMockup <$> paths)

getImages :: Text -> IO [Image]
getImages  path = textToString >>> (getFilesByExtensions [".jpeg", ".jpg"] >=> go) $ path
  where
    go paths = catMaybes <$> (sequence $ loadImage <$> paths)
    --
--------------------------------------------------------------------------------

data LoggiaOptions = LoggiaOptions
  { _imagesPath  :: FilePath
  , _mockupsPath :: FilePath
  , _outputPath  :: FilePath
  , _staticPath  :: FilePath
  } deriving (Show, Eq)

loggiaOptionsParser :: Parser LoggiaOptions
loggiaOptionsParser = LoggiaOptions
  <$> ( strOption $  long    "images"
                  <> short   'i'
                  <> metavar "DIRECTORY"
                  <> help    "Images path" )
  <*> ( strOption $  long "mockups"
                  <> short 'm'
                  <> metavar "DIRECTORY"
                  <> help "Mockups path" )
  <*> ( strOption $  long "output"
                  <> short 'o'
                  <> metavar "DIRECTORY"
                  <> help "Output path" )
  <*> ( strOption $  long "static"
                  <> short 's'
                  <> metavar "DIRECTORY"
                  <> help "Directory to serve" )

--------------------------------------------------------------------------------

names :: [String]
names = go <$> [1..]
  where go x =  textToString ("test" <> show x <> ".html")

mockups :: [Mockup1]
mockups = [testMockup0, testMockup1]

--------------------------------------------------------------------------------

randomSplits :: Int -> Int -> [a] -> IO [[a]]
randomSplits a b l
  | length l >= b = do
    i <- randomRIO (a,b)
    let (s1, ls) = splitAt i l
    ss <- randomSplits a b ls
    return (s1:ss)
  | otherwise = return []

runOptions :: LoggiaOptions -> IO ()
runOptions (LoggiaOptions imgsP mockupsP outputP dirP) = do
  imgs    <- getImages ( pack imgsP ) >>= (randomSplits 1 4)
  mockups <- getMockups $ pack mockupsP
  -- let layouts = catMaybes  $ merge <$> mockups
  -- add image ratio meta data !!!!!
  result <-  runMaybeT $  runLoggerT print $ arrange 1 mockups imgs

  case result of
    Nothing -> putStrLn "Fuck!"
    Just a  -> do
      writeAlbum ( outputP <> "index.html" ) a
      BS.writeFile "data.js" (encode $ go <$> a)
      run 8080 $ gzip def . logStdoutDev $ app  (go <$> a) dirP imgsP
  where
    go = fmap (over path $ pack . mappend "/app/img/" . takeFileName . textToString) . unMuTree

--------------------------------------------------------------------------------

-- loggiaServer::  [SPage] -> Server LoggiaAPI
loggiaServer album dirIndex dirImg  = (const $ return album)
                                 :<|> serveDirectory dirImg
                                 :<|> serveDirectory dirIndex

-- app :: [SPage] -> Application
app album dirIndex dirImg = serve loggiaAPI $ loggiaServer album dirIndex dirImg

main :: IO ()
main = execParser opts >>= runOptions where
  opts = OptA.info ( helper <*> loggiaOptionsParser )
              ( header "From SVG mockups to HTML5 albums" <> fullDesc )
