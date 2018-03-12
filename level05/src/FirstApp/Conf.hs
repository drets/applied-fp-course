{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
module FirstApp.Conf
    ( parseOptions
    ) where

import           GHC.Word                  (Word16)

import           Data.Bifunctor            (first)
import           Data.Monoid               ((<>), Last(..))

import           FirstApp.Types            (Conf(..), ConfigError(..),
                                            DBFilePath (DBFilePath),
                                            PartialConf(..), Port (Port), getDBFilePath)

import           FirstApp.Conf.CommandLine (commandLineParser)
import           FirstApp.Conf.File        (parseJSONConfigFile)

-- For the purposes of this application we will encode some default values to
-- ensure that our application continues to function in the event of missing
-- configuration values from either the file or command line inputs.
defaultConf :: PartialConf
defaultConf = PartialConf
  { pcPort       = Last (pure $ Port 3000)
  , pcDBFilePath = Last (pure $ DBFilePath "app_db.db")
  }

-- We need something that will take our PartialConf and see if can finally build
-- a complete ``Conf`` record. Also we need to highlight any missing values by
-- providing the relevant error.
makeConfig :: PartialConf -> Either ConfigError Conf
makeConfig PartialConf{..} = case pcPort of
   (Last Nothing) -> Left NoPort
   (Last (Just portNumber)) -> do
       case pcDBFilePath of
          (Last Nothing) -> Left NoDbFilePath
          (Last (Just filePath)) -> Right $ do
            Conf { portNumber = portNumber
                 , filePath   = getDBFilePath filePath
                 }


-- This is the function we'll actually export for building our configuration.
-- Since it wraps all our efforts to read information from the command line, and
-- the file, before combining it all and returning the required information.

-- Remember that we want the command line configuration to take precedence over
-- the File configuration, so if we think about combining each of our ``Conf``
-- records. By now we should be able to write something like this:
--
-- ``defaults <> file <> commandLine``
--
parseOptions :: FilePath -> IO (Either ConfigError Conf)
parseOptions filePath = do
  cmd <- commandLineParser
  cf <- parseJSONConfigFile filePath
  case cf of
    (Left _)   -> pure $ makeConfig (defaultConf <> cmd)
    (Right fc) -> pure $ makeConfig (defaultConf <> cmd <> fc)
