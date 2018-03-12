{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Conf.File where

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS

import           Data.Text                  (Text)

import           Data.Bifunctor             (first)
import           Data.Monoid                (Last (Last))

import           Control.Exception          (try)

import           Data.Aeson                 (FromJSON, Object)

import qualified Data.Aeson                 as Aeson

import           FirstApp.Types             (ConfigError(NoSuchFile, InvalidPartialConfig),
                                             PartialConf (PartialConf))
-- Doctest setup section
-- $setup
-- >>> :set -XOverloadedStrings

-- | File Parsing

-- We're trying to avoid complications when selecting a configuration file
-- package from Hackage. We'll use an encoding that you are probably familiar
-- with, for better or worse, and write a small parser to pull out the bits we
-- need. The package we're using is the ``aeson`` package to parse some JSON and
-- we'll pick the bits off the Object.

-- | Update these tests when you've completed this function.
--
-- | readConfFile
-- >>> readConfFile "badFileName.no"
-- Left (NoSuchFile badFileName.no: openBinaryFile: does not exist (No such file or directory))
-- >>> readConfFile "test.json"
-- Right "{\n  \"foo\": 33\n}\n"
--
readConfFile :: FilePath -> IO ( Either ConfigError ByteString )
readConfFile filePath = do
  r <- try (LBS.readFile filePath)
  case (r :: Either IOError ByteString) of
    Left e -> pure $ Left (NoSuchFile e)
    Right bytestring -> pure $ Right bytestring

-- Construct the function that will take a ``FilePath``, read it in, decode it,
-- and construct our ``PartialConf``.
parseJSONConfigFile :: FilePath -> IO ( Either ConfigError PartialConf )
parseJSONConfigFile filePath = do
  b <- readConfFile filePath
  case b of
    Left e -> pure $ Left e
    Right r -> do
      case Aeson.decode r of
        Nothing -> pure $ Left InvalidPartialConfig
        Just p  -> pure $ Right p
