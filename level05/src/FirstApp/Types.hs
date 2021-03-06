{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-missing-methods #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
module FirstApp.Types
  ( Error (..)
  , ConfigError (..)
  , PartialConf (..)
  , Port (..)
  , DBFilePath (..)
  , Conf (..)
  , RqType (..)
  , ContentType (..)
  , Comment (..)
  , Topic
  , CommentText
  , mkTopic
  , getTopic
  , mkCommentText
  , getCommentText
  , renderContentType
  , confPortToWai
  , fromDbComment
  ) where

import           GHC.Generics                       (Generic)
import           GHC.Word                           (Word16)

import           Data.ByteString                    (ByteString)
import           Data.Text                          (Text)

import           System.IO.Error                    (IOError)

import           Data.Monoid                        (Last,
                                                     Monoid (mappend, mempty))

import           Data.List                          (stripPrefix)
import           Data.Maybe                         (fromMaybe)
import           Data.Time                          (UTCTime)
import           Control.Exception

import           Data.Aeson                         (FromJSON (..), ToJSON, (.:))
import qualified Data.Aeson                         as A
import qualified Data.Aeson.Types                   as A

import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)
import           FirstApp.DB.Types                  (DbComment (dbCommentComment, dbCommentId, dbCommentTime, dbCommentTopic))
import           FirstApp.Types.Error               (Error ( UnknownRoute
                                                           , EmptyCommentText
                                                           , EmptyTopic
                                                           , DBError
                                                           ))
import           FirstApp.Types.CommentText        ( CommentText
                                                   , mkCommentText
                                                   , getCommentText
                                                   )
import           FirstApp.Types.Topic              (Topic, mkTopic, getTopic)

newtype CommentId = CommentId Int
  deriving (Show, ToJSON)

data Comment = Comment
  { commentId    :: CommentId
  , commentTopic :: Topic
  , commentText  :: CommentText
  , commentTime  :: UTCTime
  }
  -- Generic has been added to our deriving list.
  deriving ( Show, Generic )

-- Strip the prefix (which may fail if the prefix isn't present), fall
-- back to the original label if need be, then camel-case the name.

-- | modFieldLabel
-- >>> modFieldLabel "commentId"
-- "id"
-- >>> modFieldLabel "topic"
-- "topic"
-- >>> modFieldLabel ""
-- ""
modFieldLabel
  :: String
  -> String
modFieldLabel l =
  A.camelTo2 '_'
  . fromMaybe l
  $ stripPrefix "comment" l

instance ToJSON Comment where
  -- This is one place where we can take advantage of our Generic instance. Aeson
  -- already has the encoding functions written for anything that implements the
  -- Generic typeclass. So we don't have to write our encoding, we tell Aeson to
  -- build it.
  toEncoding = A.genericToEncoding opts
    where
      -- These options let us make some minor adjustments to how Aeson treats
      -- our type. Our only adjustment is to alter the field names a little, to
      -- remove the 'comment' prefix and use an Aeson function to handle the
      -- rest of the name. This accepts any 'String -> String' function but it's
      -- wise to keep the modifications simple.
      opts = A.defaultOptions
             { A.fieldLabelModifier = modFieldLabel
             }

-- For safety we take our stored DbComment and try to construct a Comment that
-- we would be okay with showing someone. However unlikely it may be, this is a
-- nice method for separating out the back and front end of a web app and
-- providing greater guarantees about data cleanliness.

fromDbComment
  :: DbComment
  -> Either Error Comment
fromDbComment dbc =
  Comment (CommentId     $ dbCommentId dbc)
      <$> (mkTopic       $ dbCommentTopic dbc)
      <*> (mkCommentText $ dbCommentComment dbc)
      <*> (pure          $ dbCommentTime dbc)

-- We have to be able to:
-- - Comment on a given topic
-- - View a topic and its comments
-- - List the current topics
--
-- To that end, we have the following types:
--
-- AddRq : Which needs to the target topic, and the body of the comment.
-- ViewRq : Which needs the topic being requested.
-- ListRq : Which lists all of the current topics.
data RqType
  = AddRq Topic CommentText
  | ViewRq Topic
  | ListRq

data ContentType
  = PlainText
  | JSON

renderContentType
  :: ContentType
  -> ByteString
renderContentType PlainText = "text/plain"
renderContentType JSON      = "application/json"

-----------------
-- Config Types
-----------------

-- This is an alternative way of defining a `newtype`. You define it as a record
-- with a single field, this provides the unwrapping function for free. When
-- defined using the other method, you must use pattern-matching or write a dedicated
-- function in order to get the value out.
--
newtype Port = Port
  -- You will notice we're using ``Word16`` as our type for the ``Port`` value.
  -- This is because a valid port number can only be a 16bit unsigned integer.
  { getPort :: Word16 }
  deriving (Eq, Show)

newtype DBFilePath = DBFilePath
  { getDBFilePath :: FilePath }
  deriving (Eq, Show)

-- Add some fields to the ``Conf`` type:
-- - A customisable port number: ``Port``
-- - A filepath for our SQLite database: ``DBFilePath``
data Conf = Conf
   { portNumber :: Port
   , filePath   :: FilePath
   }

-- We're storing our Port as a Word16 to be more precise agetPortnd prevent invalid
-- values from being used in our application. However Wai is not so stringent.
-- To accommodate this and make our lives a bit easier, we will write this
-- helper function to take ``Conf`` value and convert it to an ``Int``.
--
-- We'll need to use a function called; ``fromIntegral``, to convert our
-- ``Word16`` to an ``Int``. The type of this function is:
--
-- fromIntegral :: (Num b, Integral a) => a -> b
--
confPortToWai :: Conf -> Int
confPortToWai Conf{..} = fromIntegral . getPort $ portNumber

-- Similar to when we were considering our application types, leave this empty
-- for now and add to it as you go.
data ConfigError = NoSuchFile IOError
                 | InvalidPartialConfig
                 | NoPort
                 | NoDbFilePath
                 deriving Show

-- Our application will be able to load configuration from both a file and
-- command line input. We want to be able to use the command line to temporarily
-- override the configuration from our file. How do we combine the different
-- inputs to enable this property?

-- We want the command line configuration to take precedence over the File
-- configuration, so if we think about combining each of our ``Conf`` records,
-- we want to be able to write something like this:

-- ``defaults <> file <> commandLine``

-- We can use the ``Monoid`` typeclass to handle combining the ``Conf`` records
-- together, and the ``Last`` type to wrap up our values to handle the desired
-- precedence. The ``Last`` type is a wrapper for Maybe that when used with its
-- ``Monoid`` instance will always preference the last ``Just`` value that it
-- has:

-- Last (Just 3) <> Last (Just 1) = Last (Just 1)
-- Last Nothing  <> Last (Just 1) = Last (Just 1)
-- Last (Just 1) <> Last Nothing  = Last (Just 1)

-- To make this easier, we'll make a new type ``PartialConf`` that will have our
-- ``Last`` wrapped values. We can then define a ``Monoid`` instance for it and
-- have our ``Conf`` be a known good configuration.
data PartialConf = PartialConf
  { pcPort       :: Last Port
  , pcDBFilePath :: Last DBFilePath
  }

-- We now define our ``Monoid`` instance for ``PartialConf``. Allowing us to
-- define our always empty configuration, which would always fail our
-- requirements. More interestingly, we define our ``mappend`` function to lean
-- on the ``Monoid`` instance for Last to always get the last value.
instance Monoid PartialConf where
  mempty = PartialConf mempty mempty

  mappend _a _b = PartialConf
    { pcPort       = mappend (pcPort _a) (pcPort _b)
    , pcDBFilePath = mappend (pcDBFilePath _a) (pcDBFilePath _b)
    }

-- When it comes to reading the configuration options from the command-line, we
-- use the 'optparse-applicative' package. This part of the exercise has already
-- been completed for you, feel free to have a look through the 'CommandLine'
-- module and see how it works.
--
-- For reading the configuration from the file, we're going to use the aeson
-- library to handle the parsing and decoding for us. In order to do this, we
-- have to tell aeson how to go about converting the JSON into our PartialConf
-- data structure.
instance FromJSON Port where
  parseJSON = A.withObject "port" $ \o -> do
    Port <$> o .: "port"

instance FromJSON DBFilePath where
  parseJSON = A.withObject "dbFilePath" $ \o -> do
    DBFilePath <$> o .: "dbFileName"

instance FromJSON PartialConf where
  parseJSON = A.withObject "partialConfig" $ \o -> do
    pcPort       <- parseJSON (A.Object o)
    pcDBFilePath <- parseJSON (A.Object o)
    return PartialConf{..}
