{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module FirstApp.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, Query (Query), NamedParam((:=)))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           FirstApp.Types                     (Comment, CommentText,
                                                     Error, Topic, getTopic, mkTopic, fromDbComment, getCommentText)

-- ------------------------------------------------------------------------|
-- You'll need the documentation for sqlite-simple ready for this section! |
-- ------------------------------------------------------------------------|

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
--
-- To help with that, we create a new data type that can hold our `Connection`
-- for us, and allows it to be expanded later if we need to
data FirstAppDB = FirstAppDB
  { dbConn :: Connection
  }

-- Quick helper to pull the connection and close it down.
closeDB :: FirstAppDB -> IO ()
closeDB FirstAppDB{..} = Sql.close dbConn

-- Given a `FilePath` to our SQLite DB file, initialise the database and ensure
-- our Table is there by running a query to create it, if it doesn't exist
-- already.
initDB :: FilePath -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = do
  dbConn <- Sql.open fp
  Sql.execute_ dbConn createTableQ
  pure $ Right FirstAppDB{..}
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ = "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time TEXT)"

-- Note that we don't store the `Comment` in the DB, it is the type we build
-- to send to the outside world. We will be loading our `DbComment` type from
-- the FirstApp.DB.Types module before converting trying to convert it to a
-- `Comment`.
--
-- To go from a DbComment to a Comment, we need to use ``fromDbComment`` that is
-- defined in FirstApp.Types.
--
-- HINT: You can use '?' or named place-holders as query parameters. Have a look
-- at the section on parameter substitution in sqlite-simple's documentation.
getComments :: FirstAppDB -> Topic -> IO (Either Error [Comment])
getComments FirstAppDB{..} topic =
  let
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
  -- There are several possible implementations of this function. Particularly
  -- there may be a trade-off between deciding to throw an Error if a DbComment
  -- cannot be converted to a Comment, or simply ignoring any DbComment that is
  -- not valid.
  in
    Sql.query dbConn sql (Sql.Only (getTopic topic)) >>= (pure . traverse fromDbComment)

addCommentToTopic :: FirstAppDB -> Topic -> CommentText -> IO (Either Error ())
addCommentToTopic FirstAppDB{..} topic comment = do
  let sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  time <- getCurrentTime
  xs <- Sql.execute dbConn sql (getTopic topic, getCommentText comment, time)
  pure $ Right xs

getTopics :: FirstAppDB -> IO (Either Error [Topic])
getTopics FirstAppDB{..} =
  let
    sql = "SELECT DISTINCT topic FROM comments"
  in
    Sql.query_ dbConn sql >>= (pure . traverse (mkTopic . Sql.fromOnly))

deleteTopic :: FirstAppDB -> Topic -> IO (Either Error ())
deleteTopic FirstAppDB{..} topic =
  let
    sql = "DELETE FROM comments WHERE topic = ?"
  in
    Sql.execute dbConn sql (Sql.Only (getTopic topic)) >>= (pure . Right)
