{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
module FirstApp.DB
  ( FirstAppDB (FirstAppDB)
  , initDb
  , closeDb
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Data.Text                          (Text)
import qualified Data.Text                          as Text
import           Data.Traversable
import           Data.Time                          (getCurrentTime)
import           Data.Either
import           Database.SQLite.Simple             (Connection, Query (Query))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)
import           Data.Bifunctor
import           FirstApp.Types                     (Comment, CommentText,
                                                     Error(..), Topic, getTopic,
                                                     getCommentText, mkTopic,
                                                     mkCommentText,
                                                     fromDbComment)

-- ------------------------------------------------------------------------|
-- You'll need the documentation for sqlite-simple ready for this section! |
-- ------------------------------------------------------------------------|

-- We have a data type to simplify passing around the information we need to run
-- our database queries. This also allows things to change over time without
-- having to rewrite all of the functions that need to interact with DB related
-- things in different ways.
data FirstAppDB = FirstAppDB {
                    conn :: Connection
                  } 

-- Quick helper to pull the connection and close it down.
closeDb :: FirstAppDB -> IO ()
closeDb (FirstAppDB c) = Sql.close c

-- Given a `FilePath` to our SQLite DB file, initialise the database and ensure
-- our Table is there by running a query to create it, if it doesn't exist
-- already.
initDb :: FilePath -> IO ( Either SQLiteResponse FirstAppDB )
initDb fp = do
  c <- Sql.open fp
  Sql.execute_ c createTableQ
  pure $ Right $ FirstAppDB c
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
getComments (FirstAppDB c) t =
  let
    sql = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
  -- There are several possible implementations of this function. Paritcularly
  -- there may be a trade-off between deciding to throw an Error if a DbComment
  -- cannot be converted to a Comment, or simply ignoring any DbComment that is
  -- not valid.
  in do
    cmtList <- Sql.query c sql (Sql.Only $ getTopic t)
    case partitionEithers (fromDbComment <$> cmtList) of
      ([], rCmts) -> pure $ Right rCmts
      _           -> pure $ Left DBError
    -- pure $ traverse fromDbComment =<< cmtList

addCommentToTopic :: FirstAppDB -> Topic -> CommentText -> IO (Either Error ())
addCommentToTopic (FirstAppDB c) t cmt =
  let
    sql = "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  in do
    time <- getCurrentTime
    first (const DBError)
      <$>
        Sql.runDBAction (Sql.execute c sql (getTopic t, getCommentText cmt, time))

getTopics :: FirstAppDB -> IO (Either Error [Topic])
getTopics (FirstAppDB c) =
  let
    sql = "SELECT DISTINCT topic FROM comments"
  in  do
    topicEithers <- Sql.query_ c sql
    case partitionEithers (mkTopic . Sql.fromOnly <$> topicEithers) of
      ([], rTopics) -> pure $ Right rTopics
      _             -> pure $ Left DBError

deleteTopic :: FirstAppDB -> Topic -> IO (Either Error ())
deleteTopic (FirstAppDB c) t =
  let
    sql = "DELETE FROM comments WHERE topic = ?"
  in do
    first (const DBError)
      <$>
        Sql.runDBAction (Sql.execute c sql (Sql.Only (getTopic t)))
