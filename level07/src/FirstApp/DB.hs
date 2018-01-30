{-# LANGUAGE OverloadedStrings #-}
module FirstApp.DB
  ( Table (..)
  , FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import           Control.Monad.IO.Class             (liftIO)
import           Control.Monad.Reader               (asks)

import           Data.Bifunctor                     (first)

import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow,
                                                     Query (fromQuery), ToRow)
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import           FirstApp.DB.Types                  (FirstAppDB (FirstAppDB, dbConn),
                                                     Table (Table, getTableName))
import           FirstApp.Error                     (Error (DBError))
import           FirstApp.Types                     (Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Topic, fromDbComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)
-- import           Data.Either                        (fromRight)
import           FirstApp.AppM                      (AppM, envDB, throwL)

-- Quick helper to pull the connection and close it down.
closeDB
  :: FirstAppDB
  -> IO ()
closeDB =
  Sql.close . dbConn

initDB
  :: DBFilePath
  -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we be told in the types?
  con <- Sql.open ( getDBFilePath fp )
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ = "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

getDBConn
  :: AppM Connection
getDBConn =
  asks (dbConn . envDB)

runDB :: (a -> Either Error b) -> (Connection -> IO a) -> AppM b
runDB f dbAct = do
  c <- getDBConn
  r <- liftIO $ Sql.runDBAction $ dbAct c
  -- r = Either SQLiteResponse a
  either (throwL . Left . DBError) (throwL . f) r


query :: (ToRow s, FromRow r) => Query -> s -> AppM (Either Error [r])
query q args = runDB try (\c -> Sql.query c q args)

query_ :: FromRow r => Query -> AppM (Either Error [r])
query_ q = runDB (\c -> Sql.query_ c q)

execute :: ToRow s => Query -> s -> AppM (Either Error ())
execute q args = runDB (\c -> Sql.execute c q args)

execute_ :: Query -> AppM (Either Error ())
execute_ q = runDB (\c -> Sql.execute_ c q)

getComments :: Topic -> AppM [Comment]
getComments t = do
  -- m a -> (a -> m b) -> mb
  -- Write the query with an icky string and remember your placeholders!
  let q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
  results <- query q (Sql.Only $ getTopic t)
  pure $ results >>= traverse fromDbComment

-- getComments :: Topic
--             -> AppM (Either Error [Comment])
-- getComments t = do


addCommentToTopic
  :: Topic
  -> CommentText
  -> AppM ()
addCommentToTopic =
  error "Copy your completed 'appCommentToTopic' and refactor to match the new type signature"

getTopics
  :: AppM [Topic]
getTopics =
  error "Copy your completed 'getTopics' and refactor to match the new type signature"

deleteTopic
  :: Topic
  -> AppM ()
deleteTopic =
  error "Copy your completed 'deleteTopic' and refactor to match the new type signature"
