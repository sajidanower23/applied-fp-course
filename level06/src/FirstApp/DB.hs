{-# LANGUAGE OverloadedStrings #-}
module FirstApp.DB
  ( FirstAppDB (FirstAppDB)
  , initDB
  , closeDB
  , addCommentToTopic
  , getComments
  , getTopics
  , deleteTopic
  ) where

import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (asks)

import Data.Bifunctor (first)
import           Data.Text                          (Text)
import qualified Data.Text                          as Text

import           Data.Time                          (getCurrentTime)

import           Database.SQLite.Simple             (Connection, FromRow, ToRow,
                                                     Query (fromQuery))
import qualified Database.SQLite.Simple             as Sql

import qualified Database.SQLite.SimpleErrors       as Sql
import           Database.SQLite.SimpleErrors.Types (SQLiteResponse)

import FirstApp.AppM (AppM(..), Env (..))

import           FirstApp.Types                     (FirstAppDB (FirstAppDB, dbConn), Comment, CommentText,
                                                     DBFilePath (getDBFilePath),
                                                     Error (DBError), Topic,
                                                     fromDbComment,
                                                     getCommentText, getTopic,
                                                     mkTopic)

-- Quick helper to pull the connection and close it down.
closeDB :: FirstAppDB
        -> IO ()
closeDB =
  Sql.close . dbConn

initDB :: DBFilePath
       -> IO ( Either SQLiteResponse FirstAppDB )
initDB fp = Sql.runDBAction $ do
  -- Initialise the connection to the DB...
  -- - What could go wrong here?
  -- - What haven't we been told in the types?
  con <- Sql.open ( getDBFilePath fp )
  -- Initialise our one table, if it's not there already
  _ <- Sql.execute_ con createTableQ
  pure $ FirstAppDB con
  where
  -- Query has an `IsString` instance so string literals like this can be
  -- converted into a `Query` type when the `OverloadedStrings` language
  -- extension is enabled.
    createTableQ =
      "CREATE TABLE IF NOT EXISTS comments (id INTEGER PRIMARY KEY, topic TEXT, comment TEXT, time INTEGER)"

getDBConn :: AppM Connection
getDBConn = asks (dbConn . envDB)

runDB :: (Connection -> IO a)
      -> AppM (Either Error a)
runDB dbAct = do
  c <- getDBConn
  r <- liftIO $ Sql.runDBAction $ dbAct c
  pure $ first DBError r

query :: (ToRow s, FromRow r) => Query -> s -> AppM (Either Error [r])
query q args = runDB (\c -> Sql.query c q args)

query_ :: FromRow r => Query -> AppM (Either Error [r])
query_ q = runDB (\c -> Sql.query_ c q)

execute :: ToRow s => Query -> s -> AppM (Either Error ())
execute q args = runDB (\c -> Sql.execute c q args)

execute_ :: Query -> AppM (Either Error ())
execute_ q = runDB (\c -> Sql.execute_ c q)

getComments :: Topic
            -> AppM (Either Error [Comment])
getComments t = do
  -- m a -> (a -> m b) -> mb
  -- Write the query with an icky string and remember your placeholders!
  let q = "SELECT id,topic,comment,time FROM comments WHERE topic = ?"
  results <- query q (Sql.Only $ getTopic t)
  pure $ results >>= traverse fromDbComment

addCommentToTopic :: Topic
                  -> CommentText
                  -> AppM (Either Error ())
addCommentToTopic t c = do
  -- Record the time this comment was created.
  -- nowish <- getCurrentTime
  nowish <- liftIO $ getCurrentTime
  -- Note the triple, matching the number of values we're trying to insert, plus
  -- one for the table name.
  let q =
        -- Remember that the '?' are order dependent so if you get your input
        -- parameters in the wrong order, the types won't save you here. More on that
        -- sort of goodness later.
        "INSERT INTO comments (topic,comment,time) VALUES (?,?,?)"
  -- We use the execute function this time as we don't care about anything
  -- that is returned. The execute function will still return the number of rows
  -- affected by the query, which in our case should always be 1.
  execute q (getTopic t, getCommentText c, nowish)
  -- runDb Right $ Sql.execute (dbConn db) q (getTopic t, getCommentText c, nowish)
  -- An alternative is to write a returning query to get the Id of the DbComment
  -- we've created. We're being lazy (hah!) for now, so assume awesome and move on.

getTopics :: AppM (Either Error [Topic])
getTopics =
  let q = "SELECT DISTINCT topic FROM comments"
  in do
    results <- query_ q
    pure $ results >>= traverse (mkTopic . Sql.fromOnly)

deleteTopic :: Topic
            -> AppM (Either Error ())
deleteTopic t =
  let q = "DELETE FROM comments WHERE topic = ?"
  in
    execute q (Sql.Only $ getTopic t)
