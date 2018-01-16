{-# LANGUAGE OverloadedStrings          #-}
{-# OPTIONS_GHC -fno-warn-dodgy-exports #-}
module FirstApp.Types
  ( Topic
  , CommentText
  , ContentType (..)
  , RqType (..)
  , Error (..)
  , mkTopic
  , getTopic
  , mkCommentText
  , getCommentText
  , renderContentType
  ) where

import           Data.ByteString (ByteString)
import           Data.Text       (Text)

-- Working through the specification for our application, what are the
-- types of requests we're going to handle?

-- We have to be able to:
-- - Comment on a given topic
-- - View a topic and its comments
-- - List the current topics

-- To that end, create the following constructors for our RqType:

-- AddRq : Which needs the target topic, and the body of the comment.
-- ViewRq : Which needs the topic being requested.
-- ListRq : Which doesn't need anything and lists all of the current topics.
data RqType = AddRq Topic CommentText
            | ViewRq Topic
            | ListRq

-- Not everything goes according to plan, but it's important that our types
-- reflect when errors can be introduced into our program. Additionally it's
-- useful to be able to be descriptive about what went wrong.

-- Fill in the error constructors as you need them.
data Error = EmptyTopic | EmptyComment

-- Provide the constructors for a sum type to specify the `ContentType` Header,
-- to be used when we build our Response type.
data ContentType = PlainText | JSON

-- The ``ContentType`` constructors don't match what is required for the header
-- information, so write a function that will take our ``ContentType`` and
-- produce the correct value for the header.
renderContentType :: ContentType -> ByteString
renderContentType =
  error "renderContentType not implemented"

-- In Haskell the ``newtype`` is a wrapper of sorts that comes with zero runtime
-- cost. It is purely used for type-checking. So when you have a bare primitive
-- value, like an ``Int``, ``String``, or even ``[a]``, you can wrap it up in a
-- ``newtype`` to give it a descriptive name to be more precise in your types.

-- The type system will check it for you, and the compiler will eliminate the
-- cost of the "wrapper". Also, having specialised constructor functions for the
-- newtypes allows you to set extra restrictions, such as minimum values.

-- The ``newtype`` definitions for ``Topic`` and ``CommentText`` are provided
-- for you:

-- Topic
newtype Topic = Topic Text
  deriving Show

-- CommentText
newtype CommentText = CommentText Text
  deriving Show

-- We can choose to *not* export the constructor for a data type and instead
-- provide a function of our own. In our case, we're not interested in empty
-- `Text` values so we will eliminate them with a special constructor and return
-- an error if an empty input is provided.
--
-- The export list at the top of this file demonstrates how to export a type,
-- but not export the constructor.

mkTopic
  :: Text
  -> Either Error Topic
mkTopic "" = Left EmptyTopic
mkTopic t = Right $ Topic t

getTopic
  :: Topic
  -> Text
getTopic (Topic t) = t

mkCommentText
  :: Text
  -> Either Error CommentText
mkCommentText "" = Left EmptyComment
mkCommentText t = Right $ CommentText t

getCommentText
  :: CommentText
  -> Text
getCommentText (CommentText t) = t
