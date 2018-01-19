{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Conf.File where

import           Data.ByteString.Lazy       (ByteString)
import qualified Data.ByteString.Lazy.Char8 as LBS

import           Data.Text                  (Text)

import           Data.Bifunctor             (first)
import           Data.Monoid                (Last (Last))

import           Control.Exception          (try, IOException)

import           Data.Aeson                 (FromJSON, Object)
import           Data.Aeson.Types
import qualified Data.Aeson                 as Aeson

import           FirstApp.Types             (ConfigError(..),
                                             PartialConf (..),
                                             Port(..), DBFilePath(..))
-- Doctest setup section
-- $setup
-- >>> :set -XOverloadedStrings

-- | File Parsing

-- We're trying to avoid complications when selecting a configuration file
-- package from Hackage. We'll use an encoding that you are probably familiar
-- with, for better or worse, and write a small parser to pull out the bits we
-- need. The package we're using is the ``aeson`` package to parse some JSON and
-- we'll pick the bits off the Object.

-- Complete the helper function that will be used to retrieve values off the
-- JSON object.

-- | fromJsonObjWithKey
-- >>> let (Just obj) = ( Aeson.decode "{\"foo\":\"Susan\"}" ) :: Maybe Aeson.Object
--
-- >>> fromJsonObjWithKey "foo" (id :: Text -> Text) obj
-- Last {getLast = Just "Susan"}
--
-- >>> fromJsonObjWithKey "foo" id obj
-- Last {getLast = Nothing}
--
fromJsonObjWithKey :: FromJSON a => Text -> (a -> b) -> Object -> Last b
fromJsonObjWithKey k f obj = Last $ f <$> parseMaybe (parseField obj) k

-- |----
-- | You will need to update these tests when you've completed the following functions!
-- | The 'undefined' in these tests needs to be replaced with their respective Error constructors.
-- |----

-- | decodeObj
-- >>> decodeObj ""
-- Left (DecodeError "Error in $: not enough input")
--
-- >>> decodeObj "{\"bar\":33}"
-- Right (fromList [("bar",Number 33.0)])
--
decodeObj :: ByteString -> Either ConfigError Object
decodeObj b = first (const DecodeError) (Aeson.eitherDecode b)

-- | Update these tests when you've completed this function.
--
-- | readObject
-- >>> readObject "badFileName.no"
-- Left (NoFileError badFileName.no: openBinaryFile: does not exist (No such file or directory))
--
-- >>> readObject "test.json"
-- Right "{\"foo\":33}\n"
--
readObject :: FilePath -> IO ( Either ConfigError ByteString )
readObject fp = do
  first (const MissingDBFilePath) <$> (try (LBS.readFile fp) :: IO (Either IOException ByteString))
  -- (a -> c) -> Either a b -> 
                        -- Exception e => IO a -> IO (Either e a)
  -- try (LBS.readFile fp)
  -- res <- try (LBS.readFile fp)
  -- case res of
  --   Left e -> pure $ Left NoFileError
  --   Right b -> pure $ Right b

-- Construct the function that will take a ``FilePath``, read it in and attempt
-- to decode it as a valid JSON object, using the ``aeson`` package. Then pull
-- specific keys off this object and construct our ``PartialConf``. Using the
-- function we wrote above to assist in pulling items off the object.
parseJSONConfigFile :: FilePath -> IO ( Either ConfigError PartialConf )
parseJSONConfigFile fp = do
  objStrE <- readObject fp
  case objStrE of
    Left e1 -> pure $ Left e1
    Right objStr -> case decodeObj objStr of
      Left e2 -> pure $ Left e2
      Right obj ->
        let p = fromJsonObjWithKey "port" Port obj
            f = fromJsonObjWithKey "dbFileName" DBFilePath obj
        in
          return $ Right $ PartialConf p f
