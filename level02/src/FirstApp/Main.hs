{-# LANGUAGE OverloadedStrings #-}
module FirstApp.Main (runApp) where

import           Network.Wai              (Application, Request, Response,
                                           pathInfo, requestMethod, responseLBS,
                                           strictRequestBody)
import           Network.Wai.Handler.Warp (run)

import           Network.HTTP.Types       (Status, hContentType, status200,
                                           status400, status404)

import qualified Data.ByteString.Lazy     as LBS

import           Data.Either              (either)

import           Data.Text                (Text)
import           Data.Text.Encoding       (decodeUtf8)

import           FirstApp.Types           (ContentType(..), Error(..),
                                           RqType(..), mkCommentText, mkTopic,
                                           renderContentType)

-- --------------------------------------------
-- - Don't start here, go to FirstApp.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse :: Status -> ContentType -> LBS.ByteString -> Response
mkResponse status cType =
    responseLBS status
      [(hContentType, renderContentType cType)]

resp200 :: ContentType -> LBS.ByteString -> Response
resp200 = mkResponse status200

resp404 :: ContentType -> LBS.ByteString -> Response
resp404 = mkResponse status404

resp400 :: ContentType -> LBS.ByteString -> Response
resp400 = mkResponse status400

-- These next few functions will take raw request information and construct one
-- of our types.
mkAddRequest :: Text -> LBS.ByteString -> Either Error RqType
mkAddRequest t cmt =
    AddRq <$>
    (mkTopic t) <*>
    (mkCommentText . decodeUtf8 . LBS.toStrict $ cmt)

-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify.
-- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.
mkViewRequest :: Text -> Either Error RqType
mkViewRequest t =
  ViewRq <$> (mkTopic t)

mkListRequest :: Either Error RqType
mkListRequest = Right ListRq

mkErrorResponse :: Error -> Response
mkErrorResponse EmptyTopic   = resp400 PlainText "No empty topics"
mkErrorResponse EmptyComment = resp400 PlainText "No empty comments"
mkErrorResponse UnknownRoute = resp400 PlainText "Could not understand request"

-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest :: Request -> IO ( Either Error RqType )
mkRequest req =
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.
  case (pathInfo req, requestMethod req) of
    ([t, "add"], "POST") -> mkAddRequest t <$> (strictRequestBody req)
    ([t, "view"], "GET") -> return $ mkViewRequest t
    (["list"], "GET")    -> return $ mkListRequest
    _                    -> return $ Left UnknownRoute

-- If we find that we need more information to handle a request, or we have a
-- new type of request that we'd like to handle then we update the ``RqType``
-- structure and the compiler will let us know which parts of our application
-- are affected.
--
-- Reduction of concerns such that each section of the application only deals
-- with a small piece is one of the benefits of developing in this way.
--
-- For now, return a made-up value for each of the responses as we don't have
-- any persistent storage. Plain text responses that contain "X not implemented
-- yet" should be sufficient.
handleRequest :: RqType -> Response
handleRequest (AddRq t cmt) = resp200 PlainText "You made an add req"
handleRequest (ViewRq t) = resp200 PlainText "You made a view req"
handleRequest ListRq = resp200 PlainText "You made a list req"


-- Reimplement this function using the new functions and ``RqType`` constructors
-- as a guide.
app :: Application
app req cb = do
  rqTypeOrError <- mkRequest req
  case rqTypeOrError of
    Left e -> cb $ mkErrorResponse e
    Right rqType -> cb $ handleRequest rqType

runApp :: IO ()
runApp = run 8000 app
