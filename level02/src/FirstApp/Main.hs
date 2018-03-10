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

import           FirstApp.Types           (ContentType(PlainText, Json), Error(EmptyTopic, EmptyComment, NoHandler), RqType(AddRg, ViewRq, ListRq),
                                           mkCommentText, mkTopic,
                                           renderContentType)

-- --------------------------------------------
-- - Don't start here, go to FirstApp.Types!  -
-- --------------------------------------------

-- | Some helper functions to make our lives a little more DRY.
mkResponse :: Status -> ContentType -> LBS.ByteString -> Response
mkResponse status ct = responseLBS status [("Content-Type", (renderContentType ct))]

resp200 :: ContentType -> LBS.ByteString -> Response
resp200 = mkResponse status200

resp404 :: ContentType -> LBS.ByteString -> Response
resp404 = mkResponse status404

resp400 :: ContentType -> LBS.ByteString -> Response
resp400 = mkResponse status400

-- These next few functions will take raw request information and construct one
-- of our types.
mkAddRequest :: Text -> LBS.ByteString -> Either Error RqType
mkAddRequest topic comment =
  AddRg <$> (mkTopic topic) <*> (mkCommentText (lazyByteStringToStrictText comment))
  where
    -- This is a helper function to assist us in going from a Lazy ByteString, to a Strict Text
    lazyByteStringToStrictText = decodeUtf8 . LBS.toStrict

-- This has a number of benefits, we're able to isolate our validation
-- requirements into smaller components that are simpler to maintain and verify.
-- It also allows for greater reuse and it also means that validation is not
-- duplicated across the application, maybe incorrectly.
mkViewRequest :: Text -> Either Error RqType
mkViewRequest t = ViewRq <$> (mkTopic t)

mkListRequest :: Either Error RqType
mkListRequest = Right ListRq

mkErrorResponse :: Error -> Response
mkErrorResponse EmptyTopic   = resp400 PlainText "empty topic"
mkErrorResponse EmptyComment = resp400 PlainText "empty comment"
mkErrorResponse NoHandler    = resp400 PlainText "no handler"

-- Use our ``RqType`` helpers to write a function that will take the input
-- ``Request`` from the Wai library and turn it into something our application
-- cares about.
mkRequest :: Request -> IO ( Either Error RqType )
mkRequest request = do
  -- Remembering your pattern-matching skills will let you implement the entire
  -- specification in this function.
  case (requestMethod request, pathInfo request) of
    ("GET", ["list"])        -> pure mkListRequest
    ("GET", [topic, "view"]) -> pure $ mkViewRequest topic
    ("POST", [topic, "add"]) -> do
      body <- strictRequestBody request
      pure $ mkAddRequest topic body
    _                        -> pure $ Left NoHandler

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
handleRequest :: RqType -> Either Error Response
handleRequest (AddRg topic comment) = pure $ do
  resp200 PlainText "“Comment on a given topic” not implemented yet"
handleRequest (ViewRq topic)        = pure $ do
  resp200 PlainText "“View a topic and its comments” not implemented yet"
handleRequest ListRq                = pure $ do
  resp200 PlainText "“List the current topics” not implemented yet"

-- Reimplement this function using the new functions and ``RqType`` constructors
-- as a guide.
app :: Application
app request cb = do
  request' <- mkRequest request
  cb $ either
    mkErrorResponse
    (either mkErrorResponse id . handleRequest)
    request'

runApp :: IO ()
runApp = run 3000 app
