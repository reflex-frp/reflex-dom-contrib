{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE FlexibleContexts    #-}
{-|

Convenience functions for dealing with XMLHttpRequest.

-}

module Reflex.Dom.Contrib.Xhr where

------------------------------------------------------------------------------
import           Control.Lens
import           Control.Monad.Reader
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Default
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.String.Conv
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Types.URI
------------------------------------------------------------------------------
import           Reflex
import           Reflex.Dom.Core
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | URL encodes a map of key-value pairs.

formEncode :: Map Text ByteString -> Text
formEncode m =
    T.intercalate "&" $
      map (\(k,v) -> k <> "=" <> (encodeToText v)) $ M.toList m
  where
    encodeToText :: ByteString -> Text
    encodeToText = toS . urlEncode True . toS


------------------------------------------------------------------------------
-- | Form encodes a JSON object.
formEncodeJSON :: ToJSON a => a -> Text
formEncodeJSON a = case toJSON a of
    Object m ->
      formEncode $ M.fromList $ map (bimap id encode) $ itoList m
    _ -> error "formEncodeJSON requires an Object"


------------------------------------------------------------------------------
-- | Convenience function for constructing a POST request.
toPost
    :: Text
    -- ^ URL
    -> a
    -- ^ The post data
    -> XhrRequest a
toPost url d =
    XhrRequest "POST" url $ def { _xhrRequestConfig_headers = headerUrlEnc
                                , _xhrRequestConfig_sendData = d
                                }
  where
    headerUrlEnc :: Map Text Text
    headerUrlEnc = "Content-type" =: "application/x-www-form-urlencoded"

--toPost
--    :: Text
--    -- ^ URL
--    -> Text
--    -- ^ The post data
--    -> XhrRequest (Maybe Text)
--toPost url d =
--    XhrRequest "POST" url $ def { _xhrRequestConfig_headers = headerUrlEnc
--                                , _xhrRequestConfig_sendData = Just d
--                                }
--  where
--    headerUrlEnc :: Map Text Text
--    headerUrlEnc = "Content-type" =: "application/x-www-form-urlencoded"


------------------------------------------------------------------------------
-- | This is the foundational primitive for the XHR API because it gives you
-- full control over request generation and response parsing and also allows
-- you to match things that generated the request with their corresponding
-- responses.
performAJAX
    :: (MonadWidget t m, IsXhrPayload a)
    => (a -> XhrRequest a)
    -- ^ Function to build the request
    -> (XhrResponse -> b)
    -- ^ Function to parse the response
    -> Event t a
    -> m (Event t (a, b))
performAJAX mkRequest parseResponse req =
    performEventAsync $ ffor req $ \a cb -> do
      _ <- newXMLHttpRequest (mkRequest a) $ \response ->
             liftIO $ cb (a, parseResponse response)
      return ()


------------------------------------------------------------------------------
-- | Performs an async XHR taking a JSON object as input and another JSON
-- object as output.
performJsonAjax
    :: (MonadWidget t m, ToJSON a, FromJSON b)
    => Event t (Text, a)
    -- ^ Event with a URL and a JSON object to be sent
    -> m (Event t (a, Maybe b))
performJsonAjax req =
    performEventAsync $ ffor req $ \(url,a) cb -> do
      _ <- newXMLHttpRequest (mkRequest url a) $ \response ->
             liftIO $ cb (a, decodeXhrResponse response)
      return ()
  where
    mkRequest url a = toPost url (T.unpack $ formEncodeJSON a)
