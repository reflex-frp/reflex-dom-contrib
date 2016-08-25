{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
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
import           Data.List
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.String.Conv
import qualified Data.Text as T
import           Network.HTTP.Types.URI
------------------------------------------------------------------------------
import           Reflex
import           Reflex.Dom
------------------------------------------------------------------------------


------------------------------------------------------------------------------
-- | URL encodes a map of key-value pairs.
formEncode :: Map T.Text ByteString -> T.Text
formEncode m =
    T.intercalate "&" $
      map (\(k,v) -> T.concat [k,"=",encodeToText v]) $ M.toList m
  where
    encodeToText :: ByteString -> T.Text
    encodeToText = toS . urlEncode True . toS


------------------------------------------------------------------------------
-- | Form encodes a JSON object.
formEncodeJSON :: ToJSON a => a -> T.Text
formEncodeJSON a = case toJSON a of
    Object m ->
      formEncode $ M.fromList $ map (bimap id encode) $ itoList m
    _ -> error "formEncodeJSON requires an Object"


------------------------------------------------------------------------------
-- | Convenience function for constructing a POST request.
toPost
    :: T.Text
    -- ^ URL
    -> T.Text
    -- ^ The post data
    -> XhrRequest (Maybe T.Text)
toPost url d =
    XhrRequest "POST" url $ def { _xhrRequestConfig_headers = headerUrlEnc
                                , _xhrRequestConfig_sendData = Just d
                                }
  where
    headerUrlEnc :: Map T.Text T.Text
    headerUrlEnc = "Content-type" =: "application/x-www-form-urlencoded"


------------------------------------------------------------------------------
-- | This is the foundational primitive for the XHR API because it gives you
-- full control over request generation and response parsing and also allows
-- you to match things that generated the request with their corresponding
-- responses.
performAJAX
    :: (MonadWidget t m)
    => (a -> XhrRequest T.Text)
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
    => Event t (T.Text, a)
    -- ^ Event with a URL and a JSON object to be sent
    -> m (Event t (a, Maybe b))
performJsonAjax req =
    performEventAsync $ ffor req $ \(url,a) cb -> do
      _ <- newXMLHttpRequest (mkRequest url a) $ \response ->
             liftIO $ cb (a, decodeXhrResponse response)
      return ()
  where
    mkRequest url a = toPost url (formEncodeJSON a)


