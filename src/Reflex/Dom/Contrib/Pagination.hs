{-# LANGUAGE CPP                 #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}

module Reflex.Dom.Contrib.Pagination where

------------------------------------------------------------------------------
import           Control.Lens         hiding ((.=))
import           Data.Aeson
import           Data.ByteString.Lazy (ByteString)
import           Data.Default
import           Data.Function        (on)
import           Data.List
import           Data.Ord
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import           Data.Word
import           Reflex
import           Reflex.Dom.Core
------------------------------------------------------------------------------
import           Reflex.Dom.Contrib.Xhr
------------------------------------------------------------------------------


                                  -----------------
                                  -- Data Types  --
                                  -----------------

------------------------------------------------------------------------------
-- | General data structure needed for running queries with paginated results.
data PaginationQuery = PaginationQuery
  { _pqLimit        :: Word64
  , _pqOffset       :: Word64
  , _pqSearchString :: Text
  } deriving (Eq,Show,Read,Ord)

makeLenses ''PaginationQuery


------------------------------------------------------------------------------
instance Default PaginationQuery where
    def = PaginationQuery 5000 0 ""


------------------------------------------------------------------------------
instance FromJSON PaginationQuery where
    parseJSON (Object o) = PaginationQuery
        <$> o .:? "limit" .!= 5000
        <*> o .:? "offset" .!= 0
        <*> o .: "searchString"
    parseJSON _ = fail "PaginationQuery JSON representation must be an object"


------------------------------------------------------------------------------
instance ToJSON PaginationQuery where
    toJSON (PaginationQuery l o s) =
      object
        [ "limit"        .= l
        , "offset"       .= o
        , "searchString" .= s
        ]


------------------------------------------------------------------------------
-- | Data structure wrapping results.
data PaginationResults a = PaginationResults
  { _prOffset     :: Word64
  , _prTotalCount :: Word64
  , _prTimestamp  :: UTCTime
  , _prResults    :: [a]
  } deriving (Eq,Show,Read,Ord)

makeLenses ''PaginationResults


------------------------------------------------------------------------------
instance FromJSON a => FromJSON (PaginationResults a) where
    parseJSON (Object o) = PaginationResults
        <$> o .: "offset"
        <*> o .: "totalCount"
        <*> o .: "timestamp"
        <*> o .: "results"
    parseJSON _ = fail "PaginationResults JSON representation must be an object"


------------------------------------------------------------------------------
instance ToJSON a => ToJSON (PaginationResults a) where
    toJSON (PaginationResults o c t r) =
      object $
        [ "offset"     .= o
        , "totalCount" .= c
        , "timestamp"  .= t
        , "results"    .= r
        ]

                             -------------------------
                             -- Front-end Functions --
                             -------------------------

------------------------------------------------------------------------------
-- Some convenient type aliases
type PaginationCache k v = Map k [CacheVal v]
type PaginationInput k = (k, PaginationQuery)
type PaginationOutput k v = (k, Maybe (CacheVal v))


------------------------------------------------------------------------------
-- | Along with the query results we also need to store the PaginationQuery
-- structure that generated it as well as a flag indicating whether this data
-- should be stored in the cache.  This prevents results that are sub-searches
-- of a previous search from overwriting the results of a more general query.
data CacheVal a = CacheVal
    { _pvQuery       :: PaginationQuery
    , _pvShouldStore :: Bool
    , _pvValue       :: a
    } deriving (Eq, Show, Ord)

makeLenses ''CacheVal


------------------------------------------------------------------------------
data PQParams = PQParams
    { pqpMaxCacheSize :: Int
    -- ^ The max number of queries to cache
    , pqpPruneAmount  :: Int
    -- ^ The number of queries to discard when we reach the size limit
    } deriving (Eq, Show, Ord)


instance Default PQParams where
    def = PQParams 5 3
    --def = PQParams 20 5


------------------------------------------------------------------------------
-- | Paginated querying with built-in search and results caching.
paginatedQuery
  :: forall t m k a. (MonadWidget t m, Ord k, FromJSON a)
  => PQParams
  -> (Text -> a -> Bool)
  -> Text
  -> Event t (Map Text ByteString, PaginationInput k)
  -- ^ Param map and pagination structure.  k is any additional information
  -- that you need to disambiguate multiple PaginationQuery entries.
  -> m (Event t (Maybe (PaginationResults a)))
paginatedQuery pqp matchSearchString url input = do
    rec pcache <- foldDyn ($) mempty (addToCache pqp <$> r)
        let eme = attachWith (cachedQuery matchSearchString url)
                             (current pcache) input
        de <- widgetHold (return never) eme
        let r = switchPromptlyDyn $ de
    return $ fmap _pvValue . snd <$> r


------------------------------------------------------------------------------
addToCache
    :: Ord k
    => PQParams
    -> PaginationOutput k (PaginationResults v)
    -> PaginationCache k (PaginationResults v)
    -> PaginationCache k (PaginationResults v)
addToCache PQParams{..} (k, v) m =
    if fmap _pvShouldStore v == Just True
      then M.insertWith (++) k [fromJust v] m2
      else m
  where
    m2 = if M.size m > pqpMaxCacheSize then prune pqpPruneAmount m else m


------------------------------------------------------------------------------
-- | Prunes the PaginationCache of the oldest n entries.  This is not the
-- oldest (k,v) pairs.  It is the oldest vs out of all the (k,[v]) pairs.
prune
    :: Ord k
    => Int
    -> PaginationCache k (PaginationResults v)
    -> PaginationCache k (PaginationResults v)
prune n m =
    M.fromList $ map g $ groupBy ((==) `on` fst) $ sortBy (comparing fst) $
    drop n $ sortBy (comparing $ _prTimestamp . _pvValue . snd) $
    concatMap f $ M.toList m
  where
    f (k,vs) = map (k,) vs
    g [] = error "prune impossible error"
    g ps = (fst $ head ps, map snd ps)


------------------------------------------------------------------------------
-- | Checks a cache and makes a request to the supplied URL if the cached data
-- cannot be used to serve the results of the current requested query.
cachedQuery
    :: (MonadWidget t m, Ord k, FromJSON a)
    => (Text -> a -> Bool)
    -> Text
    -> PaginationCache k (PaginationResults a)
    -> (Map Text ByteString, PaginationInput k)
    -> m (Event t (PaginationOutput k (PaginationResults a)))
cachedQuery matchSearchString url cache input = do
    let (k, pq) = snd input
    pb <- getPostBuild
    let getData = do
          res <- getAndDecode (mkFullPath input <$ pb)
          return $ (\v -> (k, CacheVal pq True <$> v)) <$> res
    case M.lookup k cache of
      Nothing -> getData
      Just pvs ->
          case filter (isSubSearch pq) pvs of
            [] -> getData
            pv:_ -> do
              let pv2 = pv & (pvValue . prResults) %~
                             filter (matchSearchString $ _pqSearchString pq)
                           & pvShouldStore .~ False
              return $ (k,Just pv2) <$ pb
  where
    mkFullPath p = url <> "?" <> queryString p
    queryString (ps, (_,pq)) = formEncode $
      M.insert "pagination" (encode pq) ps


------------------------------------------------------------------------------
-- | Checks whether a previous cached search string is a substring of the
-- current search string.  In this case we don't need to requery the server.
isSubSearch
    :: PaginationQuery
    -> CacheVal (PaginationResults a)
    -- ^ Cached query results
    -> Bool
isSubSearch pq pv =
    (_pqSearchString (_pvQuery pv) `T.isInfixOf` _pqSearchString pq) &&
    (_prTotalCount pr == fromIntegral (length (_prResults pr))) &&
    (_prOffset pr == 0)
  where
    pr = _pvValue pv
