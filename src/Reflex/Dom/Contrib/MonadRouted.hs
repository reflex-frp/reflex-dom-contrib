{-# LANGUAGE ConstraintKinds            #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecursiveDo                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-# LANGUAGE CPP                        #-}

{-|

RouteT monad transformer for frontend routing convenience.

-}

module Reflex.Dom.Contrib.MonadRouted
  ( routeApp
  , MonadRouted(..)
  , askUri
  , askInitialSegments
  , uriPathParts
  , redirectInternal
  , redirectLocal
  , withPathSegment
  , dynPath
  , RouteT(..)
  , PathSegment
  , LocationInfo(..)
  , mkLocationInfo
  ) where

------------------------------------------------------------------------------
import           Control.Monad.Exception
import           Control.Monad.Reader
import           Control.Monad.Ref
import           Control.Monad.State.Strict
import           Data.Coerce
import qualified Data.List                  as L
import           Data.Maybe
import qualified Data.Semigroup             as Sem
import           Data.Text                  (Text)
import qualified Data.Text                  as T
import           Data.Text.Encoding
import           GHCJS.DOM.Types            (MonadJSM)
import           Reflex.Dom.Contrib.Router
import           Reflex.Dom.Core
import           Reflex.Host.Class
import           URI.ByteString
-------------------------------------------------------------------------------
import           Debug.Trace
------------------------------------------------------------------------------

------------------------------------------------------------------------------
-- | Type alias for portions of a path after splitting on '/'
type PathSegment = Text


------------------------------------------------------------------------------
-- | The environment inside the RouteT monad transformer.
data LocationInfo = LocationInfo
    { _locationUri          :: URIRef Absolute
    -- ^ A complete parsed representation of the contents of the location bar
    -- is available at all times.
    , _locationPathSegments :: [PathSegment]
    -- ^ The path split on '/'.
    , _localPathContext     :: [PathSegment]
    -- ^ Path segments in the current routing context (including static ones)
    , _locationStatic       :: T.Text
    -- ^ Purely static path parts
    } deriving (Eq,Ord,Show)

--makeLenses ''LocationInfo


------------------------------------------------------------------------------
-- | Constructs an app's top-level LocationInfo from URL text.
mkLocationInfo
  :: T.Text
     -- ^ The path initial segments not used for dynamic routing
  -> URIRef Absolute
     -- ^ Full Url of target to route to
  -> LocationInfo
mkLocationInfo ctx r = LocationInfo r routeSegs ctxSegs ctx
  where
    ctxSegs = contextPathParts ctx

    -- TODO: What should happen if the route fails to prefix-match the
    -- static path segment? Throw at error? Here we just return [] for
    -- dynamic segments
    routeSegs = fromMaybe [] .
                L.stripPrefix (contextPathParts ctx) $
                uriPathParts r


-------------------------------------------------------------------------------
-- Enter a route by one segment; one routing segment is popped and moved
-- to the dynamic context
dropSegment :: LocationInfo -> LocationInfo
dropSegment li@(LocationInfo _ [] _ _) = li
dropSegment (LocationInfo uri (x:xs) ctx st) =
  LocationInfo uri xs (ctx ++ [x]) st


------------------------------------------------------------------------------
-- | Top-level routing function.
routeApp
    :: forall t m a.MonadWidget t m
    => T.Text
       -- ^ Static part of the path not used in dynamic routing
       -- Leading and trailing '/' will be added automatically
    -> RouteT t m a
    -> m a
routeApp staticPath app = do
    rec rUri :: Dynamic t (URIRef Absolute) <- route
          (decodeUtf8 . serializeURIRef' <$>
           (switch . current $ getLMost <$> rc))

        let locInfo = mkLocationInfo staticPath <$> rUri
        (a,rc) <- runRouteT app $ RoutingInfo locInfo staticPath
    return a

data RoutingInfo t = RoutingInfo
    { _routingInfoLocation   :: Dynamic t LocationInfo
    , _routingInfoStaticPath :: Text
    }

------------------------------------------------------------------------------
-- | Canonical implementation of the MonadRouted routing interface.
newtype RouteT t m a = RouteT
    { unRouteT :: ReaderT (RoutingInfo t)
                          (DynamicWriterT t (LMost t (URIRef Absolute)) m)
                          a
    }
  deriving (Functor, Applicative, Monad, MonadIO, MonadFix, MonadHold t,
            MonadSample t, MonadAsyncException, MonadException)

runRouteT
    :: MonadWidget t m
    => RouteT t m a
    -> RoutingInfo t
    -> m (a, Dynamic t (LMost t (URIRef Absolute)))
runRouteT (RouteT m) ri = runDynamicWriterT $ runReaderT m ri

instance MonadTrans (RouteT t) where
    lift = RouteT . lift . lift

instance MonadReader r m => MonadReader r (RouteT t m) where
  ask = lift ask
  local f (RouteT a) = RouteT $ mapReaderT (local f) a

instance MonadState s m => MonadState s (RouteT t m) where
  get = lift get
  put s = lift $ put s

#if MIN_VERSION_reflex(0,6,0)
instance DynamicWriter t w m => DynamicWriter t w (RouteT t m) where
  tellDyn = lift . tellDyn
#else
instance MonadDynamicWriter t w m => MonadDynamicWriter t w (RouteT t m) where
  tellDyn = lift . tellDyn
#endif

instance EventWriter t w m => EventWriter t w (RouteT t m) where
  tellEvent = lift . tellEvent

instance HasJSContext m => HasJSContext (RouteT t m) where
  type JSContextPhantom (RouteT t m) = JSContextPhantom m
  askJSContext = lift askJSContext

instance (MonadHold t m, MonadFix m, Adjustable t m) => Adjustable t (RouteT t m) where
  runWithReplace a0 a' = RouteT $ runWithReplace (coerce a0) (coerceEvent a')
  traverseIntMapWithKeyWithAdjust f dm0 dm' = RouteT $ traverseIntMapWithKeyWithAdjust
      (\k v -> unRouteT (f k v)) (coerce dm0) (coerceEvent dm')
  traverseDMapWithKeyWithAdjust f dm0 dm' = RouteT $ traverseDMapWithKeyWithAdjust
    (\k v -> unRouteT (f k v)) (coerce dm0) (coerceEvent dm')
  traverseDMapWithKeyWithAdjustWithMove f dm0 dm' = RouteT $ traverseDMapWithKeyWithAdjustWithMove
    (\k v -> unRouteT (f k v)) (coerce dm0) (coerceEvent dm')

instance MonadRef m => MonadRef (RouteT t m) where
  type Ref (RouteT t m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance MonadAtomicRef m => MonadAtomicRef (RouteT t m) where
  atomicModifyRef r = lift . atomicModifyRef r

instance MonadReflexCreateTrigger t m => MonadReflexCreateTrigger t (RouteT t m) where
  newEventWithTrigger = lift . newEventWithTrigger
  newFanEventWithTrigger f = lift $ newFanEventWithTrigger f

instance PerformEvent t m => PerformEvent t (RouteT t m) where
  type Performable (RouteT t m) = Performable m
  performEvent_ = lift . performEvent_
  performEvent = lift . performEvent

deriving instance TriggerEvent t m => TriggerEvent t (RouteT t m)

instance PostBuild t m => PostBuild t (RouteT t m) where
  getPostBuild = lift getPostBuild

------------------------------------------------------------------------------
-- | Minimal functionality necessary for the core routing interface that gives
-- you access to the contents of the location bar, lets you set its value, and
-- provides a mechanism for traversing the path in a hierarchical fashion.
class MonadRouted t m | m -> t where
    askRI :: m (RoutingInfo t)
    localRI :: (RoutingInfo t -> RoutingInfo t) -> m a -> m a
    tellUri :: Dynamic t (LMost t (URIRef Absolute)) -> m ()

instance MonadWidget t m => MonadRouted t (RouteT t m) where
    askRI = RouteT ask
    localRI f (RouteT m) = RouteT $ local f m
    tellUri = RouteT . tellDyn

-- | Returns the full parsed URI.
askUri
    :: (Monad m, Reflex t, MonadRouted t m)
    => m (Dynamic t (URIRef Absolute))
askUri = do
  locInfo <- _routingInfoLocation <$> askRI
  return $ _locationUri <$> locInfo

-- | Path segments of original url
askInitialSegments
    :: (Monad m, MonadJSM m, HasJSContext m, MonadRouted t m)
    => m [PathSegment]
askInitialSegments = do
  staticPath <- _routingInfoStaticPath <$> askRI
  uri0 <- getURI
  return $ _locationPathSegments $ mkLocationInfo staticPath uri0

-- | Changes the location bar to the value specified in the first parameter.
withPathSegment
    :: (Monad m, Reflex t, MonadRouted t m, MonadFix m, MonadHold t m)
    => (Dynamic t (Maybe PathSegment) -> m a)
    -> m a
withPathSegment f = do
  locInfo <- _routingInfoLocation <$> askRI
  top <- holdUniqDyn $ headMay . _locationPathSegments <$> locInfo
  localRI (\ri -> ri { _routingInfoLocation = dropSegment <$> _routingInfoLocation ri })
          (f top)
  where
    headMay []    = Nothing
    headMay (x:_) = Just x

-- | The latest full path as a text value.
dynPath :: (Monad m, Reflex t, MonadRouted t m, MonadFix m, MonadHold t m) => m (Dynamic t T.Text)
dynPath = do
  duri  <- askUri
  holdUniqDyn $ uriToPath <$> duri


-- | Change the location bar by appending the specifid value to the dynamic path
-- context
redirectInternal
    :: (Monad m, Reflex t, MonadRouted t m)
    => Event t Text
    -> m ()
redirectInternal pathPart = do
  locInfo <- _routingInfoLocation <$> askRI
  let reqs = attachWith constructReq (current locInfo) pathPart
  tellUri $ constDyn $ LMost reqs

-- | Passes the first path segment to the supplied widget and pops it off
-- the locationPathSegments field of the 'LocationInfo' that the widget
-- sees.
redirectLocal
    :: (Monad m, Reflex t, MonadRouted t m)
    => Event t Text
    -> m ()
redirectLocal pathPart = do
  locInfo <- _routingInfoLocation <$> askRI
  let reqs = attachWith appendingReq (current locInfo) pathPart
  tellUri $ constDyn $ LMost reqs


-------------------------------------------------------------------------------
-- Build a URI from an "absolute" link (those issued by redirectInternal)
-- Prepend just the static path parts
-- If the redirect path contains query or fragment parts, overwrite those
-- from the original load-time URI. Otherwise, keep those load-time parts
-- (New query params & fragments will not be saved between redirects,
--  they must be specified on every redirect event occurrence)
constructReq :: LocationInfo -> Text -> URIRef Absolute
constructReq (LocationInfo u _ _ st) p =
  let fullPath = "/" <> cleanT (st <> "/" <> cleanT p)

      -- Stringify (the original uri - query/frag) + (redirect)
      -- and reparse, to search for new query/frag
      fullUriString =
        serializeURIRef' u { uriQuery = Query [], uriFragment = Nothing } <> encodeUtf8 fullPath

      -- Re-parse the URI to check for new query/frag
      -- Disregard on parse error - parse error should only happen
      -- in response to a malformed redirect request
      fullUri' = either
                 (\_ -> trace ("URI Parse error for: " ++ show fullUriString) u)
                 id
                 (parseURI laxURIParserOptions fullUriString)
      query'   = if uriQuery fullUri' == Query []
                 then uriQuery u
                 else uriQuery fullUri'
      frag'    = if   uriFragment fullUri' `elem` [Nothing, Just ""]
                 then uriFragment u
                 else uriFragment fullUri'

  in  u { uriPath     = encodeUtf8 (T.takeWhile (/= '?') fullPath)
        , uriQuery    = query'
        , uriFragment = frag'
        }

-------------------------------------------------------------------------------
-- Build a URI from a link relative to the context
-- Prepend the static and context path parts
appendingReq :: LocationInfo -> Text -> URIRef Absolute
appendingReq (LocationInfo u _ ctx _) p =
  let fullPath = "/" <> cleanT (partsToPath ctx <> "/" <> cleanT p)
  in  u { uriPath = encodeUtf8 fullPath }

instance (MonadHold t m, MonadFix m, DomBuilder t m, NotReady t (RouteT t m)) => DomBuilder t (RouteT t m) where
  type DomBuilderSpace (RouteT t m) = DomBuilderSpace m
  element t cfg (RouteT child) = RouteT $ ReaderT $ \r -> DynamicWriterT $ do
    s <- get
    (e, (a, newS)) <- lift $ element t cfg $ runStateT (unDynamicWriterT (runReaderT child r)) s
    put newS
    return (e, a)
  inputElement = lift . inputElement
  textAreaElement = lift . textAreaElement
  selectElement cfg (RouteT child) = RouteT $ ReaderT $ \r -> DynamicWriterT $ do
    s <- get
    (e, (a, newS)) <- lift $ selectElement cfg $ runStateT (unDynamicWriterT (runReaderT child r)) s
    put newS
    return (e, a)


-------------------------------------------------------------------------------
-- Utility wrapper to collect route events with 'leftmost'
newtype LMost t a = LMost { getLMost :: Event t a}

instance Reflex t => Sem.Semigroup (LMost t a) where
  LMost a <> LMost b = LMost (leftmost [a,b])

instance Reflex t => Monoid (LMost t a) where
  mempty = LMost never
  mappend = (Sem.<>)

-------------------------------------------------------------------------------
-- Utility functions
partsToPath :: [PathSegment] -> T.Text
partsToPath = T.intercalate "/"

uriPathParts :: URIRef Absolute -> [PathSegment]
uriPathParts = pathParts . decodeUtf8 . uriPath

uriToPath :: URIRef Absolute -> T.Text
uriToPath = partsToPath . uriPathParts

pathParts :: T.Text -> [PathSegment]
pathParts = T.splitOn "/" . cleanT

contextPathParts :: T.Text -> [PathSegment]
contextPathParts = splitOn' "/" . cleanT

splitOn' :: Text -> Text -> [Text]
splitOn' _ "" = [] -- instead of [""] as returned by T.splitOn
splitOn' s x  = T.splitOn s x

cleanT :: T.Text -> T.Text
cleanT = T.dropWhile (== '/')
