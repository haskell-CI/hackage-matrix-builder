{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}
{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FunctionalDependencies #-}

module Router 
  ( SetRouteT(..)
  , SetRoute(..)
  , runSetRouteT
  , mapSetRouteT
  , switchPkgRoute
  , routeLink
  , routePkgIdxTs
  , runRouteViewT
  , runRouteViewT'
  , FragRoute(..), decodeFrag, encodeFrag
  ) where

import           Prelude hiding ((.), id)
import           Control.Category (Category (..), (.))
import           Control.Lens hiding (Bifunctor, bimap, universe, element)
import           Control.Monad.Fix
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.Ref
import qualified Data.Char                 as C
import           Data.Coerce
import           Data.Monoid
import           Data.Proxy
import qualified Data.Set                  as Set
import           Data.Set                  (Set)
import           Data.Text (Text)
import qualified Data.Text as T
import           Reflex.Class
import           Reflex.Host.Class
import           Reflex.PostBuild.Class
import           Reflex.TriggerEvent.Class
import           Reflex.PerformEvent.Class
import           Reflex.EventWriter.Class
import           Reflex.EventWriter.Base
import           Reflex.Dom.Builder.Class
import           Language.Javascript.JSaddle (MonadJSM, jsNull)
import           Reflex.Dom.Core
import qualified GHCJS.DOM as DOM
import qualified GHCJS.DOM.WindowEventHandlers as DOM
import qualified GHCJS.DOM.EventM as DOM
import           GHCJS.DOM.Types (History, Window, SerializedScriptValue (..), liftJSM, fromJSVal, toJSVal)
import qualified GHCJS.DOM.Window as Window
import qualified GHCJS.DOM.History as History
import qualified GHCJS.DOM.Location as Location
import qualified GHCJS.DOM.PopStateEvent as PopStateEvent
import           Network.URI
import           Data.Maybe (Maybe(..), fromMaybe)

import           PkgId

data FragRoute = RouteHome
               | RouteQueue
               | RoutePackages
               | RoutePackage PkgN
               | RouteUser UserName
               | RouteUnknown T.Text
               deriving (Eq, Ord)

newtype SetRouteT t r m a = SetRouteT { unSetRouteT :: EventWriterT t (Endo r) m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadTrans, MonadIO, NotReady t, MonadHold t, MonadSample t, PostBuild t, TriggerEvent t, MonadReflexCreateTrigger t, HasDocument, DomRenderHook t)

instance (MonadFix m, MonadHold t m, DomBuilder t m) => DomBuilder t (SetRouteT t r m) where
  type DomBuilderSpace (SetRouteT t r m) = DomBuilderSpace m
  element t cfg child = SetRouteT $ element t cfg $ unSetRouteT child
  inputElement = lift . inputElement
  textAreaElement = lift . textAreaElement
  selectElement cfg child = SetRouteT $ selectElement cfg $ unSetRouteT child

instance HasJSContext m => HasJSContext (SetRouteT t r m) where
  type JSContextPhantom (SetRouteT t r m) = JSContextPhantom m
  askJSContext = lift askJSContext

mapSetRouteT :: (forall x. m x -> n x) -> SetRouteT t r m a -> SetRouteT t r n a
mapSetRouteT f (SetRouteT x) = SetRouteT (mapEventWriterT f x)

runSetRouteT :: (Reflex t, Monad m) => SetRouteT t r m a -> m (a, Event t (Endo r))
runSetRouteT = runEventWriterT . unSetRouteT

class Reflex t => SetRoute t r m | m -> t r where
  setRoute :: Event t (r -> r) -> m ()
  setRoute = setRoute

instance (Reflex t, Monad m) => SetRoute t r (SetRouteT t r m) where
  setRoute = SetRouteT . tellEvent . fmap Endo

instance (Monad m, SetRoute t r m) => SetRoute t r (QueryT t q m)

instance (Monad m, SetRoute t r m) => SetRoute t r (EventWriterT t w m)

--instance (Monad m, SetRoute t r m) => SetRoute t r (RoutedT t r' m)

instance (Monad m, SetRoute t r m) => SetRoute t r (ReaderT r' m)

instance Requester t m => Requester t (SetRouteT t r m) where
  type Request (SetRouteT t r m) = Request m
  type Response (SetRouteT t r m) = Response m
  requesting = SetRouteT . requesting
  requesting_ = SetRouteT . requesting_

instance (Monad m, SetRoute t r m) => SetRoute t r (RequesterT t req rsp m)

#ifndef ghcjs_HOST_OS
deriving instance MonadJSM m => MonadJSM (SetRouteT t r m)
#endif

instance PerformEvent t m => PerformEvent t (SetRouteT t r m) where
  type Performable (SetRouteT t r m) = Performable m
  performEvent = lift . performEvent
  performEvent_ = lift . performEvent_

instance MonadRef m => MonadRef (SetRouteT t r m) where
  type Ref (SetRouteT t r m) = Ref m
  newRef = lift . newRef
  readRef = lift . readRef
  writeRef r = lift . writeRef r

instance HasJS x m => HasJS x (SetRouteT t r m) where
  type JSX (SetRouteT t r m) = JSX m
  liftJS = lift . liftJS

instance PrimMonad m => PrimMonad (SetRouteT t r m ) where
  type PrimState (SetRouteT t r m) = PrimState m
  primitive = lift . primitive

instance (MonadHold t m, Adjustable t m) => Adjustable t (SetRouteT t r m) where
  runWithReplace a0 a' = SetRouteT $ runWithReplace (coerce a0) $ coerceEvent a'
  traverseIntMapWithKeyWithAdjust f a0 a' = SetRouteT $ traverseIntMapWithKeyWithAdjust (coerce f) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjust f a0 a' = SetRouteT $ traverseDMapWithKeyWithAdjust (\k v -> coerce $ f k v) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjustWithMove f a0 a' = SetRouteT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> coerce $ f k v) (coerce a0) $ coerce a'

instance (Monad m, MonadQuery t vs m) => MonadQuery t vs (SetRouteT t r m) where
  tellQueryIncremental = lift . tellQueryIncremental
  askQueryResult = lift askQueryResult
  queryIncremental = lift . queryIncremental

routeLink :: forall t m a. ( DomBuilder t m, SetRoute t FragRoute m) 
          => Bool -- PreventDefault?
          -> Text -- Target route
          -> m a -- Child widget
          -> m a
routeLink True r w = do
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
        & elementConfig_initialAttributes .~ "href" =: r
  (e, a) <- element "a" cfg w
  setRoute $ (switchPkgRoute (Just $ decodeFrag r)) <$ domEvent Click e
  return a
routeLink False r w = do
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_initialAttributes .~ "href" =: r
  (e, a) <- element "a" cfg w
  setRoute $ (switchPkgRoute (Just $ decodeFrag r)) <$ domEvent Click e 
  return a

routePkgIdxTs :: forall t m. (PostBuild t m, MonadHold t m, MonadFix m, DomBuilder t m, SetRoute t FragRoute m) 
              => PkgN
              -> Dynamic t (Set PkgIdxTs)
              -> Dynamic t PkgIdxTs
              -> m ()
routePkgIdxTs pn setIdx ddIdx = do
  let evDD = updated $ ffor2 setIdx ddIdx (\sId dVal -> createRoutePackage pn sId dVal)
  setRoute $ switchPkgRoute <$> evDD
  pure ()

createRoutePackage :: PkgN -> Set PkgIdxTs -> PkgIdxTs -> Maybe FragRoute
createRoutePackage _ _ (PkgIdxTs 0) = Nothing
createRoutePackage (PkgN pn) setIdx pkgIdx
  | Just maxIdx <- Set.lookupMax setIdx
  , True     <- maxIdx /= pkgIdx
  = Just $ RoutePackage (PkgN $ T.append pn (cons '@' $ idxTsToText pkgIdx))
  | otherwise = Just $ RoutePackage (PkgN pn)

runRouteViewT  :: forall t m. (Adjustable t m, TriggerEvent t m, PerformEvent t m, MonadHold t m, MonadJSM m, MonadJSM (Performable m), MonadFix m)
               => (FragRoute -> SetRouteT t FragRoute m ())
               -> m () --(Dynamic t (Maybe Text))
runRouteViewT app = mdo
  historyState <- manageHistory $ HistoryCommand_PushState <$> setState

  let 
    dynLoc = _historyItem_uri <$> historyState

    route :: Dynamic t FragRoute
    route = decodeFrag . T.pack . uriFragment <$> dynLoc

    setState = attachWithMaybe switchRoutingState ( (,) <$> current historyState <*> current route) changeStateE
  (result, changeStateE) <- runSetRouteT $ strictDynWidget_ app route
  pure result

runRouteViewT'  :: forall t m. (MonadJSM m, Adjustable t m, TriggerEvent t m, PerformEvent t m, MonadHold t m, MonadFix m, MonadJSM (Performable m))
               => (FragRoute -> SetRouteT t FragRoute m ())
               -> m () --(Dynamic t (Maybe Text))
runRouteViewT' app = mdo
  window <- DOM.currentWindowUnchecked
  history <- Window.getHistory window
  location <- Window.getLocation window
  oldUri <- (decodeFrag . T.pack . uriFragment) <$> getLocationUri location
  backState <- wrapDomEvent window (`DOM.on` DOM.popState) $ do
    e <- DOM.event
    jV <- PopStateEvent.getState e
    oUri <- liftJSM $ fromJSVal jV
    pure $ decodeFrag $ fromMaybe (T.pack "") oUri
  setState <- performEvent $ attachWith (switchRoutingState' history) (current route) changeStateE
  route <- foldDynMaybe switchFrag oldUri $ setState
  (result, changeStateE) <- runSetRouteT $ strictDynWidget_ app route
  pure result

switchFrag :: FragRoute -> FragRoute -> Maybe FragRoute
switchFrag newFrag oldFrag
  | True <- newFrag == oldFrag
  = Nothing
  | (RoutePackage _) <- newFrag
  , (RoutePackage _) <- oldFrag
  = Just newFrag
  | otherwise = Just newFrag

{-backRoutingState :: (MonadJSM m) => History -> m FragRoute
backRoutingState hs = wrapDomEvent window (`DOM.on` DOM.popState) $ do
  e <- DOM.event
  jV <- PopStateEvent.getState e
  oUri <- liftJSM $ fromJSVal jV
  pure $ decodeFrag $ fromMaybe (T.pack "") oUri
-}
switchRoutingState' :: (MonadJSM m) => History -> FragRoute -> Endo FragRoute -> m FragRoute
switchRoutingState' hs oldR chStateE = do
  let newRoute' = appEndo chStateE oldR
      newRoute  = encodeFrag newRoute'
      oldRoute  = fromMaybe (T.pack "") $ encodeFrag oldR
      histRoute = fromMaybe (T.pack "") $ newRoute

  newState <- liftJSM $ toJSVal histRoute
  History.pushState hs 
                    (SerializedScriptValue newState)
                    histRoute
                    (applyEncoding' oldRoute newRoute)
  pure newRoute'

switchRoutingState :: (HistoryItem, FragRoute) -> Endo FragRoute -> Maybe HistoryStateUpdate
switchRoutingState (currentHS, oldR) chStateE = -- chState :: Endo (FragRoute -> FragRoute)
  let newR     = appEndo chStateE oldR
      switchF  = switchFrag newR oldR
      oldRoute = fromMaybe (T.pack "") $ encodeFrag oldR
      
  in case switchF of
    Nothing -> Nothing
    Just nR -> Just $ HistoryStateUpdate
                       { _historyStateUpdate_state = SerializedScriptValue jsNull
                       , _historyStateUpdate_title = ""
                       , _historyStateUpdate_uri   = applyEncoding oldRoute nR (_historyItem_uri currentHS)
                       }

switchPkgRoute :: Maybe FragRoute -> FragRoute -> FragRoute
switchPkgRoute newFrag oldFrag = fromMaybe oldFrag newFrag
  
encodeFrag :: FragRoute -> Maybe Text
encodeFrag RouteHome = Just "#/"
encodeFrag RouteQueue = Just "#/queue"
encodeFrag RoutePackages = Just "#/packages"
encodeFrag (RoutePackage (PkgN pkg)) = Just $ "#/package/" <> pkg
encodeFrag (RouteUser usr) = Just $ "#/user/" <> usr
encodeFrag (RouteUnknown _) = Nothing

applyEncoding' :: Text -> Maybe Text -> Maybe Text
applyEncoding' _ Nothing = Nothing
applyEncoding' oldR (Just newR)
  | oldR /= newR = Just $ newR
  | otherwise    = Nothing

applyEncoding :: Text -> FragRoute -> URI -> Maybe URI
applyEncoding oldR r u 
  | Just newR <- encodeFrag r
  , oldR /= newR = Just $ u { uriFragment = T.unpack newR }
  | otherwise    = Nothing

decodeFrag :: T.Text -> FragRoute
decodeFrag frag = case frag of
    ""           -> RouteHome
    "#"          -> RouteHome
    "#/"         -> RouteHome
    "#/queue"    -> RouteQueue
    "#/packages" -> RoutePackages

    _ | Just sfx <- T.stripPrefix "#/package/" frag
      , not (T.null frag)
      , Just pn <- pkgNFromText sfx
        -> RoutePackage pn

      | Just sfx <- T.stripPrefix "#/user/" frag
      , not (T.null frag)
      , T.all (\c -> C.isAsciiLower c || C.isAsciiUpper c || C.isDigit c || c == '_') sfx
        -> RouteUser sfx

      | otherwise -> RouteUnknown frag

strictDynWidget_ :: forall t m. ( MonadSample t m, MonadHold t m, Adjustable t m) => (FragRoute -> m ()) -> Dynamic t FragRoute -> m ()
strictDynWidget_ f r = do
  r0 <- sample $ current r
  (_, _) <- runWithReplace (f r0) $ f <$> updated r
  pure ()