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
  ) where

import           Prelude hiding ((.), id)
import           Control.Category (Category (..), (.))
import           Control.Lens hiding (Bifunctor, bimap, universe, element)
import           Control.Monad ((<=<))
import           Control.Monad.Fix
import           Control.Monad.Primitive
import           Control.Monad.Reader
import           Control.Monad.Ref
import           Control.Monad.Trans.Control
import           Data.Coerce
import qualified Data.List.NonEmpty        as NE
import           Data.List.NonEmpty        (NonEmpty)
import qualified Data.Map.Strict           as Map
import           Data.Monoid
import           Data.Proxy
import qualified Data.Set                  as Set
import           Data.Set                  (Set)
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Functor.Compose
import           Reflex.Class
import           Reflex.Host.Class
import           Reflex.PostBuild.Class
import           Reflex.TriggerEvent.Class
import           Reflex.PerformEvent.Class
import           Reflex.EventWriter.Class
import           Reflex.EventWriter.Base
import           Reflex.Dynamic
import           Reflex.Dom.Builder.Class
import           Data.Type.Coercion
import           Language.Javascript.JSaddle
import           Reflex.Dom.Core
import qualified GHCJS.DOM.Types as DOM
import           Network.URI
import           Data.Maybe (Maybe(..), fromMaybe, isJust)
import qualified Data.List as L

import           PkgId

newtype SetRouteT t r m a = SetRouteT { unSetRouteT :: EventWriterT t r m a }
  deriving (Functor, Applicative, Monad, MonadFix, MonadTrans, MonadIO, NotReady t, MonadHold t, MonadSample t, PostBuild t, TriggerEvent t, MonadReflexCreateTrigger t, HasDocument, DomRenderHook t)

instance (Semigroup r, MonadFix m, MonadHold t m, DomBuilder t m) => DomBuilder t (SetRouteT t r m) where
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

runSetRouteT :: (Semigroup r, Reflex t, Monad m) => SetRouteT t r m a -> m (a, Event t r)
runSetRouteT = runEventWriterT . unSetRouteT

class Reflex t => SetRoute t r m | m -> t r where
  setRoute :: Event t r -> m ()
  setRoute = setRoute

instance (Semigroup r, Reflex t, Monad m) => SetRoute t r (SetRouteT t r m) where
  setRoute = SetRouteT . tellEvent

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

instance (Semigroup r, MonadHold t m, Adjustable t m) => Adjustable t (SetRouteT t r m) where
  runWithReplace a0 a' = SetRouteT $ runWithReplace (coerce a0) $ coerceEvent a'
  traverseIntMapWithKeyWithAdjust f a0 a' = SetRouteT $ traverseIntMapWithKeyWithAdjust (coerce f) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjust f a0 a' = SetRouteT $ traverseDMapWithKeyWithAdjust (\k v -> coerce $ f k v) (coerce a0) $ coerce a'
  traverseDMapWithKeyWithAdjustWithMove f a0 a' = SetRouteT $ traverseDMapWithKeyWithAdjustWithMove (\k v -> coerce $ f k v) (coerce a0) $ coerce a'

instance (Monad m, MonadQuery t vs m) => MonadQuery t vs (SetRouteT t r m) where
  tellQueryIncremental = lift . tellQueryIncremental
  askQueryResult = lift askQueryResult
  queryIncremental = lift . queryIncremental

switchPkgRoute :: Set PkgIdxTs -> URI ->  PkgIdxTs -> Maybe URI 
switchPkgRoute setPkgIdx oldRoute (PkgIdxTs 0) = Nothing
switchPkgRoute setPkgIdx oldRoute idxChange =
  let routeS  = (T.pack . uriFragment) oldRoute
      rootURI = "#/package/"
  in case T.stripPrefix rootURI routeS of
    Just sfx | (Just pkgN, Just pkgIdx) <- pkgNFromText sfx
             , True <- idxChange /= pkgIdx
             , True <- not (Set.null setPkgIdx)
             , Just setMax <- Set.lookupMax setPkgIdx
               -> if setMax == idxChange
                  then Nothing
                  else parseURI . T.unpack $ rootURI <> (pkgNToText pkgN) <> (T.pack "@") <> (idxTsToText idxChange)
             | otherwise -> Nothing
    Nothing  -> Nothing

routeLink :: forall t m a route. ( DomBuilder t m, SetRoute t (NonEmpty FragRoute) m) 
          => Bool -- PreventDefault?
          -> Text -- Target route
          -> m a -- Child widget
          -> m a
routeLink True r w = do
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_eventSpec %~ addEventSpecFlags (Proxy :: Proxy (DomBuilderSpace m)) Click (\_ -> preventDefault)
        & elementConfig_initialAttributes .~ "href" =: r
  (e, a) <- element "a" cfg w
  setRoute $ (((decodeFrag r) NE.:| []) <$ domEvent Click e) 
  return a
routeLink False r w = do
  let cfg = (def :: ElementConfig EventResult t (DomBuilderSpace m))
        & elementConfig_initialAttributes .~ "href" =: r
  (e, a) <- element "a" cfg w
  setRoute $ (((decodeFrag r) NE.:| []) <$ domEvent Click e) 
  return a

routePkgIdxTs :: forall t m. ( PostBuild t m, MonadHold t m, MonadFix m, DomBuilder t m, SetRoute t (NonEmpty FragRoute) m) 
              => PkgN
              -> PkgIdxTs
              -> Behavior t (Set PkgIdxTs)
              -> Dynamic t (Map.Map PkgIdxTs Text) 
              -> DropdownConfig t PkgIdxTs
              -> m (Dropdown t PkgIdxTs)
routePkgIdxTs pn k0 setIdx opt ddConf = do
  dd <- dropdown k0 opt ddConf
  let evDD = attach setIdx (updated $ dd ^. dropdown_value)
  setRoute $ ((\val -> (createRoutePackage pn val) NE.:| []) <$> evDD)
  pure dd

createRoutePackage :: PkgN -> (Set PkgIdxTs, PkgIdxTs) -> FragRoute
createRoutePackage pn (_, PkgIdxTs 0) = RoutePackage (pn, Nothing)
createRoutePackage pn (setIdx, pkgIdx) 
  | Just maxIdx <- Set.lookupMax setIdx
  , True     <- maxIdx /= pkgIdx
  = RoutePackage (pn, Just pkgIdx)
  | otherwise = RoutePackage (pn, Nothing)

runRouteViewT  :: forall t m a. (TriggerEvent t m, PerformEvent t m, MonadHold t m, MonadJSM m, MonadJSM (Performable m), MonadFix m)
               => (Dynamic t FragRoute -> SetRouteT t (NonEmpty FragRoute) m a)
               -> m a
runRouteViewT app = mdo
  historyState <- manageHistory $ HistoryCommand_PushState <$> setState
  dynLoc <- browserHistoryWith getLocationUri
  (result, changeStateE) <- runSetRouteT $ app route -- changeStateE :: Event t (NonEmpty Fragroute)
  let 
    route :: Dynamic t FragRoute
    route = decodeFrag . T.pack . uriFragment <$> dynLoc

    f  (currentHistoryState, oldR) chStateE = -- chState :: NonEmpty Fragroute
      let newRoute = switchPkgRoute' oldR chStateE
          oldRoute = case encodeFrag oldR of
                       (Just a) -> a
                       Nothing  -> T.empty
      in 
        HistoryStateUpdate
        { _historyStateUpdate_state = DOM.SerializedScriptValue jsNull
        , _historyStateUpdate_title = ""
        , _historyStateUpdate_uri   = applyEncoding oldRoute newRoute (_historyItem_uri currentHistoryState)
        }
    setState = attachWith f ( (,) <$> current historyState <*> current route) changeStateE
  pure result

switchPkgRoute' :: FragRoute -> NonEmpty FragRoute -> Text
switchPkgRoute' oldFrag nEFrag
  | (fragR, _) <- NE.uncons nEFrag
  , Just fResult <- encodeFrag fragR
    = fResult
  | Just oldResult <- encodeFrag oldFrag
    = oldResult
  | otherwise = T.empty
  
encodeFrag :: FragRoute -> Maybe Text
encodeFrag RouteHome = Just "#/"
encodeFrag RouteQueue = Just "#/queue"
encodeFrag RoutePackages = Just "#/packages"
encodeFrag (RoutePackage (pkg, maybeIndex))
  | Just indexTs <- maybeIndex
    = Just $ "#/package/" <> (pkgNToText pkg) <> "@" <> (idxTsToText indexTs)
  | otherwise = Just $ "#/package/" <> (pkgNToText pkg)
encodeFrag (RouteUser usr) = Just $ "#/user/" <> usr
encodeFrag (RouteUnknown _) = Nothing

applyEncoding :: Text -> Text -> URI -> Maybe URI
applyEncoding oldR newR u 
  | oldR /= newR = Just $ u { uriFragment = T.unpack newR }
  | otherwise    = Nothing