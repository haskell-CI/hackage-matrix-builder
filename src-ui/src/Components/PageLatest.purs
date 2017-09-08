module Components.PageLatest where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lib.MatrixApi as Api
import Lib.Types as T
import Data.Maybe (Maybe(..))
import Data.Array as Arr
import Data.Traversable (Accum, mapAccumL)
import Prelude (type (~>), Unit, Void, bind, const, pure, show, ($), (+), (<$>), (<>), (<<<), (/=))
import Data.Argonaut as Arg
import Data.Traversable as TR
import Data.Tuple as Tuple
import Network.RemoteData as RD
import Network.HTTP.Affjax as Affjax
import Network.HTTP.Affjax.Response as Affjax
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Data.Int as Int

type State =
 {
   latestlist :: Array T.LatestItem
 , queuelist :: Array T.QueueItem
 , packageIndexLatest :: Array (Tuple.Tuple T.PackageName String)
 , packageIndexQueue :: Array (Tuple.Tuple T.PackageName String)
 }

data Query a
  = Initialize a
  | PriorityUp T.PackageName String a
  | PriorityDown T.PackageName String a
  | RemoveQueue T.PackageName a
  | RefreshListings a
  | Finalize a

component :: forall e. H.Component HH.HTML Query Unit Void (Api.Matrix e)
component = H.lifecycleComponent
  { initialState: const initialState
  , render
  , eval
  , initializer: Just (H.action Initialize)
  , finalizer: Just (H.action Finalize)
  , receiver: const Nothing
  }
  where

    initialState :: State
    initialState =
      { latestlist: []
      , queuelist: []
      , packageIndexLatest: []
      , packageIndexQueue: []
      }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
        [ HP.id_ "page-latest"
        , HP.class_ (H.ClassName "page")
        ]
        [ HH.div
            [ HP.class_ (H.ClassName "rightcol") ]
            [ HH.div
                [ HP.class_ (H.ClassName "sub") ]
                [ HH.text "Times are shown in your timezone" ]
            , HH.div
                [ HP.class_ (H.ClassName "sub") ]
                [ HH.button
                    [ HP.class_ (H.ClassName "refresh")
                    , HP.title "Refresh listings"
                    , HE.onClick $ HE.input_ (RefreshListings)
                    ]
                    [ HH.text "Refresh listings" ]
                ]
            ]
        , HH.div
            [ HP.classes (H.ClassName <$> ["leftcol", "col-2"])]
            [ HH.div
                [ HP.class_ (H.ClassName "leftcol-2-left")]
                [ HH.h2
                    [ HP.class_ (H.ClassName "main-header") ]
                    [ HH.text "Latest Builds" ]
                , HH.ul
                    [ HP.id_ "build-list" ] $ buildPackages state <$> state.latestlist

                ]
            , HH.div
                [ HP.class_ (H.ClassName "leftcol-2-right") ]
                [ HH.h2
                    [ HP.class_ (H.ClassName "main-header") ]
                    [ HH.text "Build Queue" ]
                , HH.table
                    [ HP.id_ "queue-list" ]
                    [ HH.tbody_ $ getTheResult (accumResult state) ]
                ]
            ]
        ]
      where
        accumResult st = mapAccumL (renderTableQueue st) 1 (state.queuelist)
        getTheResult { value } = value

    eval :: Query ~> H.ComponentDSL State Query Void (Api.Matrix e)
    eval (Initialize next) = do
      st <- H.get
      qList <- H.lift Api.getQueueList
      lList <- H.lift Api.getListLatestReports
      pkgIdxLatest <- TR.traverse latestIndex (_.packageName <$> lList.items)
      pkgIdxQueue <- TR.traverse latestIndex (_.packageName <$> qList.items)
      initState <- H.put $ st { latestlist = lList.items
                              , queuelist = qList.items
                              , packageIndexLatest = pkgIdxLatest
                              , packageIndexQueue = pkgIdxQueue
                              }
      pure next
    eval (PriorityUp pkgName priority next) = do
      _ <- H.lift $ Api.putQueueSaveByName pkgName (case priority of
                                                     "low" -> T.Medium
                                                     "medium" -> T.High
                                                     _   -> T.High )
      st <- H.get
      qlist <- H.lift Api.getQueueList
      _ <- H.modify _ { queuelist = qlist.items }
      pure next
    eval (PriorityDown pkgName priority next) = do
      _ <- H.lift $ Api.putQueueSaveByName pkgName (case priority of
                                                     "low" -> T.Low
                                                     "medium" -> T.Low
                                                     _   -> T.Medium )
      st <- H.get
      qlist <- H.lift Api.getQueueList
      _ <- H.modify _ { queuelist = qlist.items }
      pure next
    eval (RemoveQueue pkgName next) = do
      _ <- H.lift $ Api.deleteQueueRemove pkgName
      pure next
    eval (RefreshListings next) = eval (Initialize next)
    eval (Finalize next) = do
      pure next

buildPackages :: forall p i. State -> T.LatestItem -> HH.HTML p i
buildPackages state latestItem =
  HH.li_ $
    [ HH.a
        [ HP.href $ "#/package/" <> latestItem.packageName
                                 <> lookupIndex latestItem.packageName state.packageIndexLatest
        ]
        [ HH.text latestItem.packageName ]
    ] <> [ HH.small_ [ HH.text $ " - index-state: " <> (latestItem.modified) ] ]

lookupIndex :: T.PackageName -> Array (Tuple.Tuple T.PackageName String) -> String
lookupIndex pkgName pkgIdxTuple =
  case Tuple.lookup pkgName pkgIdxTuple of
    Just a  -> "@" <> a
    Nothing -> ""

latestIndex :: forall e m. MonadAff (ajax :: Affjax.AJAX | e) m
            => T.PackageName
            -> m (Tuple.Tuple T.PackageName String)
latestIndex pkgName = do
  idx <- Api.getTimestamp pkgName
  case idx of
    RD.Success idx -> do
      getIdx ((show <<< Int.round) <$> (fromIndexToNumber (Arg.toArray idx))) pkgName
    _              -> pure $ Tuple.Tuple "" ""
  where
    getIdx idx' pkg'=
      case Arr.last idx' of
        Just a  -> pure $ Tuple.Tuple pkg' a
        Nothing -> pure $ Tuple.Tuple "" ""

fromIndexToNumber :: Maybe Arg.JArray -> Array Number
fromIndexToNumber (Just arrJson) =
  case TR.traverse Arg.toNumber arrJson of
    Just arrStr -> arrStr
    Nothing      -> []
fromIndexToNumber Nothing        = []


renderTableQueue :: forall p. State
                 -> Int
                 -> T.QueueItem
                 -> Accum Int (HH.HTML p (Query Unit))
renderTableQueue state num { packageName, priority } =
  { accum: num + 1
  , value: HH.tr
             [ HP.class_ (H.ClassName priority) ] $
             [ HH.td
                 [ HP.class_ (H.ClassName "num") ]
                 [ HH.text $ show num ]
             , HH.td
                 [ HP.class_ (H.ClassName "package-name") ]
                 [ HH.a
                     [HP.href $ "#/package/" <> packageName
                      <> lookupIndex packageName state.packageIndexQueue
                     ]
                     [HH.text $ packageName]
                 ]
             , HH.td
                 [ HP.classes (H.ClassName <$> ["priority", priority ]) ]
                 [ HH.text $  priority ]
             , HH.td
                 [ HE.onClick $ HE.input_ (PriorityUp packageName priority) ]
                 [ HH.a
                     [ HP.class_ (H.ClassName "up") ]
                     [ HH.text "↑"]
                 ]
             , HH.td
                 [ HE.onClick $ HE.input_ (PriorityDown packageName priority) ]
                 [ HH.a
                     [ HP.class_ (H.ClassName "down") ]
                     [ HH.text "↓"]
                 ]
             , HH.td
                 [ HE.onClick $ HE.input_ (RemoveQueue packageName) ]
                 [ HH.a
                     [ HP.class_ (H.ClassName "remove")
                     , HE.onClick $ HE.input_ (RemoveQueue packageName )
                     ]
                     [ HH.text "╳"]
                 ]
             ]
  }
