module Components.PageLatest where

import Prelude (type (~>), Unit, Void, bind, const, pure, show, ($), (+), (<$>), (<>))
import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Data.Maybe (Maybe(..))
import Data.Traversable (Accum, mapAccumL)
import Lib.MatrixApi as Api
import Lib.Types as T

type State =
 {
   latestlist :: Array T.LatestItem
 , queuelist :: Array T.QueueItem
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
                    [ HP.id_ "build-list" ] $ buildPackages <$> state.latestlist

                ]
            , HH.div
                [ HP.class_ (H.ClassName "leftcol-2-right") ]
                [ HH.h2
                    [ HP.class_ (H.ClassName "main-header") ]
                    [ HH.text "Build Queue" ]
                , HH.table
                    [ HP.id_ "queue-list" ]
                    [ HH.tbody_ $ getTheResult accumResult ]
                ]
            ]
        ]
      where
        accumResult = mapAccumL renderTableQueue 1 (state.queuelist)
        getTheResult { value } = value

    eval :: Query ~> H.ComponentDSL State Query Void (Api.Matrix e)
    eval (Initialize next) = do
      st <- H.get
      qlist <- H.lift Api.getQueueList
      llist <- H.lift Api.getListLatestReports
      initState <- H.put $ st { latestlist = llist.items, queuelist = qlist.items }
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
    eval (RefreshListings next) = do
      pure next
    eval (Finalize next) = do
      pure next

buildPackages :: forall p i. T.LatestItem -> HH.HTML p i
buildPackages latestItem =
  HH.li_ $
    [ HH.a
        [ HP.href $ "#/package/" <> latestItem.packageName
        ]
        [ HH.text latestItem.packageName ]
    ] <> [ HH.small_ [ HH.text $ " - index-state: " <> (latestItem.modified) ] ]

renderTableQueue :: forall p. Int
                 -> T.QueueItem
                 -> Accum Int (HH.HTML p (Query Unit))
renderTableQueue num { packageName, priority } =
  { accum: num + 1
  , value: HH.tr
             [ HP.class_ (H.ClassName priority) ] $
             [ HH.td
                 [ HP.class_ (H.ClassName "num") ]
                 [ HH.text $ show num ]
             , HH.td
                 [ HP.class_ (H.ClassName "package-name") ]
                 [ HH.a
                     [HP.href $ "#/package/" <> packageName]
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
                     [ HP.class_ (H.ClassName "remove") ]
                     [ HH.text "╳"]
                 ]
             ]
  }
