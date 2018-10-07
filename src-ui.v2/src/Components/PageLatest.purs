module Components.PageLatest where

import Halogen as H
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lib.MatrixApi as Api
import Lib.MiscFFI as Misc
import Lib.Types as T
import Data.Maybe (Maybe(..))
import Prelude (type (~>), Unit, Void, bind, const, pure, show, otherwise, ($), (+), (-), (<$>), (<>), (<), (==))
import Data.Traversable as TR
import Network.RemoteData as RD

type State = { queuelist :: Array T.PackageQueue }

data Query a
  = Initialize a
  | PriorityUp T.PackageName T.PkgIdxTs Int a
  | PriorityDown T.PackageName T.PkgIdxTs Int a
  | RemoveQueue T.PackageName T.PkgIdxTs a
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
    initialState = { queuelist: [] }

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
                    [ HP.id_ "build-list" ] $ buildPackages <$> state.queuelist

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
        accumResult st = TR.mapAccumL (renderTableQueue st) 1 (state.queuelist)
        getTheResult { value } = value

    eval :: Query ~> H.ComponentDSL State Query Void (Api.Matrix e)
    eval (Initialize next) = do
      st <- H.get
      queueList <- H.lift Api.getQueues
      let qlist =
            case queueList of
              RD.Success a -> a
              _            -> []
      initState <- H.put $ st { queuelist = qlist }
      pure next
    eval (PriorityUp pkgName idx priority next) = do
      _ <- H.lift $ Api.putPackageQueue pkgName idx (priority + 10)
      st <- H.get
      queueList <- H.lift Api.getQueues
      let qlist =
            case queueList of
              RD.Success a -> a
              _            -> []

      _ <- H.modify _ { queuelist = qlist }
      pure next
    eval (PriorityDown pkgName idx priority next) = do
      _ <- H.lift $ Api.putPackageQueue pkgName idx (priority - 10)
      st <- H.get
      queueList <- H.lift Api.getQueues
      let qlist =
            case queueList of
              RD.Success a -> a
              _            -> []
      _ <- H.modify _ { queuelist = qlist }
      pure next
    eval (RemoveQueue pkgName idx next) = do
      _ <- H.lift $ Api.deletePackageQueue pkgName idx
      eval (Initialize next)
    eval (RefreshListings next) = eval (Initialize next)
    eval (Finalize next) = do
      pure next

buildPackages :: forall p i. T.PackageQueue -> HH.HTML p i
buildPackages {pkgname, modified} =
  HH.li_ $
    [ HH.a
        [ HP.href $ "#/package/" <> pkgname
        ]
        [ HH.text pkgname ]
    ] <> [ HH.small_ [ HH.text $ " - index-state: " <> (modified) ] ]

renderTableQueue :: forall p. State
                 -> Int
                 -> T.PackageQueue
                 -> TR.Accum Int (HH.HTML p (Query Unit))
renderTableQueue state num { pkgname, priority, idxstate } =
  { accum: num + 1
  , value: HH.tr
             [ HP.class_ (H.ClassName (Misc.showPrio priority)) ] $
             [ HH.td
                 [ HP.class_ (H.ClassName "num") ]
                 [ HH.text $ show num ]
             , HH.td
                 [ HP.class_ (H.ClassName "package-name") ]
                 [ HH.a
                     [HP.href $ "#/package/" <> pkgname
                     ]
                     [HH.text $ pkgname]
                 ]
             , HH.td
                 [ HP.classes (H.ClassName <$> ["priority", (Misc.showPrio priority) ]) ]
                 [ HH.text $ (Misc.showPrio priority) ]
             , HH.td
                 [ HE.onClick $ HE.input_ (PriorityUp pkgname idxstate priority) ]
                 [ HH.a
                     [ HP.class_ (H.ClassName "up") ]
                     [ HH.text "↑"]
                 ]
             , HH.td
                 [ HE.onClick $ HE.input_ (PriorityDown pkgname idxstate priority) ]
                 [ HH.a
                     [ HP.class_ (H.ClassName "down") ]
                     [ HH.text "↓"]
                 ]
             , HH.td_
                 --[ HE.onClick $ HE.input_ (RemoveQueue pkgname idxstate) ]
                 [ HH.a
                     [ HP.class_ (H.ClassName "remove")
                     , HE.onClick $ HE.input_ (RemoveQueue pkgname idxstate )
                     ]
                     [ HH.text "╳"]
                 ]
             ]
  }

