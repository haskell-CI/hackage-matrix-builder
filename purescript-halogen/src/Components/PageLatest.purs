module Components.PageLatest where

import Prelude

import Data.Either.Nested (Either3)
import Data.Functor.Coproduct.Nested (Coproduct6)
import Data.Maybe (Maybe(..))
import Data.Semigroup ((<>))
import Data.Traversable (Accum, mapAccumL)
import Data.Show (show)
import Data.Array

import Control.Monad.Aff.Class
import Control.Monad.Reader.Class

import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lib.MatrixApi
import Lib.Uri
import Lib.Types (PackageName, LatestItem, QueueItem, Priority, ApiList)
import Lib.MiscFFI
import CSS.Display (Display, block, displayNone, display)
import Halogen.HTML.CSS as CSS

type State =
 {
   display :: Display
 , latestlist :: Array LatestItem
 , queuelist :: Array QueueItem
 }

data Query a
  = Initialize a
  | SelectedPackage PackageName a
  | PriorityUp QueueItem a
  | PriorityDown QueueItem a
  | RemoveQueue PackageName a
  | RefreshListings a
  | Finalize a

component :: forall e. H.Component HH.HTML Query Unit Void (MatrixApis e)
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
      { display: displayNone
      , latestlist: []
      , queuelist: []
      }

    render :: State -> H.ComponentHTML Query
    render state =
      HH.div
        [ HP.id_ "page-latest"
	, HP.class_ (H.ClassName "page")
	, CSS.style $ display state.display
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
	
    eval :: Query ~> H.ComponentDSL State Query Void (MatrixApis e)
    eval (Initialize next) = do
      st <- H.get
      qlist <- H.lift getQueueList
      llist <- H.lift getListLatestReports
      initState <- H.put $ st { display = displayNone, latestlist = llist.items, queuelist = qlist.items }	
      pure next
    
    eval (SelectedPackage pkgName next) = do			
      pure next
    eval (PriorityUp queueItem next) = do
      pure next
    eval (PriorityDown queueItem next) = do
      pure next
    eval (RemoveQueue pkgName next) = do
      pure next
    eval (RefreshListings next) = do
      pure next
    eval (Finalize next) = do
      pure next

buildPackages :: forall p i. LatestItem -> HH.HTML p i
buildPackages latestItem =
  HH.li_ $
    [ HH.a
        [ -- HP.href $ "/package/" <> packageMeta.name -- all of the package's name will goes here
	-- TODO: The action onClick will be added here to direct user to package's page
        ]
        [ HH.text latestItem.packageName ]
    ] <> [ HH.small_ [ HH.text $ " - index-state: " <> (latestItem.modified) ] ]

renderTableQueue :: forall p. Int -> QueueItem -> Accum Int (HH.HTML p (Query Unit))
renderTableQueue num queueItem@{ packageName, priority } =
  { accum: num + 1
  , value: HH.tr 
             [ HP.class_ (H.ClassName priority) ] $ 
             [ HH.td 
                 [ HP.class_ (H.ClassName "num") ] 
                 [ HH.text $ show num ]
	     , HH.td 
                 [ HP.class_ (H.ClassName "package-name") ] 
                 [ HH.text $ packageName ]
	     , HH.td 
                 [ HP.classes (H.ClassName <$> ["priority", priority ]) ]
                 [ HH.text $  priority ]
	     , HH.td 
                 [ HE.onClick $ HE.input_ (PriorityUp queueItem) ] 
                 [ HH.a
		     [ HP.class_ (H.ClassName "up") ]
		     [ HH.text "↑"]
		 ]
	     , HH.td
                 [ HE.onClick $ HE.input_ (PriorityDown queueItem) ] 
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
getQueueList :: forall e m. MonadReader { matrixClient :: MatrixApi } m
             => MonadAff (api :: API | e) m
	     => m (ApiList QueueItem)
getQueueList = do
  client <- asks _.matrixClient
  liftAff (queueList client)

getListLatestReports :: forall e m. MonadReader { matrixClient :: MatrixApi } m
                     => MonadAff (api :: API | e) m
	             => m (ApiList LatestItem)
getListLatestReports = do
  client <- asks _.matrixClient
  liftAff (listLatestReports client { count : (Just 100), offset : Nothing })

putQueueSaveByName :: forall e m. MonadReader { matrixClient :: MatrixApi } m
                     => MonadAff (api :: API | e) m
	             => PackageName
		     -> QueueItem
		     -> m Unit
putQueueSaveByName pkgName queueItem = do
  client <- asks _.matrixClient
  liftAff (queueSaveByName client pkgName queueItem)