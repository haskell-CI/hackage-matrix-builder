module Container where

import Data.Either
import Components.PageError as PageError
import Components.PageHome as PageHome
import Components.PageLatest as PageLatest
import Components.PagePackage as PagePackage
import Components.PagePackages as PagePackages
import Components.PageUser as PageUser
import Data.Array as Arr
import Data.String as Str
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lib.MatrixApi as Api
import Lib.Types as T
import Network.RemoteData as RD
import Control.Alt ((<|>))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (writeRef)
import Control.Monad.Reader (asks)
import Data.Either.Nested (Either6)
import Data.Functor.Coproduct.Nested (Coproduct6)
import Data.Maybe (Maybe(..))
import Prelude (type (~>), Unit, Void, absurd, bind, const, otherwise, pure, unit, ($), (<$>), (<$), (*>), (<*>), (==))
import Routing.Match (Match)
import Routing.Match.Class (lit, str)

import Debug.Trace (traceAnyA)

type State = {
    route :: T.PageRoute
  , package :: Array T.PackageMeta
}

data Query a =
    Initialize a
  | HandlePagePackage PagePackage.Message a
  | HandleSearchBox State T.PackageName a
  | RouteChange (Either String T.PageRoute) a

type ChildQuery = Coproduct6 PageError.Query
                             PageHome.Query
                             PageLatest.Query
                             PagePackage.Query
                             PagePackages.Query
                             PageUser.Query

type ChildSlot = Either6 Unit Unit Unit Unit Unit Unit

ui :: forall e. H.Component HH.HTML Query Unit Void (Api.Matrix e)
ui =
  H.lifecycleParentComponent
    { initialState: const initialState
    , render
    , eval
    , receiver: const Nothing
    , initializer: Just (H.action Initialize)
    , finalizer: Nothing
    }
  where
    initialState :: State
    initialState =
      { route: T.HomePage
      , package: []
      }

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Api.Matrix e)
    render st =
      HH.div
        [ HP.id_ "container"]
        [ renderNavigation st
        , case st.route of
            T.LatestPage ->
              HH.slot' CP.cp3 unit PageLatest.component unit absurd
            (T.PackagePage pkgName) ->
              HH.slot' CP.cp4 unit PagePackage.component
                  (getPackageMeta pkgName st.package) (HE.input HandlePagePackage)
            T.PackagesPage ->
              HH.slot' CP.cp5 unit PagePackages.component unit absurd
            (T.UserPage usr) ->
              HH.slot' CP.cp6 unit PageUser.component usr absurd
            T.HomePage ->
              HH.slot' CP.cp2 unit PageHome.component unit absurd
            _ ->
              HH.slot' CP.cp1 unit PageError.component unit absurd
        ]
      where
        renderNavigation st =
          HH.nav
            [ HP.id_ "menu"
            , HP.class_ (H.ClassName "clearfix")
            ]
            [ HH.div
                [ HP.classes (H.ClassName <$> ["item","left","logo-container","clearfix"]) ]
                [ HH.img
                    [ HP.class_ (H.ClassName "logo")
                    , HP.src "//www.haskell.org/static/img/logo.png"
                    , HP.alt "Haskell Logo"
                    ]
                , HH.h1
                    [ HP.class_ (H.ClassName "logo-text") ]
                    [ HH.text "Hackage Matrix Builder "
                    , HH.i_ [ HH.text "3", HH.sup_ [ HH.text "rd"] ]
                    ]
                ]
            , HH.div
                [ HP.classes (H.ClassName <$> ["item","link","left"]) ]
                [ HH.a [ HP.href "#" ] [ HH.text "Home" ] ]
            , HH.div
                [ HP.classes (H.ClassName <$> ["item","link","left"]) ]
                [ HH.a [ HP.href "#/latest" ] [ HH.text "Latest builds" ] ]
            , HH.div
                [ HP.classes (H.ClassName <$> ["item","link","left"]) ]
                [ HH.a [ HP.href "#/packages" ] [ HH.text "Packages" ] ]
            , HH.div
                [ HP.classes (H.ClassName <$> ["item","search","right","clearfix"]) ]
                [ HH.div
                    [ HP.class_ (H.ClassName "text") ]
                    [ HH.text "Package Search" ]
                , HH.input
                    [ HP.type_ HP.InputText
                    , HP.class_ (H.ClassName "input")
                    , HP.id_ "search"
                    , HP.autocomplete true
                    , HE.onValueInput (HE.input (HandleSearchBox st))
                    ]
                ]
            ]

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Api.Matrix e)
    eval (Initialize next) = do
      pkgList <- H.lift Api.getPackageList
      pkgRef <- asks _.packageList
      _ <- liftEff $ writeRef pkgRef (RD.Success pkgList)
      _ <- H.modify (_ { package = pkgList.items })
      pure next

    eval (HandlePagePackage PagePackage.TagAddOrRemove next) = do
      _ <- eval (Initialize next)
      _ <- H.query' CP.cp4 unit (H.request PagePackage.UpdateTag)
      pure next
    eval (HandleSearchBox st str next) = do
      let packages = Arr.filter (packageContained str) st.package
      pure next

    eval (RouteChange str next) = 
      case str of
        (Right pg ) -> do
          _ <- H.modify (_ { route = pg })
          _ <- traceAnyA pg
          pure next
        _ -> do
          _ <- H.modify _ { route = T.ErrorPage}
          _ <- traceAnyA str
          pure next

      {-|
        _ <- H.modify _ { route = str }
      pure next
      -}

routing :: Match T.PageRoute
routing =  latest
       <|> packages
       <|> package
       <|> user
       <|> error
       <|> logroute
       <|> home
  where
    slash = lit ""
    home = T.HomePage <$ slash
    latest = T.LatestPage <$ (slash *> lit "latest")
    packages = T.PackagesPage <$ (slash *> lit "packages")
    package = T.PackagePage <$> (slash *> lit "package" *> str)
    logroute = T.LogRoute <$> (lit "package" *> str) <*> ((pure "#") *> str)
    user = T.UserPage <$> (slash *> lit "user" *> str)
    error = T.ErrorPage <$ lit "error"

getPackageMeta :: T.PackageName -> Array T.PackageMeta  -> T.PackageMeta
getPackageMeta pkgName pkgMetaArr =
  case Arr.uncons filteredPkgMetaArr of
    Just { head: x, tail: xs } -> x
    Nothing                    -> { name: "", report: Nothing, tags: []}
  where
    filteredPkgMetaArr = Arr.filter (\x -> x.name == pkgName) pkgMetaArr

packageContained :: String -> T.PackageMeta -> Boolean
packageContained str pkgMeta
    | Str.contains (Str.Pattern str) pkgMeta.name = false
    | otherwise                                   = true
