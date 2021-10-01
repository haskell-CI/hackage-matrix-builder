module Container where

import Components.PageError as PageError
import Components.PageHome as PageHome
import Components.PageLatest as PageLatest
import Components.PagePackage as PagePackage
import Components.PagePackages as PagePackages
import Components.PageUser as PageUser
import Control.Monad.Eff.JQuery as J
import Data.Array as Arr
import Data.String as Str
import Data.String.Regex as Rgx
import Data.String.Regex.Flags as RXF
import Global as G
import Halogen as H
import Halogen.Component.ChildPath as CP
import Halogen.HTML as HH
import Halogen.HTML.Events as HE
import Halogen.HTML.Properties as HP
import Lib.MatrixApi as Api
import Lib.MiscFFI as Misc
import Lib.Types as T
import Network.RemoteData as RD
import Control.Alt ((<|>))
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Ref (writeRef)
import Control.Monad.Reader (asks)
import DOM.HTML (window) as DOM
import DOM.HTML.Location (setHref, origin) as DOM
import DOM.HTML.Window (location) as DOM
import DOM.Event.Types (KeyboardEvent) as DOM
import DOM.Event.KeyboardEvent (key) as DOM
import Data.Either (fromRight)
import Data.Either.Nested (Either6)
import Data.Functor.Coproduct.Nested (Coproduct6)
import Data.Maybe (Maybe(..))
import Data.Tuple.Nested as TupleN
import Data.Foldable as Fold
import Partial.Unsafe (unsafePartial)
import Prelude (type (~>), Unit, Void, absurd, bind, const, pure, unit, ($), (<$>), (<$), (*>), (<*>), (==), (>>=), (<>))
import Routing.Match (Match)
import Routing.Match.Class (lit, str)

type State = {
    route :: T.PageRoute
  , packages :: Array T.PackageName
  , searchPkg :: T.PackageName
}

data Query a =
    Initialize a
  | HandlePagePackage PagePackage.Message a
  | HandleSearchBox State T.PackageName a
  | RouteChange T.PageRoute a
  | SearchBoxChange T.PackageName a
  | HandlePackage T.PackageName a
  | HandleKeyboard DOM.KeyboardEvent a

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
      { route: T.ErrorPage
      , packages: []
      , searchPkg: ""
      }

    render :: State -> H.ParentHTML Query ChildQuery ChildSlot (Api.Matrix e)
    render st =
      HH.div
        [ HP.id_ "container"]
        [ renderNavigation st
        , case st.route of
            T.LatestPage ->
              HH.slot' CP.cp3 unit PageLatest.component unit absurd
            (T.PackagePageVersion pkgName verName hcVer) ->
              HH.slot' CP.cp4 unit PagePackage.component
                  (getPackageMeta (Misc.makeTuplePkgVer pkgName verName hcVer) st.packages) (HE.input HandlePagePackage)
            (T.PackagePage pkgName) ->
              HH.slot' CP.cp4 unit PagePackage.component
                  (getPackageMeta (Misc.makeTuplePkgIdx pkgName) st.packages) (HE.input HandlePagePackage)
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
        renderNavigation state =
          HH.nav
            [ HP.id_ "menu"
            , HP.class_ (H.ClassName "clearfix")
            ]
            [ HH.div
                [ HP.classes (H.ClassName <$> ["item","left","logo-container","clearfix"]) ]
                [ HH.img
                    [ HP.class_ (H.ClassName "logo")
                    , HP.src "//hackage.haskell.org/static/icons/ic_haskell_grayscale_32.svg"
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
                    , HP.attr (H.AttrName "autocapitalize") "none"
                    , HE.onValueInput (HE.input (HandleSearchBox state))
                    , HE.onKeyPress (HE.input HandleKeyboard)
                    ]
                ]
            ]

    eval :: Query ~> H.ParentDSL State Query ChildQuery ChildSlot Void (Api.Matrix e)
    eval (Initialize next) = do
      pkgList <- H.lift Api.getPackages
      pkgRef <- asks _.packages
      _ <- liftEff $ writeRef pkgRef pkgList
      _ <- H.modify (_ { packages =
                           case pkgList of
                             RD.Success arr -> arr
                             _              -> []
                       }
                    )
      pure next

    eval (HandlePagePackage PagePackage.FromPagePackage next) = eval (Initialize next)

    eval (HandleSearchBox st str next) = do
      let packages = Arr.filter (packageContained str) st.packages
      _ <- H.subscribe
            (H.eventSource
              (\k -> J.select "#search" >>= Misc.autocomplete { source: packages
                                                              , select: k
                                                              })
              (\a -> Just $ SearchBoxChange (a.item.value) H.Listening))
      if Fold.notElem str packages
        then pure next
        else eval (HandlePackage str next)

    eval (RouteChange str next) = do
      _ <- H.modify _ { route = str }
      eval (Initialize next)

    eval (SearchBoxChange pkgName next) = do
      loc <- liftEff $ DOM.window >>= DOM.location
      ori <- liftEff $ DOM.window >>= DOM.location >>= DOM.origin
      _ <- liftEff $ DOM.setHref (ori <> "#/package/" <> pkgName) loc
      eval (HandlePagePackage PagePackage.FromPagePackage next)

    eval (HandlePackage pkgName next) = do
      _ <- H.modify _ { searchPkg = pkgName }
      pure next

    eval (HandleKeyboard key next) = do
      -- _ <- traceAnyA (DOM.key key)
      let
        str = DOM.key key
      case str of
        "Enter" -> do
          st <- H.get
          eval (SearchBoxChange st.searchPkg next)
        _       -> pure next

routing :: Match T.PageRoute
routing =  latest
       <|> packages
       <|> packageVersion
       <|> package
       <|> user
       <|> error
       <|> home
  where
    slash = lit ""
    home = T.HomePage <$ slash
    latest = T.LatestPage <$ (slash *> lit "latest")
    packages = T.PackagesPage <$ (slash *> lit "packages")
    packageVersion = T.PackagePageVersion  <$> (slash *> lit "package" *> str) <*> str <*> str
    package = T.PackagePage <$> (slash *> lit "package" *> str)
    user = T.UserPage <$> (slash *> lit "user" *> str)
    error = T.ErrorPage <$ lit "error"

getPackageMeta :: T.InitialPackage -> Array T.PackageName  -> T.InitialPackage
getPackageMeta initPkg pkgArr =
  case Arr.uncons (filteredPkgArr (TupleN.get1 initPkg) pkgArr) of
    Just { head: x, tail: xs } -> TupleN.tuple4 x (TupleN.get2 initPkg) (TupleN.get3 initPkg) (TupleN.get4 initPkg)
    Nothing                    -> TupleN.tuple4 "" "" Nothing Nothing
  where
    filteredPkgArr pkg arr = Arr.filter (\x -> x == pkg) arr

packageContained :: String -> T.PackageName -> Boolean
packageContained str pkgName = Str.contains (Str.Pattern str) pkgName

decodeURI :: String -> String
decodeURI uri =
  G.decodeURIComponent $
    Rgx.replace (unsafePartial fromRight $ Rgx.regex "\\+" RXF.global) " " uri
