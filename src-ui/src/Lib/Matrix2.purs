module Lib.MatrixApi where

import Prelude (Unit, pure, bind, otherwise, (<>), ($), (<<<), (<$>), show, (==))
import Network.HTTP.Affjax as Affjax
import Network.HTTP.StatusCode as SC
import Control.Monad.Aff.Class (class MonadAff, liftAff)
import Data.Traversable as TRV
import Data.Maybe (Maybe(..))
import Data.Argonaut.Core (JObject, Json, fromNumber, fromObject, jsonEmptyString, toArray, toNumber, toObject, toString, foldJsonObject) as Arg
import Data.Argonaut.Decode (decodeJson) as Arg
import Data.Either (Either(..))
import Data.HTTP.Method (Method(..))
import Data.StrMap as SM
import Network.RemoteData as RD
import Control.Monad.Reader (class MonadReader, ReaderT)
import Control.Monad.Eff.Ref (Ref)
import Control.Monad.Eff.Exception as E
import Control.Monad.Eff.AVar (AVAR)
import Control.Monad.Aff (Aff)
import Control.Apply (lift2)
import Data.Int as Int
import Data.Array as Arr
import Data.Tuple as Tuple
import Data.String as Str
import Halogen.Aff (HalogenEffects)
import DOM (DOM)
import DOM.HTML.Types as DOM
import Debug.Trace
import Unsafe.Coerce
import Halogen.Aff (HalogenEffects)

import Lib.Types as T
import Lib.MatrixParser as MP

type Environment =
  {
    packages :: Ref (RD.RemoteData E.Error (Array T.PackageName))
  }

type MatrixE e =
  HalogenEffects (ajax :: Affjax.AJAX, window :: DOM.WINDOW, dom :: DOM, history :: DOM.HISTORY, avar :: AVAR | e)

type Matrix e = ReaderT Environment (Aff (MatrixE e))
type MatrixEffects = HalogenEffects (ajax :: Affjax.AJAX, window :: DOM.WINDOW, dom :: DOM, history :: DOM.HISTORY, avar :: AVAR)
-- /v2/idxstates with parameter min & max
getIdxstate :: forall e m. MonadAff (MatrixE e) m
            => m (RD.RemoteData E.Error (Array T.PkgIdxTs))
getIdxstate = do
  res <- liftAff (Affjax.affjax Affjax.defaultRequest {
                                   url = "/api/v2/idxstates?min=0&max=2147483647"
                                 , method = Left GET
                                 })
  case res.status of
    SC.StatusCode 200 -> do
      let
        decodedApi = Arg.decodeJson res.response
      case decodedApi of
        Right a -> pure (RD.Success a)
        Left e  -> pure (RD.Failure (E.error "decoded failed"))
    SC.StatusCode _ -> pure (RD.Failure (E.error "Report Not Found"))

-- /v2/idxstates/latest
getLatestIdxstate :: forall e m. MonadAff (MatrixE e) m
                  => m (RD.RemoteData E.Error T.PkgIdxTs)
getLatestIdxstate = do
  res <- liftAff (Affjax.affjax Affjax.defaultRequest {
                                   url = "/api/v2/idxstates/latest"
                                 , method = Left GET
                                 })
  case res.status of
    SC.StatusCode 200 -> do
      let
        decodedApi = Arg.decodeJson (res.response :: Arg.Json)
      case decodedApi of
        (Right a) ->
          case Arg.toNumber a of
            Just idx -> pure (RD.Success (Int.round idx))
            Nothing  -> pure (RD.Failure (E.error "Index-state is not Number"))
        (Left e)  -> pure (RD.Failure (E.error e))
    SC.StatusCode _ -> pure (RD.Failure (E.error "Index-state Not Found"))


-- TODO: This function signature MUST be changed after API v2 fully implemented
-- /v2/packages
getPackages :: forall e m. MonadReader Environment m
            => MonadAff (MatrixE e) m
            => m (RD.RemoteData E.Error (Array T.PackageName))
getPackages = do
  res <- liftAff (Affjax.affjax Affjax.defaultRequest
                        {
                          url = "/api/v2/packages"
                          , method = Left GET
                        })
  case res.status of
    SC.StatusCode 200 -> do
      let
        decodedApi = Arg.decodeJson (res.response)
      case decodedApi of
        Right a -> pure $ RD.Success a
        Left e  -> pure $ RD.Failure (E.error "decoding failed")
    SC.StatusCode 304 -> do
      let
        decodedApi = Arg.decodeJson (res.response)
      case decodedApi of
        Right a -> pure $ RD.Success a
        Left e  -> pure $ RD.Failure (E.error "decoding failed")
    SC.StatusCode _ -> pure (RD.Failure (E.error "Report Not Found"))

-- /v2/packages/{pkgname}/tags
getPackageTags :: forall e m. MonadAff (MatrixE e) m
               => T.PackageName
               -> m (RD.RemoteData E.Error (Array T.TagName))
getPackageTags pkgName = do
  if Str.null pkgName
    then pure (RD.Failure (E.error "Package is Empty"))
    else do
      res <- liftAff (Affjax.affjax Affjax.defaultRequest {
                                      url = "/api/v2/packages/" <> pkgName <> "/tags"
                                    , method = Left GET
                                    })
      case res.status of
        (SC.StatusCode 200)  -> do
          let
            decodedApi = Arg.decodeJson (res.response :: Arg.Json)
          case decodedApi of
            Right a -> pure (RD.Success a)
            Left e  -> pure (RD.Failure (E.error "decoded failed"))
        SC.StatusCode _ -> pure (RD.Failure (E.error "Report Not Found"))

-- /v2/packages/*/reports/latest
getPackagesIdxstate ::  forall e m. MonadAff (MatrixE e) m
                    => m (RD.RemoteData E.Error (SM.StrMap T.PkgIdxTs))
getPackagesIdxstate = do
  res <- liftAff (Affjax.affjax Affjax.defaultRequest
                        {
                          url = "/api/v2/packages/*/reports/latest"
                          , method = Left GET
                        })
  case res.status of
    SC.StatusCode 200 -> do
      let
        decodedApi = Arg.foldJsonObject SM.empty unsafeCoerce res.response --Arg.decodeJson (res.response)
      if SM.isEmpty decodedApi then pure $ RD.Failure (E.error "decoding failed") else pure $ RD.Success decodedApi
    SC.StatusCode 304 -> do
      let
        decodedApi = Arg.decodeJson (res.response)
      case decodedApi of
        Right a -> pure $ RD.Success a
        Left e  -> pure $ RD.Failure (E.error "decoding failed")
    SC.StatusCode _ -> pure (RD.Failure (E.error "Report Not Found"))

-- /v2/packages/{pkgname}/reports
getPackageReports :: forall e m. MonadAff (MatrixE e) m
                  => T.PackageName
                  -> m (RD.RemoteData E.Error (Array T.PkgIdxTs))
getPackageReports pkgName =
 if Str.null pkgName
  then pure (RD.Failure (E.error "Package is Empty"))
  else do
    res <- liftAff (Affjax.affjax Affjax.defaultRequest {
                                   url = "/api/v2/packages/" <> pkgName <> "/reports"
                                 , method = Left GET
                                 })
    case res.status of
         SC.StatusCode 200 -> do
           let
             decodedApi = Arg.decodeJson (res.response :: Arg.Json)
           case decodedApi of
             Right a -> pure (RD.Success a)
             Left e  -> pure (RD.Failure (E.error "decoded failed"))
         SC.StatusCode _ -> pure (RD.Failure (E.error "Report Not Found"))

-- /v2/packages/{pkgname}/reports/{idxstate}
getPackageIdxTsReports :: forall e m. MonadAff (MatrixE e) m
                       => T.PackageName
                       -> T.PkgIdxTs
                       -> m (RD.RemoteData E.Error T.PackageIdxTsReports)
getPackageIdxTsReports pkgName idx =
 if Str.null pkgName
  then pure (RD.Failure (E.error "Package is Empty"))
  else do
    res <- liftAff (Affjax.affjax Affjax.defaultRequest {
                                   url = "/api/v2/packages/" <> pkgName <> "/reports/" <> (show idx)
                                 , method = Left GET
                                 })
    case res.status of
         SC.StatusCode 200 -> do
           let
             decodedApi = Arg.decodeJson (res.response :: Arg.Json)
           MP.toPackageIdxTsReports decodedApi
         SC.StatusCode _ -> pure (RD.Failure (E.error "Report Not Found"))

-- /v2/packages/{pkgname}/reports/{idxstate}/{pkgver}/{hcver}
getCellReportDetail :: forall e m. MonadAff (MatrixE e) m
                    => T.PackageName
                    -> T.PkgIdxTs
                    -> T.VersionName
                    -> T.HCVer
                    -> m (RD.RemoteData E.Error T.CellReportDetail)
getCellReportDetail pkgName idx verName ghcVer =
 if Str.null pkgName
  then pure (RD.Failure (E.error "Package is Empty"))
  else do
    res <- liftAff (Affjax.affjax Affjax.defaultRequest {
                                   url = "/api/v2/packages/" <> pkgName <> "/reports/" <> (show idx) <> "/" <> verName <> "/" <> ghcVer
                                 , method = Left GET
                                 })
    case res.status of
         SC.StatusCode 200 -> do
           let
             decodedApi = Arg.decodeJson (res.response :: Arg.Json)
           MP.toCellReportDetail decodedApi
         SC.StatusCode _ -> pure (RD.Failure (E.error "Report Not Found"))

-- /v2/packages/{pkgname}/history
getPackageHistories :: forall e m. MonadAff (MatrixE e) m
             => T.PackageName
             -> m (RD.RemoteData E.Error T.PackageHistories)
getPackageHistories pkgName =
 if Str.null pkgName
  then pure (RD.Failure (E.error "Package is Empty"))
  else do
    res <- liftAff (Affjax.affjax Affjax.defaultRequest {
                                   url = "/api/v2/packages/" <> pkgName <> "/history"
                                 , method = Left GET
                                 })
    case res.status of
         SC.StatusCode 200 -> do
           let
             decodedApi = Arg.decodeJson (res.response :: Arg.Json)
           MP.toPackageHistories decodedApi
         SC.StatusCode _ -> pure (RD.Failure (E.error "Report Not Found"))

getTagsInfo :: forall e m. MonadAff (MatrixE e) m
             => T.PackageName
             -> m (RD.RemoteData E.Error Arg.Json)
getTagsInfo pkgName = do
  res <- liftAff (Affjax.affjax Affjax.defaultRequest {
                                   url = "/api/v2/tags/?pkgnames"
                                 , method = Left GET
                                 })
  let
    decodedApi = Arg.decodeJson (res.response :: Arg.Json)
  case decodedApi of
    (Right a) -> pure (RD.Success a)
    (Left e)  -> pure (RD.Failure (E.error e))

-- /v2/units/{unitid}
getUnitIdInfo :: forall e m. MonadAff (MatrixE e) m
              => T.UUID
              -> m (RD.RemoteData E.Error T.UnitIdInfo)
getUnitIdInfo uuid =
 if Str.null uuid
  then pure (RD.Failure (E.error "Unit id is Empty"))
  else do
    res <- liftAff (Affjax.affjax Affjax.defaultRequest {
                                   url = "/api/v2/units/" <> uuid
                                 , method = Left GET
                                 })
    case res.status of
         SC.StatusCode 200 -> do
           let
             decodedApi = Arg.decodeJson (res.response :: Arg.Json)
           MP.toUnitIdInfo decodedApi
         SC.StatusCode _ -> pure (RD.Failure (E.error "Report Not Found"))

-- /v2/units/{unitid}/deps
getUnitIdInfoDeps :: forall e m. MonadAff (MatrixE e) m
              => T.UUID
              -> m (RD.RemoteData E.Error T.UnitIdInfoDeps)
getUnitIdInfoDeps uuid =
 if Str.null uuid
  then pure (RD.Failure (E.error "Unit id is Empty"))
  else do
    res <- liftAff (Affjax.affjax Affjax.defaultRequest {
                                   url = "/api/v2/units/" <> uuid <> "/deps"
                                 , method = Left GET
                                 })
    case res.status of
         SC.StatusCode 200 -> do
           let
             decodedApi = Arg.decodeJson (res.response :: Arg.Json)
           MP.toUnitIdInfoDeps decodedApi
         SC.StatusCode _ -> pure (RD.Failure (E.error "Report Not Found"))

-- /v2/tags with pkgnames=true
getTagsWithPackages :: forall e m. MonadAff (MatrixE e) m
                    => m (RD.RemoteData E.Error T.TagsWithPackages)
getTagsWithPackages = do
  res <- liftAff (Affjax.affjax Affjax.defaultRequest {
                                   url = "/api/v2/tags?pkgnames=true"
                                 , method = Left GET
                                 })
  case res.status of
    SC.StatusCode 200 -> do
      let
        decodedApi = Arg.decodeJson (res.response) -- :: SM.StrMap (Array String))
      case decodedApi of
        Right a -> pure $ RD.Success a
        Left e  -> pure $ RD.Failure (E.error "decoding failed")
    SC.StatusCode _ -> pure (RD.Failure (E.error "Report Not Found"))

-- /v2/tags with pkgnames=false
getTagsWithoutPackage :: forall e m. MonadAff (MatrixE e) m
                      => m (RD.RemoteData E.Error (Array T.TagName))
getTagsWithoutPackage = do
  res <- liftAff (Affjax.affjax Affjax.defaultRequest {
                                   url = "/api/v2/tags?pkgnames=false"
                                 , method = Left GET
                                 })
  case res.status of
    SC.StatusCode 200 -> do
      let
        decodedApi = Arg.decodeJson (res.response :: Arg.Json)
      case decodedApi of
        Right a -> pure (RD.Success a)
        Left e  -> pure (RD.Failure (E.error "decoded failed"))
    SC.StatusCode 304 -> do
      let
        decodedApi = Arg.decodeJson (res.response :: Arg.Json)
      case decodedApi of
        Right a -> pure (RD.Success a)
        Left e  -> pure (RD.Failure (E.error "decoded failed"))
    SC.StatusCode _ -> pure (RD.Failure (E.error "Report Not Found"))

-- /v2/tags/{tagname}
getTagPackages :: forall e m. MonadAff (MatrixE e) m
               => T.TagName
               -> m (RD.RemoteData E.Error (Array T.TagName))
getTagPackages tagName = do
  res <- liftAff (Affjax.affjax Affjax.defaultRequest
                        {
                          url = "/api/v2/tags/" <> tagName
                          , method = Left GET
                        })
  case res.status of
    SC.StatusCode 200 -> do
      let
        decodedApi = Arg.decodeJson (res.response :: Arg.Json)
      case decodedApi of
        Right a -> pure (RD.Success a)
        Left e  -> pure (RD.Failure (E.error "decoded failed"))
    SC.StatusCode _ -> pure (RD.Failure (E.error "Report Not Found"))

-- /v2/tags/{tagname}/{pkgname}
putPackageTag :: forall e m. MonadAff (MatrixE e) m
                 => T.TagName
                 -> T.PackageName
                 -> m (Affjax.AffjaxResponse Unit)
putPackageTag tagName pkgName = do
  let
    url = "/api/v2/tags/" <> tagName <> "/" <> pkgName
    dt = Arg.jsonEmptyString
  liftAff ( Affjax.put_ url dt )

-- /v2/tags/{tagname}/{pkgname}
deletePackageTag :: forall e m. MonadAff (MatrixE e) m
                 => T.TagName
                 -> T.PackageName
                 -> m (Affjax.AffjaxResponse Unit)
deletePackageTag tagName pkgName = do
  let
    url = "/api/v2/tags/" <> tagName <> "/" <> pkgName
  liftAff ( Affjax.delete_ url)

-- /v2/queue
getQueues :: forall e m. MonadAff (MatrixE e) m
        => m (RD.RemoteData E.Error (Array T.PackageQueue))
getQueues = do
  res <- liftAff (Affjax.affjax Affjax.defaultRequest {
                                   url = "/api/v2/queue"
                                 , method = Left GET
                                 })
  case res.status of
    SC.StatusCode 200 -> do
      let
        decodedApi = Arg.decodeJson (res.response :: Arg.Json)
      MP.toPackageQueue decodedApi
    SC.StatusCode _ -> pure (RD.Failure (E.error "Report Not Found"))

-- /v2/queue/{pkgname}
getQueuePackages :: forall e m. MonadAff (MatrixE e) m
               => T.PackageName
               -> m (RD.RemoteData E.Error (Array T.PackageQueue))
getQueuePackages pkgName = do
  res <- liftAff (Affjax.affjax Affjax.defaultRequest
                        {
                          url = "/api/v2/queue/" <> pkgName
                          , method = Left GET
                        })
  case res.status of
    SC.StatusCode 200 -> do
      let
        decodedApi = Arg.decodeJson (res.response :: Arg.Json)
      MP.toPackageQueue decodedApi
    SC.StatusCode _ -> pure (RD.Failure (E.error "Report Not Found"))

-- /v2/queue/{pkgname}/{idxstate}
getSpecificQueue :: forall e m. MonadAff (MatrixE e) m
               => T.PackageName
               -> T.PkgIdxTs
               -> m (RD.RemoteData E.Error T.PackageQueue)
getSpecificQueue pkgName idx = do
  res <- liftAff (Affjax.affjax Affjax.defaultRequest
                        {
                          url = "/api/v2/queue/" <> pkgName <> "/" <> (show idx)
                          , method = Left GET
                        })
  case res.status of
    SC.StatusCode 200 -> do
      let
        decodedApi = Arg.decodeJson (res.response :: Arg.Json)
      MP.toSpecificPackageQueue decodedApi
    SC.StatusCode _ -> pure (RD.Failure (E.error "Report Not Found"))

-- /v2/queue/{pkgname}/{idxstate}
putPackageQueue :: forall e m. MonadAff (MatrixE e) m
                 => T.PackageName
                 -> T.PkgIdxTs
                 -> Int
                 -> m (Affjax.AffjaxResponse Unit)
putPackageQueue pkgName idx prio = do
  let
    url = "/api/v2/queue/" <> pkgName <> "/" <> (show idx)
    dt = Arg.fromObject (SM.singleton "priority"
           ((Arg.fromNumber <<< Int.toNumber) prio))
  liftAff ( Affjax.put_ url dt )

-- /v2/queue/{pkgname}/{idxstate}
deletePackageQueue :: forall e m. MonadAff (MatrixE e) m
                 => T.PackageName
                 -> T.PkgIdxTs
                 -> m (Affjax.AffjaxResponse Unit)
deletePackageQueue pkgName idx = do
  let
    url = "/api/v2/queue/" <> pkgName <> "/" <> (show idx)
  liftAff ( Affjax.delete_ url)

-- /v2/users/name/{username}
getUser ::  forall e m. MonadAff (MatrixE e) m
        => T.Username
        -> m (RD.RemoteData E.Error T.User)
getUser usr = do
  res <- liftAff (Affjax.affjax Affjax.defaultRequest
                        {
                          url = "/api/v2/users/name/" <> usr <> "/"
                          , method = Left GET
                        })
  case res.status of
    SC.StatusCode 200 -> do
      let
        decodedApi = Arg.decodeJson (res.response :: Arg.Json)
      MP.toUser decodedApi
    SC.StatusCode _ -> pure (RD.Failure (E.error "Report Not Found"))


latestIndex :: forall e m. MonadAff (MatrixE e) m
            => T.PackageName
            -> m (Tuple.Tuple T.PackageName String)
latestIndex pkgName = do
  index <- getPackageReports pkgName
  case index of
    RD.Success idx -> getIdx (show  <$> idx) pkgName
    _              -> pure $ Tuple.Tuple "" ""
  where
    getIdx idx' pkg'=
      case Arr.last idx' of
        Just a  -> pure $ Tuple.Tuple pkg' a
        Nothing -> pure $ Tuple.Tuple "" ""

