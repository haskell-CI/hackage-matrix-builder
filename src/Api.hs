module Api (api) where

import           Rest.Api

import qualified Api.Package             as Package
import qualified Api.Package.Report      as Package.Report
import qualified Api.Package.Report.Cell as Package.Report.Cell
import qualified Api.Queue               as Queue
import           Api.Root                (Root)
import qualified Api.Tag                 as Tag
import qualified Api.User                as User

api :: Api Root
api = Versioned [(mkVersion 1 0 0, Some1 router)]
  where
    router :: Router Root Root
    router =
      root -/ route queue
           -/ route package --/ route report ---/ route cell
           -/ route tag
           -/ route user
    package = Package.resource
    queue   = Queue.resource
    report  = Package.Report.resource
    tag     = Tag.resource
    user    = User.resource
    cell    = Package.Report.Cell.resource
