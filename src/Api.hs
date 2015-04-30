module Api (api) where

import           Rest.Api

import qualified Api.Package        as Package
import qualified Api.Package.Report as Package.Report
import qualified Api.Queue          as Queue
import           Api.Root           (Root)

api :: Api Root
api = [(mkVersion 1 0 0, Some1 router)]
  where
    router :: Router Root Root
    router =
      root -/ route package --/ route report
           -/ route queue
    package = Package.resource
    report  = Package.Report.resource
    queue   = Queue.resource
