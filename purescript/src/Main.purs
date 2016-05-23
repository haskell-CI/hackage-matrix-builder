module Main where

import Prelude
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Eff.JQuery

-- main :: forall e. Eff (dom :: DOM, console :: CONSOLE | e) Unit
main = do
  ready do
    log "Hello sailor!"
  return unit
