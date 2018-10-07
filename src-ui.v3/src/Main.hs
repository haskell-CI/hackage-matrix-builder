{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Reflex.Dom

main :: IO ()
main = mainWidget $ display =<< count =<< button "ClickMe"
