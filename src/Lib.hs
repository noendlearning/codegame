{-# LANGUAGE OverloadedStrings #-}

module Lib (someFunc) where

import ClassyPrelude
import qualified HTTP.Main as HTTP

someFunc = do
    HTTP.main 3000