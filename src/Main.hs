-- --------------------------------------------------------------------------
-- |
-- Module      :  Main
-- Copyright   :  (c) Maximilian Mayer 2020
-- License     :  MIT (see LICENSE)
--
-- Maintainer  :  max@maxmayer.xyz
-- Stability   :
-- Portability :
--
-- hs-todo, a minimalist TUI for the todo.txt format.
--
-----------------------------------------------------------------------------

module Main where

import Todo.IO
import Todo.UI

import System.Environment (getArgs)
import System.Exit (exitSuccess)

main :: IO()
main = do
    todoFilePath <- parseArgs getArgs
    todoItems <- readTodoFile todoFilePath
    runMain todoFilePath todoItems
    exitSuccess
