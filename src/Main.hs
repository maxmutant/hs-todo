module Main where

import System.Environment
import System.Exit

import qualified TodoUI     as TU
import qualified TodoIO     as IO

main :: IO()
main = do
    todoFilePath <- IO.parseArgs getArgs
    -- let todoFilePath = "./res/example.txt"
    todoItems <- IO.readTodoFile todoFilePath
    outputItems <- TU.runMain todoItems
    IO.writeTodoFile todoFilePath outputItems
    exitSuccess
