module Main where

import System.Directory
import System.Environment
import System.Exit

import qualified TodoUI as UI

main :: IO()
main = do
    todoFilePath <- parseArgs getArgs
    -- let todoFilePath = "/home/max/programming/haskell/hs-todo/res/example.txt"
    todoItems <- readTodoFile todoFilePath
    UI.runMain todoItems
    exitSuccess

parseArgs :: IO [String] -> IO String
parseArgs args = do
    input <- args
    if null input
       then errorWithoutStackTrace "No todo file path given! Exiting..."
       else return $ head input

readTodoFile :: String -> IO [UI.TodoItem]
readTodoFile path = do
    fileExists <- doesFileExist path
    if fileExists
       then do
           todoLines <- lines <$> readFile path
           return $ parseTodoItems todoLines
       else errorWithoutStackTrace "File does not exist! Exiting..."

parseTodoItems :: [String] -> [UI.TodoItem]
parseTodoItems items = [UI.TodoItem "test" (head items)]
