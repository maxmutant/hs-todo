module Main where

import System.Directory
import System.Environment
import System.Exit

import qualified TodoUI     as TU
import qualified TodoItem   as TI

main :: IO()
main = do
    todoFilePath <- parseArgs getArgs
    -- let todoFilePath = "./res/example.txt"
    todoItems <- readTodoFile todoFilePath
    TU.runMain todoItems
    exitSuccess

parseArgs :: IO [String] -> IO String
parseArgs args = do
    input <- args
    if null input
       then errorWithoutStackTrace "No todo file path given! Exiting..."
       else return $ head input

readTodoFile :: String -> IO [TI.TodoItem]
readTodoFile path = do
    fileExists <- doesFileExist path
    if fileExists
       then do
           contents <- readFile path
           return . map TI.parseTodoItem $ lines contents
       else errorWithoutStackTrace "File does not exist! Exiting..."
