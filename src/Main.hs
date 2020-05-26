module Main where

import Data.Maybe
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
           contents <- readFile path
           return . map parseTodoItem $ lines contents
       else errorWithoutStackTrace "File does not exist! Exiting..."

parseTodoItem :: String -> UI.TodoItem
parseTodoItem todoStr = UI.TodoItem { UI._desc = parseDesc todoStr
                                    , UI._project = parseStartsWith todoStr '+'
                                    , UI._context = parseStartsWith todoStr '@'
                                    }

parseDesc :: String -> String
parseDesc todoStr = todoStr

parseStartsWith :: String -> Char -> Maybe String
parseStartsWith todoStr pre = listToMaybe . filter (hasPrefix pre) $ words todoStr

hasPrefix ::  Char -> String -> Bool
hasPrefix _ [] = False
hasPrefix pre (x:_) = x == pre
