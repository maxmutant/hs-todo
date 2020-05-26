module Main where

import Data.Maybe
import System.Directory
import System.Environment
import System.Exit
-- import Text.Regex.PCRE

import qualified TodoUI as UI

main :: IO()
main = do
    -- todoFilePath <- parseArgs getArgs
    let todoFilePath = "/home/max/programming/haskell/hs-todo/res/example.txt"
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
parseTodoItem todoStr = UI.TodoItem { UI._desc = parseDesc todoWords
                                    , UI._project = parseStartsWith todoWords '+'
                                    , UI._context = parseStartsWith todoWords '@'
                                    }
                        where todoWords = words todoStr

parseDesc :: [String] -> String
parseDesc = head

parseStartsWith :: [String] -> Char -> Maybe String
parseStartsWith todoWords pre = listToMaybe $ filter (hasPrefix pre) todoWords

hasPrefix ::  Char -> String -> Bool
hasPrefix _ [] = False
hasPrefix pre (x:_) = x == pre
