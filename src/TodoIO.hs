module TodoIO
( parseArgs
, readTodoFile
, writeTodoFile
) where

import System.Directory
import Control.Exception
import System.IO

import qualified TodoItem   as TI

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

writeTodoFile :: String -> [TI.TodoItem] -> IO ()
writeTodoFile fileName outputItems =
    bracketOnError (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName)
    (\(tempName, tempHandle) -> do
        hPutStr tempHandle . unlines $ map TI.reverseForm outputItems
        hClose tempHandle
        removeFile fileName
        renameFile tempName fileName)
