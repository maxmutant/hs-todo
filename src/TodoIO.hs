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
           let parsedItems = map TI.parseTodoItem $ lines contents
           return . map validate $ zip [1..] parsedItems
       else errorWithoutStackTrace "File does not exist! Exiting..."

validate :: (Integer, Either String TI.TodoItem) -> TI.TodoItem
validate (line, item) = case item of
                         Right ti -> ti
                         Left err ->
                             let msg = ("Line " <> show line <> " - "<> err) in
                             errorWithoutStackTrace msg

writeTodoFile :: String -> [TI.TodoItem] -> IO ()
writeTodoFile fileName outputItems =
    bracketOnError (openTempFile "." "temp")
    (\(tempName, tempHandle) -> do
        hClose tempHandle
        removeFile tempName)
    (\(tempName, tempHandle) -> do
        hPutStr tempHandle . unlines $ map show outputItems
        hClose tempHandle
        removeFile fileName
        renameFile tempName fileName)
