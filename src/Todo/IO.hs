-- --------------------------------------------------------------------------
-- |
-- Module      :  Todo.IO
-- Copyright   :  (c) Maximilian Mayer 2020
-- License     :  MIT (see LICENSE)
--
-- Maintainer  :  max@maxmayer.xyz
-- Stability   :
-- Portability :
--
-- File and command line arguments operations.
--
-----------------------------------------------------------------------------

module Todo.IO
( parseArgs
, readTodoFile
, writeTodoFile
) where

import Todo.FixedStrings
import Todo.Item

import Control.Exception (bracketOnError)
import System.Directory
import System.IO
import Text.Printf (printf)

-- | Parse the todo.txt file path from given program arguments. This
-- evaluates only the first argument (all others are ignored). If
-- no arguments were provided, an error is thrown.
parseArgs :: IO [String] -> IO String
parseArgs args = do
    input <- args
    if null input
       then errorWithoutStackTrace txtErrNoFile
       else return $ head input

-- | Reads and parses todo.txt entries from the given path. If a
-- line within the file could not be parsed, an error is thrown.
readTodoFile :: String -> IO [TodoItem]
readTodoFile path = do
    fileExists <- doesFileExist path
    if not fileExists
       then errorWithoutStackTrace txtErrPath
       else do
           contents <- readFile path
           let parsedItems = map parseTodoItem $ lines contents
           return . map validate $ zip [1..] parsedItems

validate :: (Integer, Either String TodoItem) -> TodoItem
validate (line, item) = case item of
                          Right ti -> setId ti line
                          Left err -> errorWithoutStackTrace msg
                              where msg = printf txtErrLine (show line) err

-- | Writes list of todo.txt entries to the given path. The new
-- items are first written to a temporary file, which is later
-- moved to the given path to avoid file corruption on errors.
writeTodoFile :: String -> [TodoItem] -> IO ()
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
