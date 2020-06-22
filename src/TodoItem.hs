module TodoItem
( TodoItem (..)
, parseTodoItem
, printIndented
, toggleDone
) where

import Data.List.Extra
import Text.Regex.TDFA

data TodoItem = TodoItem
  { _id       :: Integer
  , _original :: String
  , _done     :: Bool
  , _priority :: String
  , _dates    :: String
  , _desc     :: String
  , _project  :: [String]
  , _context  :: [String]
  , _keyval   :: [String]
  }

instance Show TodoItem where
    show = _original

instance Eq TodoItem where
    x == y = _id x == _id y

parseTodoItem :: String -> Either String TodoItem
parseTodoItem todoStr = do
    let doneResult = parseStart todoStr (getRegex "done")
        doneNext   = getNext doneResult
        prioResult = parseStart doneNext (getRegex "priority")
        prioNext   = getNext prioResult
        dateResult = parseStart prioNext (getRegex "date")
        dateNext   = getNext dateResult
        projects   = parseAll dateNext (getRegex "project")
        contexts   = parseAll dateNext (getRegex "context")
        keyvals    = parseAll dateNext (getRegex "keyval")
        varying    = merge projects $ merge contexts keyvals
        desc       = parseDesc dateNext varying
    if null desc
       then Left "Given string is not in valid todo.txt format!"
       else Right TodoItem { _id       = 0
                           , _original = todoStr
                           , _done     = matched' doneResult /= ""
                           , _priority = matched' prioResult
                           , _dates    = matched' dateResult
                           , _desc     = desc
                           , _project  = projects
                           , _context  = contexts
                           , _keyval   = keyvals
                           }

printIndented :: TodoItem -> String
printIndented item = do
        let x = if _done item then "x " else "  "
            prio = if _priority item /= ""
                      then _priority item
                      else "    "
        " " ++ x
            ++ prio
            ++ _dates item
            ++ _desc item
            ++ printList (_project item)
            ++ printList (_context item)
            ++ printList (_keyval item)

printList :: [String] -> String
printList [] = ""
printList [x] = " " ++ x
printList (x:xs) = " " ++ x ++ printList xs


toggleDone :: TodoItem -> TodoItem
toggleDone i = i { _original = if _done i
                                  then drop 2 $ _original i
                                  else 'x':' ': _original i
                 , _done = not $ _done i }

getRegex :: String -> String
getRegex "done" = "(x )"
getRegex "priority" = "(\\([A-Z]\\) )"
getRegex "date" = "([0-9]{4}-[0-9]{2}-[0-9]{2} ){1,2}"
getRegex "project" = "(^\\+[^ ]+ | \\+[^ ]+ | \\+[^ ]+$)"
getRegex "context" = "(^@[^ ]+ | @[^ ]+ | @[^ ]+$)"
getRegex "keyval" = "(^[^ :]+:[^ :]+ | [^ :]+:[^ :]+ | [^ :]+:[^ :]+$)"
getRegex _ = ""

parse :: String -> String -> (String, String, String)
parse str regex = str =~ regex

parseStart :: String -> String -> (String, String, String)
parseStart str regex = parse str ('^' : regex)

getNext :: (String, String, String) -> String
getNext (x,_,"") = x
getNext ("",_,z) = z
getNext t = error "Can't process " ++ show t

parseAll :: String -> String -> [String]
parseAll str regex = map trim $ getAllTextMatches (str =~ regex)

parseDesc :: String -> [String] -> String
parseDesc str varying = unwords $ (\\) (words str) varying

matched' :: (String, String, String) -> String
matched' (_,x,_) = x
