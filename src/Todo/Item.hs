-- --------------------------------------------------------------------------
-- |
-- Module      :  Todo.Item
-- Copyright   :  (c) Maximilian Mayer 2020
-- License     :  MIT (see LICENSE)
--
-- Maintainer  :  max@maxmayer.xyz
-- Stability   :
-- Portability :
--
-- The representation of a single entry within the todo.txt.
--
-----------------------------------------------------------------------------

module Todo.Item
( TodoItem
, getId, setId
, compareByText
, toggleDone
, parseTodoItem
, printIndented
, SortFunc
, sortFull, sortId, sortDone, sortDate, sortPrio, sortProj, sortCont
) where

import Todo.FixedStrings

import Data.List.Extra (merge, trim, (\\))
import Data.Ord (comparing)
import Text.Regex.TDFA (getAllTextMatches, (=~))

import qualified Data.Vector                  as Vec
import qualified Data.Vector.Algorithms.Intro as Vec

-- | TodoItem, the parsed representation of a todo.txt line.
data TodoItem = TodoItem
  { _id       :: Integer  -- ^ Unique identifier of each item
  , _full     :: String   -- ^ The full todo item string (1 line in todo.txt)
  , _done     :: Bool     -- ^ Whether item is done "x "
  , _priority :: String   -- ^ Priority of item ([A-Z])
  , _dates    :: String   -- ^ Completion and creation dates in form YYYY-MM-DD
  , _desc     :: String   -- ^ Description of what should be done
  , _project  :: [String] -- ^ Projects of item, starting with '+'
  , _context  :: [String] -- ^ Contexts of item, starting with '@'
  , _keyval   :: [String] -- ^ Optional key-value pairs in form k:v
  }

instance Show TodoItem where
    show = _full

instance Eq TodoItem where
    x == y = _id x == _id y

getId :: TodoItem -> Integer
getId = _id

setId :: TodoItem -> Integer -> TodoItem
setId item newId = item { _id = newId }

compareByText :: TodoItem -> TodoItem -> Bool
compareByText x y = _full x == _full y

-- | Toggle the done flag of a TodoItem and update the 'full'
-- string to match the new state.
toggleDone :: TodoItem -> TodoItem
toggleDone i = i { _full = if isDone
                                  then drop 2 $ _full i
                                  else 'x':' ': _full i
                 , _done = not isDone
                 }
               where isDone = _done i

-- --------------------------------------------------------------------------
-- Parsing operations

-- | Parse a TodoItem from a given string. If the passed string
-- is not in valid todo.txt format, an error message is returned.
--
-- Note: The id of the TodoItem is not set here. This must be
-- done by the caller.
parseTodoItem :: String -> Either String TodoItem
parseTodoItem todoStr = do
    let doneResult = parseStart todoStr regDone
        doneNext   = getNext doneResult
        prioResult = parseStart doneNext regPrio
        prioNext   = getNext prioResult
        dateResult = parseStart prioNext regDate
        dateNext   = getNext dateResult
        projects   = parseAll dateNext regProj
        contexts   = parseAll dateNext regCont
        keyvals    = parseAll dateNext regKeyVal
        varying    = merge projects $ merge contexts keyvals
        desc       = parseDesc dateNext varying
    if null desc
       then Left txtErrFormat
       else Right TodoItem { _id       = 0
                           , _full     = todoStr
                           , _done     = matched' doneResult /= ""
                           , _priority = matched' prioResult
                           , _dates    = matched' dateResult
                           , _desc     = desc
                           , _project  = projects
                           , _context  = contexts
                           , _keyval   = keyvals
                           }

parse :: String -> String -> (String, String, String)
parse str regex = str =~ regex

parseStart :: String -> String -> (String, String, String)
parseStart str regex = parse str ('^' : regex)

parseAll :: String -> String -> [String]
parseAll str regex = map trim $ getAllTextMatches (str =~ regex)

parseDesc :: String -> [String] -> String
parseDesc str varying = unwords $ (\\) (words str) varying

getNext :: (String, String, String) -> String
getNext (x,_,"") = x
getNext ("",_,z) = z
getNext _        = ""

matched' :: (String, String, String) -> String
matched' (_,y,_) = y

-- | Various regular expressions for parsing the individual parts
-- of a TodoItem.
regDone, regPrio, regDate, regProj, regCont, regKeyVal :: String
regDone   = "(x )"
regPrio   = "(\\([A-Z]\\) )"
regDate   = "([0-9]{4}-[0-9]{2}-[0-9]{2} ){1,2}"
regProj   = "(^\\+[^ ]+ | \\+[^ ]+ | \\+[^ ]+$)"
regCont   = "(^@[^ ]+ | @[^ ]+ | @[^ ]+$)"
regKeyVal = "(^[^ :]+:[^ :]+ | [^ :]+:[^ :]+ | [^ :]+:[^ :]+$)"

-- --------------------------------------------------------------------------
-- Printing

-- | Print a given TodoItem in an "indented" format. In this
-- format, items which aren't done or have no priority are
-- indented with spaces to align with others.
printIndented :: TodoItem -> String
printIndented item =
    " " ++ spacingDone
        ++ spacingPrio
        ++ _full item
        where
            spacingDone = if _done item then "" else "  "
            spacingPrio = if null $ _priority item then "    " else ""

-- --------------------------------------------------------------------------
-- Sorting

type SortFunc = (Vec.Vector TodoItem -> Vec.Vector TodoItem)

-- | Various functions to sort multiple TodoItems based on
-- their individual parts.
sortFull, sortId, sortDone, sortDate, sortPrio, sortProj, sortCont :: SortFunc
sortFull = sortBy _full
sortId   = sortBy _id
sortDone = sortBy _done
sortDate = sortBy _dates
sortPrio = sortBy _priority
sortProj = sortBy _project
sortCont = sortBy _context

sortBy
    :: (Ord a)
    => (TodoItem -> a)
    -> Vec.Vector TodoItem
    -> Vec.Vector TodoItem
sortBy f = Vec.modify $ Vec.sortBy $ comparing f
