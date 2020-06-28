-- --------------------------------------------------------------------------
-- |
-- Module      :  Todo.FixedString
-- Copyright   :  (c) Maximilian Mayer 2020
-- License     :  MIT (see LICENSE)
--
-- Maintainer  :  max@maxmayer.xyz
-- Stability   :  stable
-- Portability :  portable
--
-- Definition of all TUI texts, messages and labels.
--
-----------------------------------------------------------------------------

module Todo.FixedStrings where

-- | Editor labels
txtLNone, txtLAdd, txtLEdit, txtLSearch, txtLExit :: String
txtLNone   = ""
txtLAdd    = "Add"
txtLEdit   = "Edit"
txtLSearch = "Search"
txtLExit   = "There are unsaved changes. Save them? [Yy]es [Nn]o [Cc]ancel"

-- | Messages in editor
txtHeader, txtSave, txtAdd, txtEdit, txtDel :: String
txtHeader   = "hs-todo 0.0.1 - Your tasks (%s of %s)"
txtSave     = "\"%s\" written"
txtAdd      = "Successfully added new item"
txtEdit     = "Successfully updated item"
txtDel      = "Deleted selected item"

-- | Error messages
txtErrNoFile, txtErrPath, txtErrLine, txtErrFormat, txtErrEmpty, txtErrNoSel
    :: String
txtErrNoFile  = "No todo file path given! Exiting..."
txtErrPath    = "File does not exist! Exiting..."
txtErrLine    = "Line %s - %s"
txtErrFormat  = "Given string is not in valid todo.txt format!"
txtErrEmpty   = "Can't use empty input!"
txtErrNoSel   = "No item selected!"
