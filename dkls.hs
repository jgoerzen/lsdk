{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

import System.IO
import Database.HDBC
import Database.HDBC.Sqlite3

import System.Environment

main = do
    args <- getArgs
