{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

import System.IO
import System.Exit
import Database.HDBC
import Database.HDBC.Sqlite3
import qualified Data.Map as Map

import System.Environment

type Tag = (Integer, String)

main = do
    args <- getArgs
    processIt args

processIt [dbpath] = processIt [dbpath, "/"]
processIt (dbpath:albums) = handleSqlError $ do
    dbh <- connectSqlite3 dbpath
    tags <- loadTags dbh 
    print tags
    -- mapM_ (procAlbum dbh) albums
    disconnect dbh

processIt _ = do
    putStrLn "Syntax: dkls dbpath.db [album [album...]]"
    exitFailure

loadTags :: Connection -> IO (Map.Map Integer Tag)
loadTags dbh = do
    result <- quickQuery dbh "SELECT id, pid, name FROM Tags" []
    return $ foldl insertfunc Map.empty result
    where insertfunc oldm [svid, svpid, svname] =
              Map.insert (fromSql svid) (fromSql svpid, fromSql svname)
                        oldm
          insertfunc _ _ = error $ "Bad result"

