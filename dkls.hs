{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

import System.IO
import System.Exit
import Database.HDBC
import Database.HDBC.Sqlite3
import qualified Data.Map as Map

import System.Environment

type Tag = (Integer, String)
type TagMap = Map.Map Integer Tag

main = do
    args <- getArgs
    processIt args

processIt [dbpath] = processIt [dbpath, "/"]
processIt (dbpath:albums) = handleSqlError $ do
    dbh <- connectSqlite3 dbpath
    tags <- loadTags dbh 
    print tags
    putStrLn (getTagName tags 157)
    -- mapM_ (procAlbum dbh) albums
    disconnect dbh

processIt _ = do
    putStrLn "Syntax: dkls dbpath.db [album [album...]]"
    exitFailure

loadTags :: Connection -> IO TagMap
loadTags dbh = do
    result <- quickQuery dbh "SELECT id, pid, name FROM Tags" []
    return $ foldl insertfunc Map.empty result
    where insertfunc oldm [svid, svpid, svname] =
              Map.insert (fromSql svid) (fromSql svpid, fromSql svname)
                        oldm
          insertfunc _ _ = error $ "Bad result"

getTagName :: TagMap -> Integer -> String
getTagName _ 0 = ""
getTagName map id =
    case Map.lookup id map of
         Nothing -> error $ "Couldn't find tag id " ++ show id
         Just (0, name) -> name
         Just (pid, name) -> getTagName map pid ++ "/" ++ name
