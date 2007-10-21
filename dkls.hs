{- Copyright (c) 2007 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

import System.IO
import System.Exit
import Database.HDBC
import Database.HDBC.Sqlite3
import qualified Data.Map as Map
import Data.List

import System.Environment

type Tag = (Integer, String)
type TagMap = Map.Map Integer Tag
data Album = Album {dirid :: Integer, url :: String, aDate :: String,
                    aCaption :: String}
     deriving (Eq, Read, Show, Ord)

main = do
    args <- getArgs
    processIt args

processIt [dbpath] = processIt [dbpath, "/"]
processIt (dbpath:albumPaths) = handleSqlError $ do
    dbh <- connectSqlite3 dbpath
    tags <- loadTags dbh 
    albums <- loadAlbums dbh albumPaths
    print albums
    disconnect dbh

processIt _ = do
    putStrLn "Syntax: dkls dbpath.db [album [album...]]"
    exitFailure

loadAlbums :: Connection -> [String] -> IO [Album]
loadAlbums dbh albumPaths = 
    do allAlbumsSV <- quickQuery dbh "SELECT id, url, date, caption FROM Albums" []
       let albumCandidates = map toAlbum allAlbumsSV
       let selectedAlbums = 
             filter (\a -> any (\x -> isPrefixOf x (url a)) albumPaths) albumCandidates
       return $ sortBy sortFunc selectedAlbums
    where toAlbum [svdir, svurl, svdat, svcap] = 
             Album {dirid = fromSql svdir, url = fromSql svurl,
                    aDate = fromSql svdat, aCaption = fromSql svcap}
          toAlbum _ = error $ "Bad params to toAlbum"
          sortFunc a1 a2 = compare (url a1) (url a2)


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
