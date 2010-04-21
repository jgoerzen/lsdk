{- Copyright (c) 2007-2008 John Goerzen <jgoerzen@complete.org>
   Please see the COPYRIGHT file -}

import System.IO
import System.Exit
import Database.HDBC hiding (run)
import Database.HDBC.Sqlite3
import Control.Exception(evaluate)
import Control.Monad(unless)
import HSH
import qualified Data.Map as Map
import Data.List

import System.Environment

type Tag = (Integer, String)
type TagMap = Map.Map Integer Tag
data Album = Album {dirid :: Integer, url :: String, aDate :: String,
                    aCaption :: Maybe String}
     deriving (Eq, Read, Show, Ord)

main = do
    args <- getArgs
    processIt args

processIt ("-t":xs) = processItByTag xs
processIt [dbpath] = processIt [dbpath, "/"]
processIt (dbpath:albumPaths) = handleSqlError $ do
    dbh <- connectSqlite3 dbpath
    tags <- loadTags dbh 
    evaluate (Map.size tags)
    albums <- loadAlbums dbh albumPaths
    mapM_ (procAlbum dbh tags) albums
    disconnect dbh

processIt _ = syntaxError

syntaxError = do 
    putStrLn "Syntax: lsdk dbpath.db [album [album...]]"
    putStrLn "Tag view: lsdk -t dbpath.db tag [tag...]"
    exitFailure

processItByTag (dbpath:tagNames) = handleSqlError $ do
   dbh <- connectSqlite3 dbpath
   tags <- loadTags dbh
   evaluate (Map.size tags)
   let ourTags = Map.filter selectTag tags
   albums <- loadAlbums dbh ["/"]

   
   where ourTags = FIXME

processItByTag _ = syntaxError

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

albumHeaderStr album =
    unlines $ 
            [replicate 70 '=',
             "Album:  " ++ url album,
             "",
             "Date:   " ++ aDate album,
             fmtCap "Caption: " (aCaption album)]
    
imagesHeaderStr imagesSV =
    unlines $ ["Images:  " ++ show (length (imagesSV)),
               ""]

procAlbum :: Connection -> TagMap -> Album -> IO ()
procAlbum dbh tags album =
    do putStr $ albumHeaderStr album

       imagesSV <- quickQuery dbh "SELECT id, name, caption, datetime FROM Images WHERE dirid = ?" [toSql (dirid album)]

       putStr $ imagesHeaderStr imagesSV

       mapM_ (procImage dbh tags album) imagesSV

procAlbumTagView :: Connection -> TagMap -> Album -> IO ()
procAlbumTagView 

procImage :: Connection -> TagMap -> Album -> [SqlValue] -> IO ()
procImage dbh tags album [idsv, namesv, captionsv, datetimesv] =
    do putStrLn $ "    " ++ (replicate 65 '-')
       putStrLn $ "    Image:   " ++ (url album) ++ "/" ++ (fromSql namesv)
       putStrLn $ "    Date:    " ++ (fromSql datetimesv)
       itags <- quickQuery dbh "SELECT tagid FROM ImageTags where imageid = ?"
                           [idsv]
       mapM_ (\[t] -> putStrLn $ "    Tag:     " ++ getTagName tags (fromSql t))
             itags
       fmtCap "    Caption: " (fromSql captionsv)
       putStrLn ""

procImage _ _ _ _ = fail "Invalid procImage args"

fmtCap :: String -> Maybe String -> IO ()
fmtCap _ Nothing = return ()
fmtCap label (Just "") = return ()
fmtCap label (Just captxt) = 
    do putStr label
       clines <- run $ echo captxt -|- ("fmt", ["-w", show linewidth])
       putStrLn (head clines)
       putStr ((unlines . map ((replicate (length label) ' ') ++)) (tail clines))

    where linewidth = 70 - (length label)
