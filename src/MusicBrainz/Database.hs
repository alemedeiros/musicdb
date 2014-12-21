-- Database.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Musicbrainz interface module for musicdb
--
-- Database submodule, responsible for handling local data.

-- |MusicDB module is responsible for the interaction between the application
-- with the local database -- SQLite3.
--
-- It is responsible for initializing, storing and querying the local database
-- file.
module MusicBrainz.Database (
        createDB,
        insertArtist,
        queryArtistByName,
        queryRelG,
) where

import Data.Char (toLower)
import Data.Maybe

import Database.HDBC
import Database.HDBC.Sqlite3

import MusicBrainz.Types

-- |Initialize the Database file, if not already initialized.
createDB :: String -> IO ()
createDB dbFile = do
        -- TODO check if dbFile exists before creating the tables
        conn <- connectSqlite3 dbFile

        -- Create tables
        run conn "CREATE TABLE artists (id TEXT, name TEXT NOT NULL, PRIMARY KEY (id))" []
        run conn "CREATE TABLE art_tags (id TEXT NOT NULL, name TEXT NOT NULL, count INTEGER NOT NULL, FOREIGN KEY (id) REFERENCES artists(id))" []
        run conn "CREATE TABLE releases (id TEXT, title TEXT NOT NULL, type TEXT, PRIMARY KEY (id))" []
        run conn "CREATE TABLE rel_tags (id TEXT NOT NULL, name TEXT NOT NULL, count INTEGER NOT NULL, FOREIGN KEY (id) REFERENCES releases(id))" []
        run conn "CREATE TABLE art_rel (art_id TEXT NOT NULL, rel_id TEXT NOT NULL, FOREIGN KEY (art_id) REFERENCES artists(id), FOREIGN KEY (rel_id) REFERENCES releases(id))" []

        commit conn
        disconnect conn

-- |Insert an artist to the Database File
insertArtist :: String -> (Artist, [ReleaseGroup]) -> IO ()
insertArtist dbFile (Artist id name rels tags, rels') = do
        -- TODO check if dbFile has necessary tables
        conn <- connectSqlite3 dbFile

        -- TODO check if artist exists in DB before inserting

        -- Insert artist basic info
        artStmt <- prepare conn "INSERT INTO artists (id, name) VALUES (?,?)"
        execute artStmt [ toSql id, toSql name ]

        -- Insert releases info
        relgStmt <- prepare conn "INSERT INTO releases (id, title, type) VALUES (?,?,?)"
        executeMany relgStmt $ map prepareRelGArg rels'

        -- Insert artist releases
        atrStmt <- prepare conn "INSERT INTO art_rel (art_id, rel_id) VALUES (?,?)"
        executeMany atrStmt $ map relArg rels

        -- Insert artist tags
        tagStmt <- prepare conn "INSERT INTO art_tags (id, name, count) VALUES (?,?,?)"
        executeMany tagStmt $ map (prepareTagArg id) tags

        -- Insert releases tags
        tagStmt' <- prepare conn "INSERT INTO rel_tags (id, name, count) VALUES (?,?,?)"
        executeMany tagStmt' $ relGTagList rels'

        commit conn
        disconnect conn
                where
                        relArg rID = [ toSql id, toSql rID ]

-- |Prepare a Tag to SqlValues used on the tags table using the given id as
-- reference
prepareTagArg :: String -> Tag -> [SqlValue]
prepareTagArg id (t,c) = [ toSql id, toSql t, toSql c ]

-- |Prepare a ReleaseGroupd to SqlValues used on the releases table using the
-- given id as reference
prepareRelGArg :: ReleaseGroup -> [SqlValue]
prepareRelGArg (ReleaseGroup id title typ _) = [ toSql id, toSql title, toSql typ ]

relGTagList :: [ReleaseGroup] -> [[SqlValue]]
relGTagList = concatMap (\(ReleaseGroup id _ _ t) -> map (prepareTagArg id) t)

-- |Query an Artist by its name
-- 
-- There *may* be more than one artist with the same name (?)
queryArtistByName :: String -> String -> IO [Artist]
queryArtistByName dbFile art = do
        -- TODO check if dbFile has necessary tables
        conn <- connectSqlite3 dbFile
        queryData <- quickQuery' conn "SELECT * FROM artists WHERE LOWER(name) = ?" [toSql $ map toLower art]

        mArts <- completeArtist conn queryData

        return $ catMaybes mArts

-- |Query an Artist by its ID
queryArtistByID :: String -> String -> IO [Artist]
queryArtistByID dbFile aId = do
        conn <- connectSqlite3 dbFile
        queryData <- quickQuery' conn "SELECT * FROM artists WHERE id = ?" [toSql aId]

        mArts <- completeArtist conn queryData

        disconnect conn
        return $ catMaybes mArts

-- |Query a Release Group by its ID
queryRelG :: String -> String -> IO [ReleaseGroup]
queryRelG dbFile rId = do
        conn <- connectSqlite3 dbFile
        queryData <- quickQuery' conn "SELECT * FROM releases WHERE id = ?" [toSql rId]

        mRelgs <- completeRelG conn queryData

        disconnect conn
        return $ catMaybes mRelgs

-- |Complete the artist query by finding the information about the artists
-- spread throughout the tables
completeArtist :: IConnection c => c -> [[SqlValue]] -> IO [Maybe Artist]
completeArtist conn [] = return []
completeArtist conn (x:xs) = case x of
        [sqlId, sqlName] -> do
                sqlRelgs <- quickQuery' conn "SELECT rel_id FROM art_rel WHERE art_id = ?" [ sqlId ]
                sqlTags <-  quickQuery' conn "SELECT name, count FROM art_tags WHERE id = ?" [ sqlId ]

                let
                    id = fromSql sqlId
                    name = fromSql sqlName
                    relgs = concatMap (map fromSql) sqlRelgs
                    tags = map (\[n,c] -> (fromSql n, fromSql c)) sqlTags

                rest <- completeArtist conn xs
                return (Just (Artist id name relgs tags) : rest)

        _ -> do
                rest <- completeArtist conn xs
                return (Nothing : rest)

-- |Complete the release group query by finding the information about the
-- artists spread throughout the tables
completeRelG :: IConnection c => c -> [[SqlValue]] -> IO [Maybe ReleaseGroup]
completeRelG conn [] = return []
completeRelG conn (x:xs) = case x of
        [sqlId, sqlName, sqlType] -> do
                sqlTags <-  quickQuery' conn "SELECT name, count FROM rel_tags WHERE id = ?" [ sqlId ]

                let
                    id = fromSql sqlId
                    name = fromSql sqlName
                    typ = fromSql sqlType
                    tags = map (\[n,c] -> (fromSql n, fromSql c)) sqlTags

                rest <- completeRelG conn xs
                return (Just (ReleaseGroup id name typ tags) : rest)

        _ -> do
                rest <- completeRelG conn xs
                return (Nothing : rest)
