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
module MusicBrainz.Database (createDB, insertArtist) where

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
        run conn "CREATE TABLE tags (id TEXT NOT NULL, name TEXT NOT NULL, count INTEGER NOT NULL, FOREIGN KEY (id) REFERENCES artists(id))" []
        --run conn "CREATE TABLE release (art_id TEXT NOT NULL, alb_id TEXT NOT NULL, FOREIGN KEY (art_id) REFERENCES artists(id), FOREIGN KEY (alb_id) REFERENCES albums(id))" []
        run conn "CREATE TABLE releases (art_id TEXT NOT NULL, alb_id TEXT NOT NULL, FOREIGN KEY (art_id) REFERENCES artists(id))" []
        commit conn
        disconnect conn

-- |Insert an artist to the Database File
-- TODO: also add releases (probably should receive a list of releases)
insertArtist :: String -> Artist -> IO ()
insertArtist dbFile (Artist id name rels tags) = do
        -- TODO check if dbFile has necessary tables
        conn <- connectSqlite3 dbFile

        -- TODO check if artist exists in DB before inserting

        -- Insert artist basic info
        artStmt <- prepare conn "INSERT INTO artists (id, name) VALUES (?,?)"
        execute artStmt [ toSql id, toSql name ]

        -- TODO Insert releases info

        -- Insert artist releases
        relStmt <- prepare conn "INSERT INTO releases (art_id, alb_id) VALUES (?,?)"
        executeMany relStmt . map relArg $ rels

        -- Insert artist tags
        tagStmt <- prepare conn "INSERT INTO tags (id, name, count) VALUES (?,?,?)"
        executeMany tagStmt . map (prepareTagArg id) $ tags

        -- TODO Insert releases tags

        commit conn
        disconnect conn
                where
                        relArg rID = [ toSql id, toSql rID ]

-- |Prepare a Tag to SqlValues used on the tags table using the given id as
-- reference
prepareTagArg :: String -> Tag -> [SqlValues]
prepareTagArg id (t,c) = [ toSql id, toSql t, toSql c ]
