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
module MusicBrainz.Database where
-- TODO Export only externaly used declarations

import Database.HDBC
import Database.HDBC.Sqlite3

-- |Initialize the Database file, if not already initialized.
createDB :: String -> IO ()
createDB dbFile = do
        conn <- connectSqlite3 dbFile
        -- TODO create appropriate tables here
        run conn "CREATE TABLE foo (bar TEXT)" []
        commit conn
