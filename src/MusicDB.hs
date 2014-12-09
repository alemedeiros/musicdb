-- MusicDB.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Database interface module for musicdb

-- |MusicDB module is responsible for the interaction between the application
-- with the local database -- SQLite3.
--
-- It is responsible for initializing, storing and querying the local database
-- file.
module MusicDB where
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
