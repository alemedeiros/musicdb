-- Main.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Main file for musicdb, a MusicBrainz based artist information database
-- application

import MusicDB

import System.Environment
import System.IO

-- |The main function provides TODO different functionalities:
--
--  [@init@] will initialize the database music.db
--
--  [@add artist@] will download the artist information with it's releases and
--  add to the database
main :: IO ()
main = do args <- getArgs
          case args of
                  ("init":_) -> createDB "music.db"
                  ("add":art:_) -> undefined -- TODO
                  -- Help commands
                  ("help":_) -> printHelp
                  ("usage":_) -> printHelp
                  -- Command errors
                  [] -> do
                          printError "missing command"
                          suggestHelp
                  _ -> do
                          printError $ "undifided arguments -- " ++ show args
                          suggestHelp

-- TODO add options parsing support for:
--  - alternative db file

-- |Print error messages, prepended by the program name (musicdb).
printError :: String -> IO ()
printError = hPutStrLn stderr . (++) "musicdb: "

-- |Suggest help command.
suggestHelp :: IO ()
suggestHelp = hPutStrLn stderr "Try 'musicdb help' for more information."

-- |Print help message.
printHelp :: IO ()
printHelp = putStrLn "Usage: MusicDB command [args]\n\
        \\n\
        \init               Initialize the database music.db\n\
        \add artist         \n\
        \usage | help       Print this help message\n"
