-- Errors.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Errors definition submodule

-- |Contains error handling definitions and useful error printing funcions
module MusicBrainz.Errors where

{- Error handling -}

-- TODO

{- Useful printing functions -}

-- |Print error messages, prepended by the program name (musicdb) without
-- crashing.
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
