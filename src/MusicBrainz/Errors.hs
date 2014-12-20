-- Errors.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Errors definition submodule

-- |Contains error handling definitions and useful error printing funcions
module MusicBrainz.Errors where

import System.IO

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
printHelp = putStrLn "Usage: MusicDB command [options]\n\
        \\n\
        \init                       Initialize localdatabase\n\
        \add artist                 Download the artist information with it's releases and add to local database\n\
        \search artist              Search MusicBrainz database for the given artist and print the top results\n\
        \genplaylist artist album   Generate a playlist of similar (local) albums, starting with the given album\n\
        \usage | help               Print this help message\n"
        -- TODO add help information about options
