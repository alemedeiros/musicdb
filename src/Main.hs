-- Main.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Main file for musicdb, a MusicBrainz based artist information database
-- application

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import MusicBrainz
import MusicBrainz.Database

import System.Environment
import System.IO

-- |The main function provides TODO different functionalities:
--
--  [@init@] will initialize the database music.db
--
--  [@add artist@] will download the artist information with it's releases and
--  add to the database
--
--  [@search artist@] will search MusicBrainz database for the given artist and
--  print the top results
main :: IO ()
main = do args <- getArgs
          case args of
                  ("init":_) -> createDB "music.db"
                  ("add":art:opt) -> do
                          let
                              optMap = optParse opt
                              lim = read . fromJust $ Map.lookup "lim" optMap
                          artInfo <- getArtistInfo art lim
                          putStrLn artInfo
                  ("search":art:opt) -> do
                          let
                              optMap = optParse opt
                              lim = read . fromJust $ Map.lookup "lim" optMap
                          artSearch <- searchArtist art lim
                          -- Check result and print it
                          case artSearch of
                                  [] -> printError "no artist found"
                                  _ -> mapM_ (\(i,n) -> putStrLn (i ++ "\t" ++ n)) artSearch
                  -- Help commands
                  ("help":_) -> printHelp
                  ("usage":_) -> printHelp
                  -- Command errors
                  [] -> do
                          printError "missing command"
                          suggestHelp
                  _ -> do
                          printError $ "undefided arguments -- " ++ show args
                          suggestHelp


{- Command-line parsing -}

-- |Parse options arguments
optParse :: [String] -> Map String String
optParse = fillDefaultOpt defaultOptions . optParseAux
        where
                optParseAux [] = Map.empty
                optParseAux ("--id":opt) = Map.insert "id" "True" $ optParseAux opt
                optParseAux ("--lim":lim:opt) = Map.insert "lim" lim $ optParseAux opt
                optParseAux opt = error $ "couldn't parse arguments -- " ++ show opt

-- |Fill the option map with the default option for options not specified
fillDefaultOpt :: [(String,String)] -> Map String String -> Map String String
fillDefaultOpt [] opt = opt
fillDefaultOpt ((k,v):def) opt
        | Map.notMember k opt = Map.insert k v $ fillDefaultOpt def opt
        | otherwise = fillDefaultOpt def opt

-- | Default options
defaultOptions :: [(String, String)]
defaultOptions = [ ("id", "False"), ("lim", "4") ]


{- Useful printing functions -}

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
