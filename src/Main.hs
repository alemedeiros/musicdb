-- Main.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Main file for musicdb, a MusicBrainz based artist information database
-- application

-- | A small project for artist/music information database using MusicBrainz as
-- a source of information (Individual Project for QMUL's ECS713 - Functional
-- Programming Module).
--
-- The project code can be found on github: https://github.com/alemedeiros/musicdb
module Main where

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe

import MusicBrainz

import System.Environment

-- |The main function provides 5 different functionalities:
--
--  [@init@]
--  Initialize local database
--
--  [@add artist@]
--  Download the artist information with it's releases and add to local database
--
--  [@search artist@]
--  Search MusicBrainz database for the given artist and print the top results
--
--  [@query artist album@]
--  Query the local database for the specified album
--
--  [@recommend artist album@]
--  Generate a recommendation list of similar artists (with the computed score)
--  from the local database, starting with the given album/artist
main :: IO ()
main = do args <- getArgs
          case args of
                  ("init":opt) -> do
                          let
                              optMap = optParse opt
                              dbFile = fromJust $ Map.lookup "dbfile" optMap
                          createDB dbFile
                  ("add":art:opt) -> do
                          let
                              optMap = optParse opt
                              lim = read . fromJust $ Map.lookup "lim" optMap
                              dbFile = fromJust $ Map.lookup "dbfile" optMap
                              byID = read . fromJust $ Map.lookup "id" optMap
                          artInfo <- if byID
                                        then getArtistInfoByID dbFile art
                                        else getArtistInfo dbFile art lim
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
                  ("query":art:alb:opt) -> do
                          let
                              optMap = optParse opt
                              dbFile = fromJust $ Map.lookup "dbfile" optMap
                          info <- queryLocalDB dbFile art alb
                          putStr info
                  ("recommend":art:alb:opt) -> do
                          let
                              optMap = optParse opt
                              dbFile = fromJust $ Map.lookup "dbfile" optMap
                              lim = read . fromJust $ Map.lookup "lim" optMap
                              thr = read . fromJust $ Map.lookup "threshold" optMap
                          recommendations <- recommend dbFile lim thr art alb
                          case recommendations of
                                  [] -> printError "problems computing recommendations"
                                  _ -> putStrLn $ concatMap (\(s,a) -> show s ++ "\t" ++ artName a ++ "\n") recommendations
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
                optParseAux ("-i":opt) = Map.insert "id" "True" $ optParseAux opt
                optParseAux ("--lim":lim:opt) = Map.insert "lim" lim $ optParseAux opt
                optParseAux ("-l":lim:opt) = Map.insert "lim" lim $ optParseAux opt
                optParseAux ("--dbfile":file:opt) = Map.insert "dbfile" file $ optParseAux opt
                optParseAux ("-f":file:opt) = Map.insert "dbfile" file $ optParseAux opt
                optParseAux ("--threshold":file:opt) = Map.insert "threshold" file $ optParseAux opt
                optParseAux ("-t":file:opt) = Map.insert "threshold" file $ optParseAux opt
                optParseAux opt = error $ "couldn't parse arguments -- " ++ show opt

-- |Fill the option map with the 'defaultOption' for options not specified by
-- the user
fillDefaultOpt :: [(String,String)] -> Map String String -> Map String String
fillDefaultOpt [] opt = opt
fillDefaultOpt ((k,v):def) opt
        | Map.notMember k opt = Map.insert k v $ fillDefaultOpt def opt
        | otherwise = fillDefaultOpt def opt

-- |Default options definition
defaultOptions :: [(String, String)]
defaultOptions = [ ("id", "False"), ("lim", "4") , ("dbfile", "music.db"), ("threshold", "100") ]
