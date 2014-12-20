-- Main.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Main file for musicdb, a MusicBrainz based artist information database
-- application

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
--  [@query artist album@] /TODO/
--  Query the local database for the specified album
--
--  [@genplaylist artist album@] /TODO/
--  Generate a playlist of similar (local) albums, starting with the given album
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
                          undefined -- TODO: implement
                  ("genplaylist":art:alb:opt) -> do
                          let
                              optMap = optParse opt
                              dbFile = fromJust $ Map.lookup "dbfile" optMap
                          undefined -- TODO: implement
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
defaultOptions = [ ("id", "False"), ("lim", "4") , ("dbfile", "music.db") ]
