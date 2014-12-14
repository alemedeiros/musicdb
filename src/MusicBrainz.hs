-- Musicbrainz.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Musicbrainz interface module for musicdb

-- |Musicbrainz module is responsible for the interaction between the project
-- with the musicbrainz database.
--
-- It is responsible for the download and parsing of the informations related to
-- the releases, artists and songs.
module MusicBrainz (getArtistInfo, searchArtist) where

import Data.Char
import Data.List
import Data.Maybe

import MusicBrainz.Database
import MusicBrainz.Parser
import MusicBrainz.Types
import MusicBrainz.URI

import Network.HTTP
import Network.URI

type URL = String

-- |Get the information XML for the artist with the ID specified on the string.
-- TODO Add dbFile option
getArtistInfo :: String -> Int -> IO String
getArtistInfo art lim = do
        srchList <- searchArtist art lim
        let
            res = find (\(_,n) -> (==) (map toLower art) (map toLower n)) srchList
        case res of
                Nothing -> let
                               searchResult = foldl (++) "" $ map (\(i,n) -> i ++ "\t" ++ n ++ "\n") srchList
                           in
                              return $ "Artist not found\n\n" ++ searchResult
                Just (id,_) -> do
                        artLookup <- uriDownload $ uriLookupArtist id
                        let
                            artData = getArtistLookupResult artLookup
                        case artData of
                                Nothing -> return $ "Artist found but could not parse xml\n\n" ++ artLookup
                                Just a -> do
                                        relLookup <- mapM (uriDownload . uriLookupRelGroup) $ artRelGroupIDList a
                                        let
                                            rels = mapMaybe getRelGLookupResult relLookup
                                        insertArtist "music.db" (a, rels)
                                        return $ "Artist found\n\n" ++ show a ++ "\n\nReleases:\n" ++ foldl (\l e -> l ++ show e ++ "\n\n") "" rels

-- |Search for the artist data by the artist name, limiting the number of
-- artists in the result
--
-- Returns a list of pairs (id, name)
searchArtist :: String -> Int -> IO [(String, String)]
searchArtist art lim = do
        srch <- uriDownload $ uriSearchArtist art lim
        return $ getSearchResult srch

-- |Download a given uri and return its content as a String
uriDownload :: URI -> IO String
uriDownload uri = do
        resp <- simpleHTTP request
        case resp of
                Left x -> return $ "Error connecting: " ++ show x
                Right r -> case rspCode r of
                        (2,_,_) -> return $ rspBody r
                        -- TODO check other return codes
                        _ -> return $ show r
                where
                        request = Request
                                { rqURI = uri
                                , rqMethod = GET
                                , rqHeaders = []
                                , rqBody = ""
                                }
