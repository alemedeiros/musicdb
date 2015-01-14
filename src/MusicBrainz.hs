-- Musicbrainz.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Musicbrainz interface module for musicdb

-- |Musicbrainz module is responsible for the interaction between the project
-- with the musicbrainz database.
--
-- It is responsible for the download and parsing of the informations related to
-- the releases, artists and songs.
module MusicBrainz (
        getArtistInfo,
        getArtistInfoByID,
        searchArtist,
        queryLocalDB,
        recommend,
        module MusicBrainz.Analysis,
        module MusicBrainz.Database,
        module MusicBrainz.Errors,
        module MusicBrainz.Types,
) where

import Control.Applicative

import Data.Char
import Data.Function (on)
import Data.List
import Data.Maybe

import MusicBrainz.Analysis
import MusicBrainz.Database
import MusicBrainz.Errors
import MusicBrainz.Parser
import MusicBrainz.Types
import MusicBrainz.URI

import Network.HTTP
import Network.URI

import System.Posix.Unistd

type URL = String

-- |Get the information XML for the artist with the artist name specified on the
-- string, searching the top search results for a perfect match with the name
-- given (case insensitive).
getArtistInfo :: String -> String -> Int -> IO String
getArtistInfo dbFile art lim = do
        srchList <- searchArtist art lim
        let
            res = find (\(_,n) -> nonCaseCmp (==) art n) srchList
        case res of
                Just (id,_) -> getArtistInfoByID dbFile id
                Nothing -> let
                               searchResult = foldl (++) "" $ map (\(i,n) -> i ++ "\t" ++ n ++ "\n") srchList
                           in
                              return $ "Artist not found\n\n" ++ searchResult

-- |Get the information XML for the artist with the ID specified on the string.
getArtistInfoByID :: String -> String -> IO String
getArtistInfoByID dbFile id = do
        artLookup <- uriDownload $ uriLookupArtist id
        let
            artData = getArtistLookupResult artLookup
        case artData of
                Nothing -> return $ "could not parse xml\n\n" ++ artLookup
                Just a -> do
                        relLookup <- mapM (uriDownload . uriLookupRelGroup) $ artRelGroupIDList a
                        let
                            rels = mapMaybe getRelGLookupResult relLookup
                        insertArtist dbFile (a, rels)
                        return $ "Artist found\n\n" ++ show a ++ "\n\nReleases:\n" ++ foldl (\l e -> l ++ show e ++ "\n\n") "" rels

-- |Make a query to the local database
queryLocalDB :: String -> String -> String -> IO String
queryLocalDB dbFile art alb = do
        relg <- getLocalRelG dbFile art alb
        case relg of
                Nothing -> return "Album/Artist not found in local database\n"
                Just (a,r) -> return $ show a ++ "\n" ++ show r

-- |Get Release Group from local database
getLocalRelG :: String -> String -> String -> IO (Maybe (Artist, ReleaseGroup))
getLocalRelG dbFile art alb = do
        arts <- queryArtistByName dbFile art
        let
            relgsId = concatMap (\(Artist _ _ r _) -> r) arts
        mRelgs <- mapM (queryRelG dbFile) relgsId
        let
            relgs = concat mRelgs
            relg = find (\(ReleaseGroup _ n _ _) -> nonCaseCmp (==) alb n) relgs
        case relg of
                Nothing -> return Nothing
                Just r -> let
                              Just auth = find (\(Artist _ _ rs _) -> elem (relID r) rs) arts
                        in
                           return (Just (auth, r))

-- |Generate a recommendation list based on the given artist-album pair
recommend :: String -> Int -> Int -> String -> String -> IO [(Int, Artist)]
recommend dbFile lim thr art alb = do
        start <- getLocalRelG dbFile art alb
        let
            tagList = fromMaybe [] $ genStyleTagList <$> start
            analyse = sortBy (flip compare `on` fst) . filter ((<) thr . fst) . map (analyseArtist tagList)
            startArt = fromJust $ fmap fst start

        artists <- getAllArtists dbFile

        return . take lim . analyse $ filter (startArt /=) artists


-- |Search for the artist data by the artist name, limiting the number of
-- artists in the result
--
-- Returns a list of pairs (id, name)
searchArtist :: String -> Int -> IO [(String, String)]
searchArtist art lim = do
        srch <- uriDownload $ uriSearchArtist art lim
        return $ getSearchResult srch

-- |Download a given uri and return its content as a String
-- TODO: return error in a better way (i.e Either or Maybe)
uriDownload :: URI -> IO String
uriDownload uri = do
        resp <- simpleHTTP request
        usleep 500000 -- wait a little while, so server doesn't block us
        case resp of
                Left x -> do
                        printError $ "couldn't download page: " ++ show uri ++ "\nReceived follwing error:\n" ++ show x
                        return ""
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
-- |Simple case insensitive String comparisson
nonCaseCmp :: (String -> String -> Bool) -> String -> String -> Bool
nonCaseCmp f s w = f (map toLower s) (map toLower w)
