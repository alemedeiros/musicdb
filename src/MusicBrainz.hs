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

import Data.Maybe

import MusicBrainz.URI
import MusicBrainz.Parser

import Network.HTTP
import Network.URI

type URL = String

-- |Get the information XML for the artist with the ID specified on the string.
getArtistInfo :: String -> IO String
getArtistInfo art = do
        srch <- uriDownload $ uriSearchArtist art 1
        let
            id = head . fromJust $ idFromSearch srch
        putStrLn srch
        uriDownload $ uriLookupArtist id

-- |Search for the artist data by the artist name, limiting the number of
-- artists in the result
searchArtist :: String -> Int -> IO String
searchArtist art lim = do
        srch <- uriDownload $ uriSearchArtist art lim
        return . show $ idFromSearch srch

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
