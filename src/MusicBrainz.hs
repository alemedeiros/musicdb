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

import MusicBrainz.URI

import Network.HTTP
import Network.URI

type URL = String

-- |Get the information XML for the artist with the ID specified on the string.
getArtistInfo :: String -> IO String
getArtistInfo id = undefined

-- |Search for the artist data by the artist name, limiting the number of
-- artists in the result
searchArtist :: String -> Int -> IO String
searchArtist art lim = urlDownload $ uriSearchArtist art lim

-- |Download a given uri and return its content as a String
urlDownload :: URI -> IO String
urlDownload uri = do 
        resp <- simpleHTTP request
        case resp of
                Left x -> return $ "Error connecting: " ++ show x
                Right r -> case rspCode r of
                        (2,_,_) -> return $ rspBody r
                        _ -> return $ show r
                where
                        request = Request
                                { rqURI = uri
                                , rqMethod = GET
                                , rqHeaders = []
                                , rqBody = ""
                                }