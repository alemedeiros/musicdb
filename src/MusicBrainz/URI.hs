-- URI.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Musicbrainz interface module for musicdb
--
-- URI submodule, responsible for building the URI for the requests

-- |Contains functions for formatting the URIs for the requests
module MusicBrainz.URI where

import Network.HTTP
import Network.URI

-- |Build the URI for a seach with the given artist name
--
-- The format of the artist search is:
-- http://musicbrainz.org/ws/2/artist/?query=artist:<artist>&limit=1
uriSearchArtist :: String -> Int -> URI
uriSearchArtist art lim = URI 
        { uriScheme = "http:"
        , uriAuthority = Just $ URIAuth "" "musicbrainz.org" ""
        , uriPath = "/ws/2/artist/"
        , uriQuery = "?query=artist:" ++ urlEncode art ++ "&limit=" ++ show lim
        , uriFragment = ""
        }
