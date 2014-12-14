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

-- |Build the URI for a search with the given artist name
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

-- |Build the URI for a lookup request with the given artist id
--
-- Assume the id is valid
--
-- The format of the artist search is:
-- http://musicbrainz.org/ws/2/artist/<id>?inc=release-groups++tags
uriLookupArtist :: String -> URI
uriLookupArtist id = URI
        { uriScheme = "http:"
        , uriAuthority = Just $ URIAuth "" "musicbrainz.org" ""
        , uriPath = "/ws/2/artist/" ++ id
        , uriQuery = "?inc=release-groups+tags"
        , uriFragment = ""
        }

-- |Build the URI for a lookup request with the given release group id
--
-- Assume the id is valid
--
-- The format of the artist search is:
-- http://musicbrainz.org/ws/2/release-group/<id>?inc=releases+recordings+tags
uriLookupRelGroup :: String -> URI
uriLookupRelGroup id = URI
        { uriScheme = "http:"
        , uriAuthority = Just $ URIAuth "" "musicbrainz.org" ""
        , uriPath = "/ws/2/release-group/" ++ id
        , uriQuery = "?inc=tags"
        , uriFragment = ""
        }
