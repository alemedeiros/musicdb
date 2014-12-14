-- Parser.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Musicbrainz interface module for musicdb
--
-- XML parser submodule

-- |Contains functions for XML parsing of the MusicBrainz XML data
module MusicBrainz.Parser (getSearchResult, getArtistLookupResult, getRelGLookupResult) where

import Control.Arrow
import Control.Applicative

import Data.List
import Data.Maybe

import MusicBrainz.Types

import Text.XML.HaXml hiding (find)
import Text.XML.HaXml.Posn


{- Exported functions -}

-- |Parse artist ID from MusicBrainz's search XML
getSearchResult :: String -> [(String, String)]
getSearchResult = readInfo . artistListFromSearch . parseContents
        where
                readInfo = mapMaybe (maybePair . (getArtistID &&& getArtistName))

-- |Parse artist informations from MusicBrainz's lookup XML
getArtistLookupResult :: String -> Maybe Artist
getArtistLookupResult = (=<<) readArtistData . artistFromLookup . parseContents
        where
                -- Read artist information from artist xml tag
                readArtistData :: Content Posn -> Maybe Artist
                readArtistData e
                        | isNothing id = Nothing
                        | isNothing name = Nothing
                        | otherwise = Just $ Artist jId jName rels tags
                        where
                                id = getArtistID e
                                name = getArtistName e
                                rels = getArtistRelGroupIDList e
                                tags = getArtistTagList e
                                jId = fromJust id
                                jName = fromJust name

-- |Parse artist informations from MusicBrainz's lookup XML
getRelGLookupResult :: String -> Maybe ReleaseGroup
getRelGLookupResult = (=<<) readRelGData . relgFromLookup . parseContents
        where
                -- Read release-group information from xml tag
                readRelGData :: Content Posn -> Maybe ReleaseGroup
                readRelGData e
                        | isNothing id = Nothing
                        | isNothing title = Nothing
                        | otherwise = Just $ ReleaseGroup jId jTitle tags
                        where
                                id = getRelGID e
                                title = getRelGTitle e
                                tags = getRelGTagList e
                                jId = fromJust id
                                jTitle = fromJust title

{- MusicBrainz XML structure aware functions -}

-- |Get artist list from base search XML
--
-- Return a list of Content, where each content is an artist tag element
artistListFromSearch :: Content Posn -> [Content Posn]
artistListFromSearch = tag "metadata" /> tag "artist-list" /> tag "artist"

-- |Get artist from base lookup XML
--
-- Assume there will be only one artist tag
artistFromLookup :: Content Posn -> Maybe (Content Posn)
artistFromLookup = uniList . (tag "metadata" /> tag "artist")

-- |Get release group from base lookup XML
--
-- Assume there will be only one release-group tag
relgFromLookup :: Content Posn -> Maybe (Content Posn)
relgFromLookup = uniList . (tag "metadata" /> tag "release-group")

-- |Get artist name from artist xml tag
--
-- Artist should have one, and only one, name
getArtistName :: Content Posn -> Maybe String
getArtistName = uniList . names
        where
                names = map verbatim . (tag "artist" /> tag "name" /> txt)

-- |Get artist id from artist xml tag
getArtistID :: Content Posn -> Maybe String
getArtistID = getAttrValByName "id"

-- |Get artist release group id list from artist xml tag
--
-- Artist should have one, and only one, name
-- TODO: filter albums (?)
getArtistRelGroupIDList :: Content Posn -> [String]
getArtistRelGroupIDList = mapMaybe (getAttrValByName "id") . getRelG
        where
                getRelG = tag "artist" /> tag "release-group-list" /> tag "release-group"

-- |Get artist tag lis from artist xml tag
getArtistTagList :: Content Posn -> [Tag]
getArtistTagList = mapMaybe readTag . getTags
        where
                getTags = tag "artist" /> tag "tag-list" /> tag "tag"

-- |Get release group title from release-group xml tag
--
-- Release Groups should have one, and only one, title
getRelGTitle :: Content Posn -> Maybe String
getRelGTitle = uniList . name
        where
                name = map verbatim . (tag "release-group" /> tag "title" /> txt)

-- |Get release group id from release-group xml tag
getRelGID :: Content Posn -> Maybe String
getRelGID = getAttrValByName "id"

-- |Get artist tag lis from artist xml tag
getRelGTagList :: Content Posn -> [Tag]
getRelGTagList = mapMaybe readTag . getTags
        where
                getTags = tag "release-group" /> tag "tag-list" /> tag "tag"

-- |Read Tag from xml tag
readTag :: Content Posn -> Maybe Tag
readTag = maybePair . (tagName &&& tagCount)
        where
                tagName = uniList . map verbatim . (tag "tag" /> tag "name" /> txt)
                tagCount = fmap read . getAttrValByName "count"


{- XML generic helper functions -}

parseContents :: String -> Content Posn
parseContents = xmlContents . xmlParse "musicdb.err"

-- |Get Attribute list from an Element
getElemAttrs :: Element a -> [Attribute]
getElemAttrs (Elem _ attr _) = attr

-- |Get Contents from HaXml Document
xmlContents :: Document Posn -> Content Posn
xmlContents (Document _ _ e _) = CElem e noPos

-- |Get The Element from content if it is a CElem
getElement :: Content a -> Maybe (Element a)
getElement (CElem e _) = Just e
getElement _ = Nothing

-- |Find an attribute from the list of attributes by name
--
-- Expect attribute to have only one value
getAttrValByName :: String -> Content a -> Maybe String
getAttrValByName name = (=<<) ((=<<) checkAttr . find (compName name)) . fmap getElemAttrs . getElement
        where
                compName str (N n,_) = n == str
                compName _ _ = False

                checkAttr :: Attribute -> Maybe String
                checkAttr (_, AttValue vals) = checkAttrAux =<< uniList vals

                checkAttrAux :: Either String Reference -> Maybe String
                checkAttrAux (Left v) = Just v
                checkAttrAux _ = Nothing


{- General helper functions -}

-- |If a list has only one element, return it
--
-- Used for tag elements which should be unique
uniList :: [a] -> Maybe a
uniList [x] = Just x
uniList _ = Nothing

-- |Transform a Pair of Maybe to a Maybe of a Pair
maybePair :: (Maybe a, Maybe b) -> Maybe (a,b)
maybePair (Nothing, _) = Nothing
maybePair (_, Nothing) = Nothing
maybePair (Just x, Just y) = Just (x, y)
