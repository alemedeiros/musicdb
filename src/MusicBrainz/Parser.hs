-- Parser.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Musicbrainz interface module for musicdb
--
-- XML parser submodule

-- |Contains functions for XML parsing of the MusicBrainz XML data
module MusicBrainz.Parser (getSearchResult, getArtistLookupResult) where

import Control.Arrow
import Control.Applicative

import Data.List
import Data.Maybe

import MusicBrainz.Types

import Text.XML.Light.Input
import Text.XML.Light.Types


{- Exported functions -}

-- |Parse artist ID from MusicBrainz's search XML
getSearchResult :: String -> [(String, String)]
getSearchResult = maybe [] (mapMaybe idFromArtist) . artistList
        where
                artistList = getArtistListFromSearch . parseXML . removeHeader

                -- Read artist ID from artist content element of xml
                idFromArtist :: Content -> Maybe (String, String)
                idFromArtist = readTagInfo . fromContent

                -- Remove from Content datatype and check tag
                fromContent = (=<<) (checkTag "artist") . getElement
                -- Get ID and name from artist element
                readTagInfo = (=<<) (maybePair . (getArtistID &&& getArtistName))

-- |Parse artist informations from MusicBrainz's lookup XML
getArtistLookupResult :: String -> Maybe Artist
getArtistLookupResult = (=<<) readArtistData . artistData
        where
                artistData = getArtistListFromLookup . parseXML . removeHeader

                -- Read artist information from artist xml tag
                readArtistData :: Element -> Maybe Artist
                readArtistData e
                        | isNothing id = Nothing
                        | isNothing name = Nothing
                        | isNothing relg = Nothing
                        | isNothing tags = Nothing
                        | otherwise = Just $ Artist jId jName jRelg jTags
                        where
                                id = getArtistID e
                                name = getArtistName e
                                relg = getArtistRelGroupIDList e
                                tags = getArtistTagList e
                                jId = fromJust id
                                jName = fromJust name
                                jRelg = fromJust relg
                                jTags = fromJust tags


{- MusicBrainz XML structure aware functions -}

-- |Get artist list from base search XML
getArtistListFromSearch :: [Content] -> Maybe [Content]
getArtistListFromSearch [Elem e] = auxSearch . elContent =<< checkTag "metadata" e
       where
               auxSearch :: [Content] -> Maybe [Content]
               auxSearch [Elem e'] = elContent <$> checkTag "artist-list" e'
getArtistListFromSearch _ = Nothing

-- |Get artist from base lookup XML
getArtistListFromLookup :: [Content] -> Maybe Element
getArtistListFromLookup [Elem e] = auxLookup . elContent =<< checkTag "metadata" e
       where
               auxLookup :: [Content] -> Maybe Element
               auxLookup [Elem e'] = checkTag "artist" e'
getArtistListFromLookup _ = Nothing

-- |Get artist ID from artist xml tag
getArtistID :: Element -> Maybe String
getArtistID = getElemAttrVal "id"

-- |Get artist name from artist xml tag
getArtistName :: Element -> Maybe String
getArtistName = getElemText "name"

-- |Get artist name from artist xml tag
getArtistRelGroupIDList :: Element -> Maybe [String]
getArtistRelGroupIDList = fmap readRelGroupID . getElementByName "release-group-list" . elContent
        where
                readRelGroupID :: Element -> [String]
                readRelGroupID = mapMaybe (getElemAttrVal "id") . filterAlbums . mapMaybe checkRelGroup . elContent

                checkRelGroup = (=<<) (checkTag "release-group") . getElement

                filterAlbums = filter isAlbum
                isAlbum = (==) "Album" . fromMaybe "" . getElemAttrVal "type"

-- |Get artist name from artist xml tag
getArtistTagList :: Element -> Maybe [Tag]
getArtistTagList = fmap readTagList . getElementByName "tag-list" . elContent
        where
                readTagList :: Element -> [Tag]
                readTagList = mapMaybe (maybePair . readTag ) . mapMaybe checkTagTag . elContent

                checkTagTag = (=<<) (checkTag "tag") . getElement

                readTag = getElemText "name" &&& (fmap read . getElemAttrVal "count")


{- Get Content actual values functions -}

-- |Get Element from Content
getElement :: Content -> Maybe Element
getElement (Elem e) = Just e
getElement _ = Nothing

-- |Get Data from Content
getData :: Content -> Maybe CData
getData (Text d) = Just d
getData _ = Nothing

-- |Get CRef from Content
getCRef :: Content -> Maybe String
getCRef (CRef r) = Just r
getCRef _ = Nothing


{- Content functions -}

-- |Find an elment on an content list and return
getElementByName :: String -> [Content] -> Maybe Element
getElementByName name = find ((==) name . getElementName) . mapMaybe getElement

{- Attribute content functions -}

-- |Find an attribute on an attribute list and return its value
getAttrByName :: String -> [Attr] -> Maybe Attr
getAttrByName attr = find ((==) attr . getAttrName)

-- |Get Attribute name
getAttrName :: Attr -> String
getAttrName = qName . attrKey


{- Element content functions -}

-- |Get Element name
getElementName :: Element -> String
getElementName = qName . elName

-- |Check if current element is a tag with the given name
checkTag :: String -> Element -> Maybe Element
checkTag str e
        | getElementName e /= str = Nothing
        | otherwise = Just e

-- |Read the Text from element content
readTagText :: Element -> Maybe String
readTagText (Element {elContent=[Text cd]}) = Just $ cdData cd
readTagText _ = Nothing

getElemAttrVal :: String -> Element -> Maybe String
getElemAttrVal attr = fmap attrVal . getAttrByName attr . elAttribs

getElemText :: String -> Element -> Maybe String
getElemText elem = (=<<) readTagText . getElementByName elem . elContent

{- Data content functions -}



{- General helper functions -}

-- |Remove xml version header
removeHeader :: String -> String
removeHeader = tail . dropWhile (/= '>')

-- |Transform a Pair of Maybe to a Maybe of a Pair
maybePair :: (Maybe a, Maybe b) -> Maybe (a,b)
maybePair (Nothing, _) = Nothing
maybePair (_, Nothing) = Nothing
maybePair (Just x, Just y) = Just (x, y)
