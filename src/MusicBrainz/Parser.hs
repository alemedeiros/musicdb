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
--getArtistLookupResult :: String -> Maybe Artist
getArtistLookupResult = (=<<) magic . artistData
        where
                artistData = getArtistListFromLookup . parseXML . removeHeader

                -- Read artist information from artist xml tag
                magic :: Element -> Maybe Artist
                magic _ = Nothing


{- MusicBrainz XML structure aware functions -}

-- |Get artist list from base search XML
getArtistListFromSearch :: [Content] -> Maybe [Content]
getArtistListFromSearch [Elem e] = auxSearch . getElementContents =<< checkTag "metadata" e
       where
               auxSearch :: [Content] -> Maybe [Content]
               auxSearch [Elem e'] = getElementContents <$> checkTag "artist-list" e'
getArtistListFromSearch _ = Nothing

-- |Get artist from base lookup XML
getArtistListFromLookup :: [Content] -> Maybe Element
getArtistListFromLookup [Elem e] = auxLookup . getElementContents =<< checkTag "metadata" e
       where
               auxLookup :: [Content] -> Maybe Element
               auxLookup [Elem e'] = checkTag "artist" e'
getArtistListFromLookup _ = Nothing

-- |Get artist ID from artist xml tag
getArtistID :: Element -> Maybe String
getArtistID (Element {elAttribs=attr}) = getAttrVal <$> getAttrByName "id" attr

-- |Get artist name from artist xml tag
getArtistName :: Element -> Maybe String
getArtistName (Element {elContent=cont}) = readTagText =<< getElementByName "name" cont

-- |Get artist name from artist xml tag
getArtistRelGroupIDList :: Element -> Maybe [String]
getArtistRelGroupIDList (Element {elContent=cont}) = readRelGroupID =<< getElementByName "release-group-list" cont
        where
                readRelGroupID = undefined

-- |Get artist name from artist xml tag
getArtistTagList :: Element -> Maybe Tag
getArtistTagList (Element {elContent=cont}) = readTagList =<< getElementByName "tag-list" cont
        where
                readTagList = undefined


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
getAttrName (Attr {attrKey=(QName {qName=name})}) = name

-- |Get Attribute value
getAttrVal :: Attr -> String
getAttrVal (Attr {attrVal=val}) = val


{- Element content functions -}

-- |Get Element contents
getElementContents :: Element -> [Content]
getElementContents (Element {elContent=conts}) = conts

-- |Get Element name
getElementName :: Element -> String
getElementName (Element {elName=(QName {qName=name})}) = name

-- |Check if current element is a tag with the given name
checkTag :: String -> Element -> Maybe Element
checkTag str e
        | getElementName e /= str = Nothing
        | otherwise = Just e

-- |Read the Text from element content
readTagText :: Element -> Maybe String
readTagText (Element {elContent=[Text cd]}) = Just $ getDataContent cd
readTagText _ = Nothing


{- Data content functions -}

-- |Get Data content
getDataContent :: CData -> String
getDataContent (CData {cdData=d}) = d


{- General helper functions -}

-- |Remove xml version header
removeHeader :: String -> String
removeHeader = tail . dropWhile (/= '>')

-- |Transform a Pair of Maybe to a Maybe of a Pair
maybePair :: (Maybe a, Maybe b) -> Maybe (a,b)
maybePair (Nothing, _) = Nothing
maybePair (_, Nothing) = Nothing
maybePair (Just x, Just y) = Just (x, y)
