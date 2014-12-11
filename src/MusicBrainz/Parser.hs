-- Parser.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Musicbrainz interface module for musicdb
--
-- XML parser submodule

-- |Contains functions for XML parsing of the MusicBrainz XML data
module MusicBrainz.Parser (idFromSearch) where

import Control.Applicative

import Data.List
import Data.Maybe

import Text.XML.Light.Input
import Text.XML.Light.Types


{- Exported functions -}

-- |Parse artist ID from MusicBrainz's search XML
idFromSearch :: String -> Maybe [String]
idFromSearch xml = mapMaybe idFromArtist <$> getArtistList xmlData
        where
                xmlData = parseXML . removeHeader $ xml
                -- Read artist ID from artist content element of xml
                idFromArtist :: Content -> Maybe String
                idFromArtist = (=<<) getArtistID . (=<<) checkArtist . getElement
                checkArtist :: Element -> Maybe Element
                checkArtist e
                        | getElementName e /= "artist" = Nothing
                        | otherwise = Just e


{- MusicBrainz XML structure aware functions -}

-- |Get artist list from base XML
getArtistList :: [Content] -> Maybe [Content]
getArtistList [Elem e]
        | getElementName e /= "metadata" = Nothing
        | otherwise = auxGetArtistList $ getElementContents e
       where
               auxGetArtistList [Elem e']
                                | getElementName e' /= "artist-list" = Nothing
                                | otherwise = Just $ getElementContents e'
getArtistList _ = Nothing

-- |Get artist ID from artist xml tag
getArtistID :: Element -> Maybe String
getArtistID (Element {elAttribs=attr}) = getAttr "id" attr


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

{- Attribute content functions -}

-- |Find an attribute on an attribute list and return its value
getAttr :: String -> [Attr] -> Maybe String
getAttr attr = fmap getAttrVal . find ((==) attr . getAttrName)

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


{- General helper functions -}

-- |Remove xml version header
removeHeader :: String -> String
removeHeader = tail . dropWhile (/= '>')
