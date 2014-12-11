-- Parser.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Musicbrainz interface module for musicdb
--
-- XML parser submodule

-- |Contains functions for XML parsing of the MusicBrainz XML data
module MusicBrainz.Parser (idFromSearch) where

import Data.Maybe

import Text.XML.Light.Input
import Text.XML.Light.Types

-- |Parse artist ID from MusicBrainz's search XML
idFromSearch :: String -> [Maybe String]
idFromSearch xml = fromJust . fmap (map getID) $ getArtList xmlData
        where
                xmlData = parseXML . removeHeader $ xml

-- |Get artist list from base XML
getArtList :: [Content] -> Maybe [Content] 
getArtList [Elem (Element {elContent=[Elem (Element {elContent=artList})]})] = Just artList
getArtList _ = Nothing

-- |Get artist ID from artist xml tag
getID :: Content -> Maybe String
getID (Elem (Element {elAttribs=attr})) = getAttr "id" attr
getID _ = Nothing

-- |Find an attribute on an attribute list and return its value
getAttr :: String -> [Attr] -> Maybe String
getAttr _ [] = Nothing
getAttr attr ((Attr {attrVal=val, attrKey=(QName {qName=key})}):attrs)
                            | key == attr = Just val
                            | otherwise = getAttr attr attrs

-- |Remove xml version header
removeHeader :: String -> String
removeHeader = tail . dropWhile (/= '>')
