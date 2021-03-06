-- Types.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Musicbrainz interface module for musicdb
--
-- Datatypes definition submodule

-- |Contains all the internal Datatypes definitions and its classes instances
module MusicBrainz.Types where

-- |Artist datatype
--
-- TODO Datatype description
data Artist = Artist
        { artID :: String
        , artName :: String
        , artRelGroupIDList :: [String]
        , artTagList :: [Tag]
        --, artCountry :: Maybe String
        --, artArea :: Maybe Area
        }

instance Eq Artist where
        (==) Artist{artID=a} Artist{artID=b} = a == b

-- Pretty print for Artist
instance Show Artist where
        show (Artist id name rels tags) = "Artist:\t" ++ name  ++ "\nid:\t" ++ id
                ++ "\ntags:\n" ++ foldl (\l -> listStr l . show) "" tags
                ++ "\nalbums (id):\n" ++ foldl listStr "" rels
                        where
                                listStr :: String -> String -> String
                                listStr l e = l ++ "\t" ++ e ++ "\n"

-- |Release-Group datatype
--
-- TODO Datatype description
data ReleaseGroup = ReleaseGroup
        { relID :: String
        , relTitle :: String
        , relType :: String
        -- , relArtistID :: String
        , relTagList :: [Tag]
        }

-- Pretty print for Release-Group
instance Show ReleaseGroup where
        show (ReleaseGroup id title typ tags) = "Release:\t" ++ title  ++ "\nid:\t\t" ++ id
                ++ "\ntype:\t\t" ++ typ ++ "\ntags:\n" ++ foldl (\l -> listStr l . show) "" tags
                        where
                                listStr :: String -> String -> String
                                listStr l e = l ++ "\t" ++ e ++ "\n"

type Tag = (String, Int)
