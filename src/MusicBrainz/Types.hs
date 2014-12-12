-- Types.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Musicbrainz interface module for musicdb
--
-- Datatypes definition submodule

-- |Contains all the internal Datatypes definitions and its classes instances
module MusicBrainz.Types where

-- |Artist datatype
-- TODO Datatype description
-- TODO possibly useful fields left commented and nonessential fields are Maybe
data Artist = Artist
        { artID :: String
        , artName :: String
        , artRelGroupsIDList :: [String]
        , artTagList :: [Tag]
        --, artCountry :: Maybe String
        --, artArea :: Maybe Area
        }

-- Pretty print for Artist
instance Show Artist where
        show (Artist id name rels tags) = "Artist: " ++ name  ++ "\nid: " ++ id
                ++ "\ntags: " ++ show tags ++ "\nalbums (id): " ++ show rels

-- |Release-Group datatype
-- TODO Datatype description
-- TODO possibly useful fields left commented and nonessential fields are Maybe
data ReleaseGroup = ReleaseGroup
        { relID :: String
        , relTitle :: String
        -- , relType :: String -- probably gonna filter only Albuns
        -- , relArtistID :: String
        , relTagList :: [Tag]
        } deriving (Show)

type Tag = (String, Int)
