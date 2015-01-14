-- Analysis.hs
--  by alemedeiros <alexandre.n.medeiros _at_ gmail.com>
--
-- Musicbrainz interface module for musicdb
--
-- Music analysis module

-- |Contains functions for generating a style tag list for records, used for
-- recomendations
module MusicBrainz.Analysis (
        analyseArtist,
        genStyleTagList,
) where

import Control.Arrow (second)

import Data.Function (on)
import Data.List
import Data.Map (Map)
import qualified Data.Map.Strict as Map
import Data.Maybe

import MusicBrainz.Types

-- |Generate a score for the artist based on the tag list given
analyseArtist :: [Tag] -> Artist -> (Int, Artist)
analyseArtist ts a = (sum score, a)
        where
                score = concatMap (\t -> map (combineTags (*) t) ts) $ artTagList a

-- |Combine two tags' count with a function, if they have the same name
combineTags :: (Int -> Int -> Int) -> Tag -> Tag -> Int
combineTags f (ta,ca) (tb,cb)
        | ta == tb = f ca cb
        | otherwise = 0

-- |Generate the Style Tag List: which is the union of the release and artist
-- tag, giving a weight of 10 to the release tags
genStyleTagList :: (Artist, ReleaseGroup) -> [Tag]
genStyleTagList (art,rel) = sortTagList . Map.toList $ Map.unionWith (+) artTag relTag
        where
                relTag = Map.fromList . map (second (10*)) $ relTagList rel
                artTag = Map.fromList $ artTagList art

-- |An alias for a sort function for the Tag List, sorting the tags by count and
-- greater count first
sortTagList :: [Tag] -> [Tag]
sortTagList = sortBy (flip compare `on` snd)
