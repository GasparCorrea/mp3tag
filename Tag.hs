module Tag
( getTag
, getInfo
) where

import ID3.Simple
import Data.Maybe

getTag route = do result <- (readTag route)
                  return (fromJust result)

getInfo tag = let artist = "Artist: "++(fromJust (getArtist tag))
                  album = "Album: "++(fromJust (getAlbum tag))
                  title = "Title: "++(fromJust (getTitle tag))
                  track = "Track: "++(fromJust (getTrack tag))
                  year = "Year: "++(fromJust (getYear tag))
                  in putStr (unlines(title: artist: album: track: []))


