import ID3.Simple
import Data.Maybe
import System.Environment

changeArtist route artist = do result <- (readTag route)
                               (writeTag route (setArtist artist (fromJust result)))

changeAlbum route album = do result <- (readTag route)
                             (writeTag route (setAlbum album (fromJust result)))

changeTitle route title = do result <- (readTag route)
                             (writeTag route (setTitle title (fromJust result)))

changeYear route year = do result <- (readTag route)
                           (writeTag route (setYear year (fromJust result)))

changeTrack route track = do result <- (readTag route)
                             (writeTag route (setTrack track (fromJust result)))


getInfo route = do result <- (readTag route)
                   let artist = "Artist: "++(fromJust (getArtist (fromJust result)))
                       album = "Album: "++(fromJust (getAlbum (fromJust result)))
                       title = "Title: "++(fromJust (getTitle (fromJust result)))
                       track = "Track: "++(fromJust (getTrack (fromJust result)))
                       year = "Year: "++(fromJust (getYear (fromJust result)))
                       in putStr (unlines(title: artist: album:year:track:[]))

main = do args <- getArgs
          case args of
            ["-artist",route, artist] -> changeArtist route artist
            ["-album",route, album] -> changeAlbum route album
            ["-title",route,title] -> changeTitle route title
            ["-track",route,track] -> changeTrack route track
            ["-year",route,year] -> changeYear route year
            ["-info",route] ->  getInfo route
            _-> putStrLn "error: not enough arguments"
