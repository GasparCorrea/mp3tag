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

main = do args <- getArgs
          case args of
            ["-artist",route, artist] -> changeArtist route artist
            ["-album",route, album] -> changeAlbum route album
            ["-title",route,title] -> changeTitle route title
            ["-track",route,track] -> changeTrack route track
            ["-year",route,year] -> changeYear route year
            _-> putStrLn "error: exactly three arguments needed"

