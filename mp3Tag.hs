import ID3.Simple
import Data.Maybe
import System.Environment

getTag route = do result <- (readTag route)
                  return (fromJust result)

getInfo tag = let artist = "Artist: "++(fromJust (getArtist tag))
                  album = "Album: "++(fromJust (getAlbum tag))
                  title = "Title: "++(fromJust (getTitle tag))
                  track = "Track: "++(fromJust (getTrack tag))
                  year = "Year: "++(fromJust (getYear tag))
                  in putStr (unlines(title: artist: album: track: []))

loop [] tag route = do getInfo tag
                       writeTag route tag
loop (parameter:input:list) tag route = case parameter of
                                            "-artist" -> loop list (setArtist input tag) route
                                            "-album" -> loop list (setAlbum input tag) route
                                            "-title" -> loop list (setTitle input tag) route
                                            "-track" -> loop list (setTrack input tag) route
                                            "-year" -> loop list (setYear input tag) route
                                            _-> putStr ("error: "++parameter++" is not a valid parameter")

main =  do args <- getArgs
           tag <- getTag (head args)
           loop (tail args) tag (head args)
