import Sort
import Tag
import System.Environment
import ID3.Simple


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
           case (head args) of
            "-sortArtist" -> sort getArtist
            "-sortAlbum" -> sort getAlbum
            "-tag" -> do tag <- getTag (head(tail args))
                         loop (drop 2 args) tag (head(tail args))
