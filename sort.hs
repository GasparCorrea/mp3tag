import ID3.Simple
import Data.Maybe
import Data.String.Utils
import System.Directory

getTag route = do result <- (readTag route)
                  return (fromJust result)

mp3List [] outputList = outputList
mp3List (x:xs) outputList = if (endswith ".mp3" x)
                               then mp3List xs (x:outputList)
                               else mp3List xs outputList

listAssociation [] outputList = do return outputList
listAssociation (x:xs) outputList = do tag <- getTag ("/home/gasparoctavio/Música/"++ x)
                                       associatedList <- (listAssociation xs ((x,fromJust (getArtist tag )):outputList))
                                       return associatedList

moveTo [] = putStrLn "Complete"
moveTo (x:xs) = let folderPath = ("/home/gasparoctavio/Música/"++(snd x))
                    oldPath = ("/home/gasparoctavio/Música/"++(fst x))
                    newPath = (folderPath++"/"++(fst x))
                    in do createDirectoryIfMissing False folderPath
                          renamePath oldPath newPath
                          moveTo xs
