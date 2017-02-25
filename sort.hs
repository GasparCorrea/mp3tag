import ID3.Simple
import Data.Maybe
import Data.String.Utils
import System.Directory

getTag route = do result <- (readTag route)
                  return (fromJust result)

mp3List [] outputList = outputList
mp3List (x:xs) outputList = if (endswith ".mp3" (head inputList))
                               then mp3List (tail inputList) ((head inputList):outputList)
                               else mp3List (tail inputList) outputList

f [] outputList = do return outputList
f inputList outputList = do tag <- getTag ("/home/gasparoctavio/Música/"++(head inputList))
                            associatedList <- (f (tail inputList) (((head inputList),fromJust (getArtist tag )):outputList))
                            return associatedList

moveTo [] = putStrLn "Complete"
moveTo inputList = let folderPath = ("/home/gasparoctavio/Música/"++(snd (head inputList)))
                       oldPath = ("/home/gasparoctavio/Música/"++(fst (head inputList)))
                       newPath = (folderPath++"/"++(fst(head inputList)))
                       in do createDirectoryIfMissing False folderPath
                             renamePath oldPath newPath
                             moveTo (tail inputList)
