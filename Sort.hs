module Sort
( sort
) where
import ID3.Simple
import Data.Maybe
import Data.String.Utils
import System.Directory
import Tag

mp3List [] outputList = outputList
mp3List (x:xs) outputList = if (endswith ".mp3" x)
                               then mp3List xs (x:outputList)
                               else mp3List xs outputList

listAssociation f [] outputList path = do return outputList
listAssociation f (x:xs) outputList path = do tag <- getTag (path ++"/" ++ x)
                                              associatedList <- (listAssociation f xs ((x,fromJust (f tag )):outputList) path)
                                              return associatedList

moveTo [] path = putStrLn "Complete"
moveTo (x:xs) path = let folderPath = (path ++"/"++ (snd x))
                         oldPath = (path ++"/"++ (fst x))
                         newPath = (folderPath++"/"++(fst x))
                         in do createDirectoryIfMissing False folderPath
                               renamePath oldPath newPath
                               moveTo xs path

sort f =  do path <- getCurrentDirectory
             files <- listDirectory path
             associatedList <- (listAssociation f (mp3List files []) [] (path++"/"))
             return associatedList
             moveTo associatedList path
