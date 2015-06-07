import qualified Data.ByteString.Lazy.Char8 as BS
import System.IO
import Data.Int (Int64)
import Control.Monad
import Data.Maybe
import Control.Monad.IO.Class
import Control.Lens

data Direction = Direction {
		directionStartTime :: Float,
		directionNextTime :: Float,
		directionPreviousTime :: Float,
		directionIndex :: Float,
		directionCommand :: String,
		directionValue :: String
	} deriving (Show, Read, Eq, Ord)


directionFromTuple :: [Float] -> [String] -> Direction
directionFromTuple f s = Direction (f!!0) (f!!1) (f!!2) (f!!3) (s!!0) (s!!1)

directionFromStr :: BS.ByteString -> Direction
directionFromStr s = directionFromTuple a b
	where
		a = strToTime $ fst $ splits
		b = strToCommands $ snd $ splits
		splits = splitAt 4 $ BS.split ' ' s

strToTime :: [BS.ByteString] -> [Float]
strToTime s = map (\x -> read (BS.unpack x) :: Float) $ s

strToCommands :: [BS.ByteString] -> [[Char]]
strToCommands s = map (\x -> BS.unpack x) $ s

splitAtCRLF :: BS.ByteString -> Maybe (BS.ByteString, BS.ByteString)
splitAtCRLF s = case findCRLF s of
                  Nothing -> Nothing
                  Just (i,l) -> Just (s1, BS.drop l s2)
                      where (s1,s2) = BS.splitAt i s

splitAtCRLF_ :: BS.ByteString -> (BS.ByteString, BS.ByteString)
splitAtCRLF_ s = fromMaybe (s, BS.empty) (splitAtCRLF s)

findCRLF :: BS.ByteString -> Maybe (Int64,Int64)
findCRLF s = 
    case findCRorLF s of
		Nothing -> Nothing
		Just j | BS.null (BS.drop (j+1) s) -> Just (j,1)
		Just j -> case (BS.index s j, BS.index s (j+1)) of
			('\n','\r') -> Just (j,2)
			('\r','\n') -> Just (j,2)
			_           -> Just (j,1)

findCRorLF :: BS.ByteString -> Maybe Int64
findCRorLF = BS.findIndex (\c -> c == '\n' || c == '\r')

streamToList :: (BS.ByteString, BS.ByteString) -> [BS.ByteString]
streamToList (x, xs) = if (xs == BS.empty) then [x] else x : (streamToList $ splitAtCRLF_ xs)

main = do
	file <- BS.readFile "/Volumes/LAST_CH_1/CHAPTER1/SECTION1/SCRIPT"
	putStrLn $ show $ map directionFromStr (a file)
	putStrLn $ show $ length $ a file
		where a file = streamToList (splitAtCRLF_ file)