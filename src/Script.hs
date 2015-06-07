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

getDirectionTuple f s = Direction (f!!0) (f!!1) (f!!2) (f!!3) (s!!0) (s!!1)
getDirectionStr s = getDirectionTuple a b
	where
		a = strToTime $ fst $ splits
		b = strToCommands $ snd $ splits
		splits = splitAt 4 $ BS.split ' ' s

strToTime s = map (\x -> read (BS.unpack x) :: Float) $ s
strToCommands s = map (\x -> BS.unpack x) $ s

splitAtCRLF :: BS.ByteString -- ^ String to split.
            -> Maybe (BS.ByteString, BS.ByteString)
            -- ^  Returns 'Nothing' if there is no CRLF.
splitAtCRLF s = case findCRLF s of
                  Nothing -> Nothing
                  Just (i,l) -> Just (s1, BS.drop l s2)
                      where (s1,s2) = BS.splitAt i s

splitAtCRLF_ :: BS.ByteString -> (BS.ByteString, BS.ByteString)
splitAtCRLF_ s = fromMaybe (s, BS.empty) (splitAtCRLF s)

findCRLF :: BS.ByteString -- ^ String to split.
         -> Maybe (Int64,Int64)
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

g (x, xs) = if (xs == BS.empty) then [x] else x : (g (splitAtCRLF_ xs))

t1 = "0 21.2 0 1 updateTEXT \"0001text\"\r\n"
t2 = "0 72.1 0 2 updateLINK \"END LINK\"\r\n"
t3 = "0 23 0 3 updatePIX \"T201 DNA+stuffedAye-Aye <A9>AleG\"\r\n"
allStr = BS.pack(t1 ++ t2 ++ t3)

main = do
	file <- BS.readFile "/Volumes/LAST_CH_1/CHAPTER1/SECTION1/SCRIPT"
	mapM_ print (a file)
	putStrLn $ show $ map getDirectionStr (a file)
	putStrLn $ show $ length $ a file
		where a file = g (splitAtCRLF_ file)