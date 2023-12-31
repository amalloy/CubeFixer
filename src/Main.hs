import Text.Regex.Applicative ((=~), RE, sym, psym, string)
import Text.Regex.Applicative.Common (decimal)
import Control.Applicative (many, some, optional)
import Data.List (intercalate, isPrefixOf, isInfixOf)
import Data.Maybe (fromMaybe)
import Data.Set (Set, member)
import qualified Data.Set as S

type Regex a = RE Char a

data CardDetail = CardDetail {set, num :: String} deriving (Show)
data Card = Card {quantity :: Int, frontFace :: String, backFace :: Maybe String, details :: Maybe CardDetail} deriving (Show)

word :: Regex String
word = some (psym (not . (`elem` " /#()")))

opt :: String -> Regex String
opt s = fromMaybe [] <$> optional (string s)

cardDetail :: Regex CardDetail
cardDetail = CardDetail <$> setSymbol <* sym ' ' <*> identifier
  where setSymbol = sym '(' *> some (psym (/= ')')) <* sym ')'
        identifier = opt "#" <> opt "A-" <> word

face :: Regex String
face = intercalate " " <$> ((:) <$> word <*> many more)
  where more = some (sym ' ') *> word

card :: Regex Card
card = Card <$> decimal <* sym ' ' <*> face <*> back <*> optional (sym ' ' *> cardDetail)
  where back = optional (string " // " *> face)

alchemySets :: Set String
alchemySets = S.fromList ["YWOE", "YONE", "YBRO", "YDMU", "HBG", "YSNC", "YNEO", "YMIN"]

alchemy :: Card -> Bool
alchemy (Card q front _ d) = "A-" `isPrefixOf` front || case d of
  Nothing -> False
  Just (CardDetail s n) -> s `member` alchemySets || "A-" `isInfixOf` n

ttsFormat :: Card -> String
ttsFormat (Card q front _ d) = show q <> " " <> front <> case d of
  Nothing -> ""
  Just (CardDetail s n) -> " (" <> s <> ") " <> n

main :: IO ()
main = interact $ unlines . map ttsFormat . filter (not . alchemy) . map parse . lines
  where parse c = fromMaybe (error ("Couldn't parse [" <> c <> "]")) $ c =~ card
