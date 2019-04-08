{-# LANGUAGE Strict     #-}
{-# LANGUAGE StrictData #-}
module Main where

import           Control.Monad
import           Data.Char
import qualified Data.Map.Strict as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set        as S
import           Debug.Trace     (trace)
import           System.IO


-- lib

class GCJRead a where
  read' :: String -> a

instance GCJRead Int where
  read' = read

instance (Read a, Read b) => GCJRead (a, b) where
  read' = (\(a:b:_) -> (read a, read b)) . words

instance Read a => GCJRead [a] where
  read' = fmap read . words

getGCJ :: GCJRead a => IO a
getGCJ = read' <$> getLine

showTuple :: (Show a1, Show a2) => (a1, a2) -> String
showTuple (a, b) = show a <> " " <> show b

debug :: Show a => a -> a
debug = id
-- debug x = trace (show x) x

-- main

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  caseNum <- getGCJ :: IO Int
  forM_ [1..caseNum] $ \num -> do
    r <- solve <$> getGCJ <*> getGCJ
    putStrLn $ "Case #" <> show num <> ": " <> r

solve :: (Int, Int) -> [Int] -> String
solve (gn, gl) ns =
  let hX = gcd (head ns) (ns !! 1)
      h = head ns `div` hX
      ps = reverse $ snd $ foldl f (h, [h]) (ns)
      m = M.fromList $ zip (S.toAscList . S.fromList $ ps) ['A'..'Z']
  in fmap (m M.!) ps
  where
    f (x, acc) v
      | otherwise =
        let (y, m) =  v `divMod` x
        in if m /= 0 then error "?" else (y, y:acc)

-- isPangrams xs = all (`elem` (toLower <$> xs)) ['a'..'z']


{-
2
103 31
217 1891 4819 2291 2987 3811 1739 2491 4717 445 65 1079 8383 5353 901 187 649 1003 697 3239 7663 291 123 779 1007 3551 1943 2117 1679 989 3053
10000 25
3292937 175597 18779 50429 375469 1651121 2102 3722 2376497 611683 489059 2328901 3150061 829981 421301 76409 38477 291931 730241 959821 1664197 3057407 4267589 4729181 5335543

Case #1: CJQUIZKNOWBEVYOFDPFLUXALGORITHMS
Case #2: SUBDERMATOGLYPHICFJKNQVWXZ


a = 11
b = 17
c = 31

bac
ba = 187
ac = 341

eaclidian: O(log n)
hs: gcd

187 341

11
17
31

17 11 31

217 1891 4819 2291
CJ   JQ   QU   UI
CJQUI

-}
{-
d = [(3,'A'),(5,'B'),(7,'C'),(11,'D'),(13,'E'),(17,'F'),(19,'G'),(23,'H'),(29,'I'),(31,'J'),(37,'K'),(41,'L'),(43,'M'),(47,'N'),(53,'O'),(59,'P'),(61,'Q'),(67,'R'),(71,'S'),(73,'T'),(79,'U'),(83,'V'),(3323,'W'),(6829,'X'),(6833,'Y'),(9973,'Z')]

d'' = [('A', 2), ('B', 9769), ('C', 9781), ('D', 9787), ('E', 9791), ('F', 9803), ('G', 9811), ('H', 9817), ('I', 9829), ('J', 9833), ('K', 9839), ('L', 9851), ('M', 9857), ('N', 9859), ('O', 9871), ('P', 9883), ('Q', 9887), ('R', 9901), ('S', 9907), ('T', 9923), ('U', 9929), ('V', 9931), ('W', 9941), ('X', 9949), ('Y', 9967), ('Z', 9973)]

d' = f <$> d
  where f (a,b) = (b, a)

-- t = "CJQUIZKNOWBEVYOFDPFLUXALGORITHMS"
-- t = "ZZQUQUZUZUDRZEZWNZQPPITYUKDUDUDUDUSJYSNGFSTEADNXGFXGFOPVJETRWCESSDFZDSAQRWAFDZXMHCLITUDBFXCAFXFWCITFOYUVMHCFGSTRSUDETYFVFHNFHVDCTRHDGFBHGFGDDUFUGUGUDUTDEUTDUFLUGVYUHTDBGZTJKZZQQQWWPKNNNMJUFUFUQUQU"
t = "ONTHECODEJAMTEAMWEENJOYSENDINGEACHOTHERPANGRAMSWHICHAREPHRASESTHATUSEEACHLETTEROFTHEENGLISHALPHABETATLEASTONCEONECOMMONEXAMPLEOFAPANGRAMISTHEQUICKBROWNFOXJUMPSOVERTHELAZYDOGSOMETIMESOURPANGRAMSCONTAINCONFIDENTIALINFORMATIONFOREXAMPLECJQUIZKNOWBEVYOFDPFLUXALGORITHMSSOWENEEDTOKEEPTHEMSECURE"

ff = do
  let rs = fromJust . f <$> zip t (tail t)
  putStrLn (unwords $  show <$> rs)
  where f (a, b) = do
          x <- lookup a d''
          y <- lookup b d''
          pure $ x * y
-}
