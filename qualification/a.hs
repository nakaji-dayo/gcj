module Main where

import           Control.Monad
import           Data.Monoid
import           System.IO

-- lib

getInt :: IO Int
getInt = read <$> getLine

showTuple :: (Show a1, Show a2) => (a1, a2) -> String
showTuple (a, b) = show a <> " " <> show b

-- main

main :: IO ()
main = do
  hSetBuffering stdout NoBuffering
  caseNum <- getInt
  forM_ [1..caseNum] $ \num -> do
    r <- solve <$> getInt
    putStrLn $ "Case #" <> show num <> ": " <> (showTuple r)

solve v =
  let d1 = read $ f <$> show v
  in (v - d1, d1)
  where
    f '4' = '1'
    f _   = '0'
