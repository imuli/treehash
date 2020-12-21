{-#OPTIONS_GHC -Wall #-}
module Main
  ( main
  ) where

import           Data.Encoding.Base16 (encode16)
import           Data.Serialize (encode)
import           Data.Text (unpack)
import           Data.Text.Encoding (decodeUtf8)
import           Data.Treehash

toString :: Blake2s1 -> String
toString = unpack . decodeUtf8 . encode16 . encode

filterData :: [String] -> [String]
filterData = filter (\x -> head x == '\t')

trimTo :: Eq t => t -> [t] -> [t]
trimTo y = reverse . dropTo . reverse . dropTo
  where
    dropTo []       = []
    dropTo (x : xs) = if x == y then xs else dropTo xs

extractHashes :: [String] -> [String]
extractHashes = map (trimTo '"')

makeHashes :: Blake2s1 -> Int -> [Blake2s1]
makeHashes _ 0 = []
makeHashes inHash n = let outHash = hash inHash inHash (Salt 0 0 0 0)
                       in outHash : makeHashes outHash (n-1)

testHashes :: [Blake2s1]
testHashes = makeHashes zero 20

declareEqual :: Show a => Eq a => a -> a -> String
declareEqual x y = if x == y then show x else show x ++ " != " ++ show y

main :: IO ()
main = do
  input <- readFile "test/blake2s1_vectors.jsonp"
  putStr $ unlines $ zipWith declareEqual (toString <$> testHashes) $ extractHashes $ filterData $ lines input
