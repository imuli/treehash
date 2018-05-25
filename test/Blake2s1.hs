{-#OPTIONS_GHC -Wall #-}

module Main (main) where

import Blake2s1

filterData :: [String] -> [String]
filterData = filter (\x -> head x == '\t')

trimTo :: Eq t => t -> [t] -> [t]
trimTo y = reverse . dropTo . reverse . dropTo
  where
    dropTo [] = []
    dropTo (x : xs) = if x == y then xs else dropTo xs

extractHashes :: [String] -> [String]
extractHashes = map (trimTo '"')

makeHashes :: Hash -> Int -> [Hash]
makeHashes _ 0 = []
makeHashes inHash n = let outHash = hash inHash inHash (0,0,0,0)
                       in outHash : makeHashes outHash (n-1)

testHashes :: [Hash]
testHashes = makeHashes zero 20

declareEqual :: Show a => Eq a => a -> a -> String
declareEqual x y = if x == y then show x else show x ++ " != " ++ show y

main :: IO ()
main = do
  input <- readFile "test/blake2s1_vectors.jsonp"
  putStr $ unlines $ zipWith declareEqual (map Just testHashes) $ map fromHex $ extractHashes $ filterData $ lines input

