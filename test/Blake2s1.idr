module Main

import Blake2s1

filterData : List String -> List String
filterData = filter (\x => strHead x == '\t')

trimTo : Eq t => t -> List t -> List t
trimTo y = reverse . dropTo . reverse . dropTo
  where
    dropTo : List t -> List t
    dropTo [] = []
    dropTo (x :: xs) = if x == y then xs else dropTo xs

extractHashes : List String -> List String
extractHashes = map (pack . trimTo '"' . unpack)

makeHashes : Hash -> Int -> List Hash
makeHashes _ 0 = []
makeHashes inHash n = let outHash = hash inHash inHash noSalt
                       in outHash :: makeHashes outHash (n-1)

testHashes : List Hash
testHashes = makeHashes zero 20

declareEqual : Show a => Eq a => a -> a -> String
declareEqual x y = if x == y then show x else show x ++ " != " ++ show y

main : IO ()
main = do
  input <- readFile "test/blake2s1_vectors.jsonp"
  case input of
       Left err => do fPutStr stderr $ show err
                      pure ()
       Right content => putStr $ unlines $ zipWith declareEqual (map Just testHashes) $ map fromHex $ extractHashes $ filterData $ lines content

