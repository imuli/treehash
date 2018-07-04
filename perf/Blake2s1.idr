module Main

import Blake2s1
import System

-- how deep to go
n : Int
n = 1000000

getHash : Int -> Hash -> Hash
getHash 0 prev = prev
getHash m prev = getHash (m-1) $ hash prev prev noSalt

main : IO ()
main = do
  start <- time
  putStrLn $ show $ getHash n zero
  end <- time
  let sec : Double = cast $ end - start
  let khps = (cast n) / (sec * 1000)
  let mbps = 64 * khps / 1000
  putStrLn $ (show khps) ++ "KH/s (" ++ (show mbps) ++ " MB/s)"

