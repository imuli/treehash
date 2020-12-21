{-#OPTIONS_GHC -Wall #-}

module Main
  ( main
  ) where

import           Data.Time.Clock
import           Data.Treehash

-- how deep to go
n :: Int
n = 1000000

getHash :: Int -> Blake2s1 -> Blake2s1
getHash 0 prev = prev
getHash m prev = getHash (m-1) $! hash prev prev (Salt 0 0 0 0)

main :: IO ()
main = do
  start <- getCurrentTime
  print $ getHash n zero
  end <- getCurrentTime
  let sec :: Float
      sec = realToFrac (diffUTCTime end start)
      khps :: Float
      khps = (fromIntegral n) / (sec * 1000)
      mbps :: Float
      mbps = 64 * khps / 1000
   in putStr $ (show khps) ++ "KH/s (" ++ (show mbps) ++ " MB/s)\n"

