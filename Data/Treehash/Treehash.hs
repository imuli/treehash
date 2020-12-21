{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
module Data.Treehash.Treehash
  ( Treehash(..)
  , Treehashable(..)
  , Salt(..)
  ) where

import           Data.Bits
import           Data.ByteString as BS
import qualified Data.Encoding.Base41 as Base41
import           Data.Serialize
import           Data.Word

data Salt = Salt Word32 Word32 Word32 Word32

class Serialize h => Treehash h where
  zero :: h
  hash :: h -> h -> Salt -> h
  to41 :: h -> ByteString
  to41 = Base41.encode41 . encode
  from41 :: ByteString -> Either String h
  from41 = decode . Base41.decode41Lenient

class Treehash h => Treehashable d h where
  treehash :: d -> h

instance (Treehash h, Treehashable d h) => Treehashable (Maybe d) h where
  treehash Nothing  = zero
  treehash (Just x) = hash zero (treehash x) $ Salt 1 0 0 0

instance (Treehash h, Treehashable d h) => Treehashable [d] h where
  treehash []       = zero
  treehash (x : xs) = hash (treehash x) (treehash xs) $ Salt 1 0 0 0

instance (Treehash h, Treehashable a h, Treehashable b h) => Treehashable (Either a b) h where
  treehash (Left x)  = hash zero (treehash x) $ Salt 1 0 0 0
  treehash (Right y) = hash zero (treehash y) $ Salt 2 0 0 0

instance (Treehash h, Treehashable a h, Treehashable b h) => Treehashable (a,b) h where
  treehash (x,y) = hash (treehash x) (treehash y) $ Salt 1 0 0 0


instance (Treehash h) => Treehashable Word h where
  treehash x = hash zero zero $ Salt (fromIntegral (x `shiftR` 96))
                                     (fromIntegral (x `shiftR` 64))
                                     (fromIntegral (x `shiftR` 32))
                                     (fromIntegral (x `shiftR` 0))

instance (Treehash h) => Treehashable Word64 h where
  treehash x = hash zero zero $ Salt 64 0 (fromIntegral (x `shiftR` 32)) (fromIntegral x)

instance (Treehash h) => Treehashable Word32 h where
  treehash x = hash zero zero $ Salt 32 0 0 x

instance (Treehash h) => Treehashable Word16 h where
  treehash x = hash zero zero $ Salt 16 0 0 (fromIntegral x)

instance (Treehash h) => Treehashable Word8 h where
  treehash x = hash zero zero $ Salt 8 0 0 (fromIntegral x)

instance (Treehash h) => Treehashable Bool h where
  treehash x = hash zero zero $ Salt 1 0 0 (if x then 1 else 0)
