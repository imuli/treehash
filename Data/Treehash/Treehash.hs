{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
module Data.Treehash.Treehash
  ( Treehash(..)
  , Treehashable(..)
  , Treehashable1(..)
  , Salt(..)
  ) where

import           Data.Bifunctor (bimap)
import           Data.Bits (Bits, shiftR, unsafeShiftR, (.|.))
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Encoding.Base41 as Base41
import           Data.Serialize
import           Data.Word

data Salt = Salt Word32 Word32 Word32 Word32

class (Eq h, Ord h, Show h, Serialize h) => Treehash h where
  zero :: h
  hash :: h -> h -> Salt -> h
  hashLen :: Num n => n
  hashLen = fromIntegral $ BS.length $ encode $ zero @h
  to41 :: h -> BS.ByteString
  to41 = Base41.encode41 . encode
  from41 :: BS.ByteString -> Either String h
  from41 = decode . Base41.decode41Lenient

class Treehash h => Treehashable d h where
  treehash :: d -> h

class Treehash h => Treehashable1 f h where
  treehash1 :: Treehashable a h => f a -> h

instance Treehash h => Treehashable1 Maybe h where
  treehash1 Nothing  = zero
  treehash1 (Just x) = hash zero (treehash x) $ Salt 1 0 0 0

instance (Treehash h, Treehashable a h) => Treehashable (Maybe a) h where
  treehash = treehash1

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

halfCeiling :: (Bits n, Num n) => n -> n
halfCeiling x =
  let a = x - 1
      b = a .|. unsafeShiftR a 1
      c = b .|. unsafeShiftR b 2
      d = c .|. unsafeShiftR c 4
      e = d .|. unsafeShiftR d 8
      f = e .|. unsafeShiftR e 16
   in unsafeShiftR f 1 + 1

instance (Treehash h) => Treehashable BS.ByteString h where
  treehash x =
    let len = BS.length x
        size = 2 * hashLen @h
        half = halfCeiling len
        Right (a, b) =
          case compare len size of
               LT -> decode $ x <> BS.replicate size 0
               EQ -> decode x
               GT -> Right $ bimap treehash treehash $ BS.splitAt half x
     in hash a b $ Salt 0 0 0 $ fromIntegral len

instance (Treehash h) => Treehashable LBS.ByteString h where
  treehash x =
    let len = LBS.length x
        size = 2 * hashLen @h
        half = halfCeiling len
        Right (a, b) =
          case compare len size of
               LT -> decode $ LBS.toStrict $ x <> LBS.replicate size 0
               EQ -> decode $ LBS.toStrict x
               GT -> Right $ bimap treehash treehash $ LBS.splitAt half x
     in hash a b $ Salt 0 0 0 $ fromIntegral len
