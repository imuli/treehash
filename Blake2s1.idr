module Blake2s1

import Data.Vect

%default total

export
data Hash = H Bits32 Bits32 Bits32 Bits32 Bits32 Bits32 Bits32 Bits32
%name Hash h,g

||| the empty hash
export
zero : Hash
zero = H 0 0 0 0 0 0 0 0

||| the type of user-supplied salt
public export
data Salt = S Bits32 Bits32 Bits32 Bits32

||| an empty salt
export
noSalt : Salt
noSalt = S 0 0 0 0

-- support data types and operators for the hash function

data State = St Bits32 Bits32 Bits32 Bits32 Bits32 Bits32 Bits32 Bits32 Bits32 Bits32 Bits32 Bits32 Bits32 Bits32 Bits32 Bits32

infixl 10 |>
(|>) : a -> (a -> b) -> b
x |> f = f x

infixr 10 .|.
(.|.) : Bits32 -> Bits32 -> Bits32
(.|.) = prim__orB32

infixr 10 .^.
(.^.) : Bits32 -> Bits32 -> Bits32
(.^.) = prim__xorB32

infixr 10 .&.
(.&.) : Bits32 -> Bits32 -> Bits32
(.&.) = prim__andB32

shr : Bits32 -> Bits32 -> Bits32
shr = prim__lshrB32

shl : Bits32 -> Bits32 -> Bits32
shl = prim__shlB32

rotr : Bits32 -> Integer -> Bits32
rotr x c = shl x (fromInteger $ 32 - c) .|. shr x (fromInteger c)

-- internal functions

-- half of G
mix : Salt -> Bits32 -> Integer -> Integer -> Salt
mix (S a b c d) w rd rb =
  let a' = a + w + b
      d' = (d .^. a') `rotr` rd
      c' = c + d'
      b' = (b .^. c') `rotr` rb
   in S a' b' c' d'

-- half of a bunch of G that don't use the same data
group : State -> Salt -> Integer -> Integer -> State
group (St a b c d  e f g h  i j k l  m n o p) (S w x y z) rd rb =
  let (S a' b' c' d') = mix (S a b c d) w rd rb
      (S e' f' g' h') = mix (S e f g h) x rd rb
      (S i' j' k' l') = mix (S i j k l) y rd rb
      (S m' n' o' p') = mix (S m n o p) z rd rb
   in St a' b' c' d'  e' f' g' h'  i' j' k' l'  m' n' o' p'

-- A single round of the hash function
r : State -> State -> State
r (St m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 mA mB mC mD mE mF)
      (St v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 vA vB vC vD vE vF) =
  let (St w0 w4 w8 wC w1 w5 w9 wD w2 w6 wA wE w3 w7 wB wF) =
        group (St v0 v4 v8 vC v1 v5 v9 vD v2 v6 vA vE v3 v7 vB vF) (S m0 m2 m4 m6) 16 12
      (St x0 x4 x8 xC x1 x5 x9 xD x2 x6 xA xE x3 x7 xB xF) =
        group (St w0 w4 w8 wC w1 w5 w9 wD w2 w6 wA wE w3 w7 wB wF) (S m1 m3 m5 m7) 8 7
      (St y0 y5 yA yF y1 y6 yB yC y2 y7 y8 yD y3 y4 y9 yE) =
        group (St x0 x5 xA xF x1 x6 xB xC x2 x7 x8 xD x3 x4 x9 xE) (S m8 mA mC mE) 16 12
      (St z0 z5 zA zF z1 z6 zB zC z2 z7 z8 zD z3 z4 z9 zE) =
        group (St y0 y5 yA yF y1 y6 yB yC y2 y7 y8 yD y3 y4 y9 yE) (S m9 mB mD mF) 8 7
   in (St z0 z1 z2 z3 z4 z5 z6 z7 z8 z9 zA zB zC zD zE zF)

||| hash two hashes into one
export
hash : Hash -> Hash -> Salt -> Hash
hash (H m0 m1 m2 m3 m4 m5 m6 m7) (H m8 m9 mA mB mC mD mE mF) (S s0 s1 s2 s3) =
  let v0 = 0x6b08e647 -- 0x6a09e667 .^. 0x01010020, no key and digest length of 32 bytes
      v1 = 0xbb67ae85 -- no leaf length
      v2 = 0x3c6ef372 -- no node offset
      v3 = 0xa54ff53a -- no node offset, no node depth, no inner length
      v4 = 0x510e527f .^. s0
      v5 = 0x9b05688c .^. s1
      v6 = 0x1f83d9ab .^. s2
      v7 = 0x5be0cd19 .^. s3
      v8 = 0x6a09e667
      v9 = 0xbb67ae85
      vA = 0x3c6ef372
      vB = 0xa54ff53a
      vC = 0x510e523f -- 0x510e527f .^. 0x40,
      vD = 0x9b05688c
      vE = 0xe07c2654 -- ~0x1f83d9ab,
      vF = 0x5be0cd19
      (St w0 w1 w2 w3 w4 w5 w6 w7 w8 w9 wA wB wC wD wE wF) =
        (St v0 v1 v2 v3 v4 v5 v6 v7 v8 v9 vA vB vC vD vE vF) |>
        r (St m0 m1 m2 m3 m4 m5 m6 m7 m8 m9 mA mB mC mD mE mF) |>
        r (St mE mA m4 m8 m9 mF mD m6 m1 mC m0 m2 mB m7 m5 m3) |>
        r (St mB m8 mC m0 m5 m2 mF mD mA mE m3 m6 m7 m1 m9 m4) |>
        r (St m7 m9 m3 m1 mD mC mB mE m2 m6 m5 mA m4 m0 mF m8) |>
        r (St m9 m0 m5 m7 m2 m4 mA mF mE m1 mB mC m6 m8 m3 mD) |>
        r (St m2 mC m6 mA m0 mB m8 m3 m4 mD m7 m5 mF mE m1 m9) |>
        r (St mC m5 m1 mF mE mD m4 mA m0 m7 m6 m3 m9 m2 m8 mB) |>
        r (St mD mB m7 mE mC m1 m3 m9 m5 m0 mF m4 m8 m6 m2 mA) |>
        r (St m6 mF mE m9 mB m3 m0 m8 mC m2 mD m7 m1 m4 mA m5) |>
        r (St mA m2 m8 m4 m7 m6 m1 m5 mF mB m9 mE m3 mC mD m0)
   in H (v0 .^. w0 .^. w8)
        (v1 .^. w1 .^. w9)
        (v2 .^. w2 .^. wA)
        (v3 .^. w3 .^. wB)
        (v4 .^. w4 .^. wC)
        (v5 .^. w5 .^. wD)
        (v6 .^. w6 .^. wE)
        (v7 .^. w7 .^. wF)

-- support functions

byteSwap : Bits32 -> Bits32
byteSwap x =
  shr x 24 .|. (shr x 8 .&. 0x0000ff00) .|. (shl x 8 .&. 0x00ff0000) .|. shl x 24

baseDigit : Bits32 -> Char
baseDigit 0 = '0'
baseDigit 1 = '1'
baseDigit 2 = '2'
baseDigit 3 = '3'
baseDigit 4 = '4'
baseDigit 5 = '5'
baseDigit 6 = '6'
baseDigit 7 = '7'
baseDigit 8 = '8'
baseDigit 9 = '9'
baseDigit 10 = 'a'
baseDigit 11 = 'b'
baseDigit 12 = 'c'
baseDigit 13 = 'd'
baseDigit 14 = 'e'
baseDigit 15 = 'f'
baseDigit _ = '_'

digitBase : Char -> Bits32
digitBase '0' = 0
digitBase '1' = 1
digitBase '2' = 2
digitBase '3' = 3
digitBase '4' = 4
digitBase '5' = 5
digitBase '6' = 6
digitBase '7' = 7
digitBase '8' = 8
digitBase '9' = 9
digitBase 'a' = 10
digitBase 'b' = 11
digitBase 'c' = 12
digitBase 'd' = 13
digitBase 'e' = 14
digitBase 'f' = 15
digitBase _ = 0

hexes : Bits32 -> List Bits32
hexes n = map (\c => shr n (4*c) .&. 0xf) $ map fromInteger [7..0]

hexShow : Bits32 -> String
hexShow n = pack $ map baseDigit (hexes n)

hexRead : Vect 8 Char -> Bits32
hexRead v = foldl1 (\a,x => 16*a + x) $ map digitBase v

makeVect : (n:Nat) -> List a -> Maybe (Vect n a)
makeVect Z _ = Just []
makeVect (S n) [] = Nothing
makeVect (S n) (x::xs) = map (x::) (makeVect n xs)

divide : {n:Nat} -> {m:Nat} -> Vect (m*n) a -> Vect n (Vect m a)
divide {n=Z} xs = []
divide {n=S k} {m} xs =
  let xs' : Vect (m + (m*k)) a = rewrite sym $ multRightSuccPlus m k in xs
   in (take m xs') :: (divide $ drop m xs')

-- public functions

||| convert from a hash to a vector
export
toVect : Hash -> Vect 8 Bits32
toVect (H a b c d e f g h) = [a,b,c,d,e,f,g,h]

||| convert back from vector to Hash
export
fromVect : Vect 8 Bits32 -> Hash
fromVect [a,b,c,d,e,f,g,h] = H a b c d e f g h

||| convert from a hash to a hexdecimal string
export
toHex : Hash -> String
toHex h = concat $ map (hexShow . byteSwap) (toVect h)

||| try to convert from a hexdecimal string to a hash
export
fromHex : String -> Maybe Hash
fromHex str = map (fromVect . map (byteSwap . hexRead) . divide) $ makeVect 64 (unpack str)

export
Show Hash where
  show h = toHex h

export
Eq Hash where
  x == y = (toVect x) == (toVect y)

export
Ord Hash where
  compare x y = compare (toVect x) (toVect y)

