# treehash

Hashes designed for binary trees and other directed acyclic graphs.
Used properly, identical graphs will hash to the same value,
even if some parts of the graph are known only by their hash.

## blake2s1

Currently this consists of C, Haskell, and Javascript implementations of blake2s1,
BLAKE2s specialized to take a single input block, which is two of it's outputs.

Differentiation of node types is typically done with the salt,
which can also be used to incorporate small amounts of data.

### C

The C implementation may be found in `blake2s1.c` and `blake2s1.h`.

The prototype are straight-forward:

* `void blake2s1(const uint32_t m[16], const uint32_t salt[4], uint32_t out[8]);`

  Hash 16 32-bit words in `m` and 4 words of salt/personalization, into 8 words of hash in `out`.
  It is safe for `out` to overlap with `m`.

* `void blake2s1_hex(const uint32_t hash[8], char out[64]);`

  Render 8 words of hash as hexadecimal in `out`.

### Haskell

The Haskell implementation may be found in `Blake2s1.hs`.

* `Hash` is the type of a Hash
* `zero` is the empty hash
* `Salt` is a 4-tuple of `Word32`s
* `hash :: Hash -> Hash -> Salt -> Hash` hashes to `Hash`es and a `Salt` into a new `Hash`
* `toHex :: Hash -> String` makes a hex string
* `fromHex :: String -> Maybe Hash` reads a hex string
* `toList :: Hash -> [Word32]` breaks out the `Word32`s of a `Hash` into a list
* `fromList :: [Word32] -> Maybe Hash` reads sufficient `Word32`s into a `Hash`

An example to hash the structure of two trees.

```haskell
data Tree t = Branch (Tree t) (Tree t) | Leaf t
structuralHash :: Tree t -> Hash
structuralHash (Leaf _) = hash zero zero (0,0,0,1)
structuralHash (Branch left right) = hash (structuralHash left) (structuralHash right) (0,0,0,0)
```

### Javascript

A javascript implementation may be found in `blake2s1.js`.

* `hash = blake2s1.hash(data, salt, hash)`

  Hash 16 32-bit words of `data` and 4 words of salt/personalization into 8 words in `hash`, return hash.

* `words = blake2s1.fromBytes(bytes)`

  Pack an array of bytes into an array of 32-bit words that blake2s1 can process.

* `bytes = blake2s1.toBytes(words)`

  Unpack 32-bit words into an array of bytes.

* `hexstring = blake2s1.toHex(words)`

  Render a hexadecimal string suitable for displaying a hash.

For example, the outline of a function to hash a tree:

```js
function hashtree(tree){
	var salt = nodeDataTo4Words(tree);
	if(isleaf(tree)){
		return blake2s1.hash(blake2s1.zero, salt, []);
	} else {
		return blake2s1.hash(hashtree(tree.left).concat(hashtree(tree.right)), salt, []);
	}
}
var hex = blake2s1.toHex(hashtree(theTree));
```

### Idris

The Idris implementation may be found in `Blake2s1.idr`.

Much like the Haskell implementation:

* `Hash` is the type of a Hash
* `zero` is the empty hash
* `Salt` is `S` followed by 4 `Bits32`s
* `hash :: Hash -> Hash -> Salt -> Hash` hashes to `Hash`es and a `Salt` into a new `Hash`
* `toHex :: Hash -> String` makes a hex string
* `fromHex :: String -> Maybe Hash` reads a hex string
* `toVect :: Hash -> Vect 8 Bits32` breaks out the `Bits32`s of a `Hash` into a vector
* `fromVect :: Vect 8 Bits32 -> Hash` converts a vector of `Bits32`s into a `Hash`

As you can see below, the Idris performance is abysmal.

## Performance

Performance may be tested with `make perf` or by loading `perf/index.html`.

|    Processor    | Environment  | Lang    |   Hash   | MH/s | MB/s |
| --------------- | ------------ | ------- | -------- | ---- | ---- |
| i5-3337U 1.8GHz | clang v3.9.1 | C       | blake2s1 | 5.00 | 320  |
| i5-3337U 1.8GHz | node v6.9.5  | js      | blake2s1 | 3.51 | 225  |
| E5-2603 1.6GHz  | clang v3.7.1 | C       | blake2s1 | 3.45 | 220  |
| i5-3337U 1.8GHz | Chromium 61  | js      | blake2s1 | 3.05 | 196  |
| E5-2603 1.6GHz  | node v4.6.0  | js      | blake2s1 | 2.37 | 152  |
| Core 2 1.0GHz   | gcc 7.2.0    | C       | blake2s1 | 2.08 | 132  |
| i5-3337U 1.8GHz | GHC v8.0.2   | Haskell | blake2s1 | 2.03 | 130  |
| E5-2603 1.6GHz  | GHC v8.0.1   | Haskell | blake2s1 | 1.25 |  80  |
| Core 2 1.0GHz   | GHC v8.0.2   | Haskell | blake2s1 | 0.99 |  63  |
| Core 2 1.0GHz   | node v8.11.2 | js      | blake2s1 | 0.53 |  34  |
| i5-3337U 1.8GHz | 1.0 gcc 6.4  | Idris   | blake2s1 | 0.01 | 0.9  |

## Tests

Tests may be run with `make test` or by loading `test/index.html`.

* blake2s1 salt is untested due to lack of support in the `b2sum` utility.

## Unlicenced

Public Domain / [Unlicence](https://unlicence.org/)
