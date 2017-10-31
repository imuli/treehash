# treehash

Hashes designed for binary trees.
Used properly, identical binary trees will hash to the same value,
even if some parts of the tree are known only by their hash.

## blake2s1

Currently this consists of C and Javascript implementations of blake2s1,
BLAKE2s specialized to take a single input block, which is two of it's outputs.

### C

The C implementation may be found in `blake2s1.c` and `blake2s1.h`.

The prototype are straight-forward:

* `void blake2s1(const uint32_t m[16], const uint32_t salt[16], uint32_t out[8]);`

  Hash 16 32-bit words in `m` and 4 words of salt/personalization, into 8 words of hash in `out`.
  It is safe for `out` to overlap with `m`.

* `void blake2s1_hex(const uint32_t hash[8], char out[64]);`

  Render 8 words of hash as hexdecimal in `out`.

### Javascript

A javascript implementation may be found in `blake2s1.js`.

* `hash = blake2s1.hash(data, salt, hash)`

  Hash 16 32-bit words of `data` and 4 words of salt/personalization into 8 words in `hash`, return hash.

* `words = blake2s1.fromBytes(bytes)`

  Pack an array of bytes into an array of 32-bit words that blake2s1 can process.

* `bytes = blake2s1.toBytes(words)`

  Unpack 32-bit words into an array of bytes.

* `hexstring = blake2s1.toHex(words)`

  Render a hexdecimal string suitable for displaying a hash.

For example, the outline of a function to hash a tree:

```js
function hashtree(tree){
	if(isleaf(tree)) return leafTo8Words(tree);
	var salt = nodeDataTo4Words(tree);
	return blake2s1.hash(hashtree(tree.left).concat(hashtree(tree.right)), salt, []);
}
var hex = blake2s1.toHex(hashtree(theTree));
```

## Performance

Performance may be tested with `make perf` or by loading `perf/index.html`.

|    Processor    | Environment  | Lang |   Hash   | MH/s | MB/s |
| --------------- | ------------ | ---- | -------- | ---- | ---- |
| i5-3337U 1.8GHz | clang v3.9.1 | C    | blake2s1 | 5.00 | 320  |
| i5-3337U 1.8GHz | node v6.9.5  | js   | blake2s1 | 3.51 | 225  |
| E5-2603 1.6GHz  | clang v3.7.1 | C    | blake2s1 | 3.45 | 220  |
| i5-3337U 1.8GHz | Chromium 61  | js   | blake2s1 | 3.05 | 196  |
| E5-2603 1.6GHz  | node v4.6.0  | js   | blake2s1 | 2.37 | 152  |

## Tests

Tests may be run with `make test` or by loading `test/index.html`.

* blake2s1 salt is untested due to lack of support in the `b2sum` utility.

## Unlicenced

Public Domain / [Unlicence](https://unlicence.org/)
