# treehash

Hashes designed for binary trees.
Used properly, identical binary trees will hash to the same value,
even if some parts of the tree are known only by their hash.

Currently this consists of a Javascript implementation of blake2s1,
BLAKE2s specialized to take a single input block, which is two of it's outputs.

## Javascript

A javascript implementation may be found in `blake2s1.js`.

* `hash = blake2s1.hash(data, salt, hash)` hashes 16 32-bit words of data and 4 words of salt/personalization into 8 words of hash, and returns the hash.
* `words = blake2s1.fromBytes(bytes)` reads byte-sized numbers into 32-bit words that blake2s1 can process.
* `bytes = blake2s1.toBytes(words)` does the opposite, unpacks 32-bit words into an array of bytes.
* `hexstring = blake2s1.toHex(words)` renders a hexdecimal string suitable for displaying a hash.

For example, the outline of a function to hash a tree:

```js
function hashtree(tree){
	if(isleaf(tree)) return leafTo8Words(tree);
	var salt = nodeDataTo4Words(tree);
	return blake2s1.hash(hashtree(tree.left).concat(hashtree(tree.right)), salt, []);
}
var hex = blake2s1.toHex(hashtree(theTree));
```

### Performance

Performance may be tested with `make perf` or by loading `perf/index.html`.

|    Processor    | Environment  | Lang |   Hash   | MH/s | MB/s |
| --------------- | ------------ | ---- | -------- | ---- | ---- |
| i5-3337U 1.8GHz | clang v3.9.1 | C    | blake2s1 | 5.00 | 320  |
| i5-3337U 1.8GHz | node v6.9.5  | js   | blake2s1 | 3.51 | 225  |
| E5-2603 1.6GHz  | clang v3.7.1 | C    | blake2s1 | 3.45 | 220  |
| i5-3337U 1.8GHz | Chromium 61  | js   | blake2s1 | 3.05 | 196  |
| E5-2603 1.6GHz  | node v4.6.0  | js   | blake2s1 | 2.37 | 152  |

### Tests

Tests may be run with `make test` or by loading `test/index.html`.

* blake2s1 salt is untested due to lack of support in the `b2sum` utility.

## Unlicenced

Public Domain / [Unlicence](https://unlicence.org/)
