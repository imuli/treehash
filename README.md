# treehash

Hashes designed for binary trees.
Used properly, identical binary trees will hash to the same value,
even if some parts of the tree are known only by their hash.

Currently this consists of a Javascript implementation of blake2s1,
BLAKE2s specialized to take a single input block, which is two of it's outputs.

## Javascript

A javascript implementation may be found in `blake2s1.js`.

* `hash = blake2s1.hash(data, salt)` hashes 16 32-bit words of data and 4 words of salt/personalization into 8 words of hash.
* `words = blake2s1.fromBytes(bytes)` reads byte-sized numbers into 32-bit words that blake2s1 can process.
* `bytes = blake2s1.toBytes(words)` does the opposite, unpacks 32-bit words into an array of bytes.
* `hexstring = blake2s1.toHex(words)` renders a hexdecimal string suitable for displaying a hash.

For example, the outline of a function to hash a tree:

```js
function hashtree(tree){
	if(isleaf(tree)) return leafTo8Words(tree);
	var salt = nodeDataTo4Words(tree);
	return blake2s1.hash(hashtree(tree.left).concat(hashtree(tree.right)), salt);
}
var hex = blake2s1.toHex(hashtree(theTree));
```

### Performance

See perf/index.html.

```
Processor	Environment	MH/s	MB/s
i5-3337U 1.8GHz	node v6.9.5	1.58	101
i5-3337U 1.8GHz	Chromium 61	1.50	96
```

### Tests

TODO (hand checked outputs against reference version of b2sum)

## Unlicenced

Public Domain / [Unlicence](https://unlicence.org/)
