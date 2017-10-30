#!/bin/sh

[ "$(b2sum -a blake2s </dev/null)" == "69217a3079908094e11121d042354a7c1f55b6482ca1a51e1b250dfd1ed0eef9  -" ] && (
# start with 64 zeros
data=$(head -c 64 /dev/zero | xxd -c 256 -p)
echo 'var blake2s1_vectors = ['
# sadly this can only generate test vectors without salt or personalization
for i in $(seq 1 20); do
	# hash the data
	hash=$(echo $data | xxd -p -r | b2sum -a blake2s | head -c 64) 
	echo '	"'$hash'",'
	# feed back the concatenation of the hashes
	data=$hash$hash
done
echo ']; if(typeof module != "undefined") module.exports = blake2s1_vectors;'
) > blake2s1_vectors.js
