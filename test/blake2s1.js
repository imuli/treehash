if(typeof blake2s1 == 'undefined') blake2s1 = require('../blake2s1.js');
if(typeof blake2s1_vectors == 'undefined') blake2s1_vectors = require('./blake2s1_vectors.jsonp');

// start with all zeros
var h = [ 0x0,0x0,0x0,0x0, 0x0,0x0,0x0,0x0 ];
for(var i = 0; i < blake2s1_vectors.length; i++){
	// the next hash is the hash of this concatenated to itself
	blake2s1.hash(h.concat(h), [0,0,0,0], h);

	var hex = blake2s1.toHex(h);
	var output;
	if(hex == blake2s1_vectors[i]){
		output = hex;
	} else {
		output = blake2s1_vectors[i] + " != " + hex;
	}

	if(typeof document == 'undefined'){
		console.log(output);
	} else {
		document.getElementById("blake2s1").innerText += output + "\n";
	}
}

