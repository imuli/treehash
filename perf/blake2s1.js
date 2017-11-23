if(typeof blake2s1 == 'undefined') blake2s1 = require('../blake2s1.js');

var h = blake2s1.zero.slice(8);
var n = 1000000;
var start = new Date();
for(var i = 0; i < n; i++){
	for(var j=0; j < 8; j++) h[8+j] = h[j];
	blake2s1.hash(h, [0,0,0,0], h);
}
var end = new Date();

var khps = n/(end.getTime()-start.getTime());
var speed = khps + " KH/s ("  + khps * 64 / 1000 + " MB/s)";
var hash = blake2s1.toHex(h.slice(0,8));

if(typeof document == 'undefined'){
	console.log(speed);
	console.log(hash);
} else {
	var s = speed + "\n" + hash;
	document.getElementById("blake2s1").innerText = s;
}

