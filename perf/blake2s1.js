if(typeof blake2s1 == 'undefined') blake2s1 = require('../blake2s1.js');

var h = [ 0x0,0x0,0x0,0x0, 0x0,0x0,0x0,0x0 ];
var n = 1000000;
var start = new Date();
for(var i = 0; i < n; i++){
	h = blake2s1.hash(h.concat(h), [0,0,0,0]);
}
var end = new Date();

var khps = n/(end.getTime()-start.getTime());
var speed = khps + " KH/s ("  + khps * 64 / 1000 + " MB/s)";
var hash = blake2s1.toHex(h);

if(typeof document == 'undefined'){
	console.log(speed);
	console.log(hash);
} else {
	var s = speed + "\n" + hash;
	document.getElementById("blake2s1").innerText = s;
}

