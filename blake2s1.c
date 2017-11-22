#include <stdint.h>

static uint32_t IV[8] = {
	0x6a09e667UL, 0xbb67ae85UL, 0x3c6ef372UL, 0xa54ff53aUL,
	0x510e527fUL, 0x9b05688cUL, 0x1f83d9abUL, 0x5be0cd19UL,
};

static const uint8_t blake2s_sigma[10][16] =
{
  {  0,  1,  2,  3,  4,  5,  6,  7,  8,  9, 10, 11, 12, 13, 14, 15 } ,
  { 14, 10,  4,  8,  9, 15, 13,  6,  1, 12,  0,  2, 11,  7,  5,  3 } ,
  { 11,  8, 12,  0,  5,  2, 15, 13, 10, 14,  3,  6,  7,  1,  9,  4 } ,
  {  7,  9,  3,  1, 13, 12, 11, 14,  2,  6,  5, 10,  4,  0, 15,  8 } ,
  {  9,  0,  5,  7,  2,  4, 10, 15, 14,  1, 11, 12,  6,  8,  3, 13 } ,
  {  2, 12,  6, 10,  0, 11,  8,  3,  4, 13,  7,  5, 15, 14,  1,  9 } ,
  { 12,  5,  1, 15, 14, 13,  4, 10,  0,  7,  6,  3,  9,  2,  8, 11 } ,
  { 13, 11,  7, 14, 12,  1,  3,  9,  5,  0, 15,  4,  8,  6,  2, 10 } ,
  {  6, 15, 14,  9, 11,  3,  0,  8, 12,  2, 13,  7,  1,  4, 10,  5 } ,
  { 10,  2,  8,  4,  7,  6,  1,  5, 15, 11,  9, 14,  3, 12, 13 , 0 } ,
};

static __inline__ uint32_t
rotr32(const uint32_t w, const unsigned c){
  return ( w >> c ) | ( w << ( 32 - c ) );
}

#define G(r,i,a,b,c,d)                      \
    a = a + b + m[blake2s_sigma[r][2*i+0]]; \
    d = rotr32(d ^ a, 16);                  \
    c = c + d;                              \
    b = rotr32(b ^ c, 12);                  \
    a = a + b + m[blake2s_sigma[r][2*i+1]]; \
    d = rotr32(d ^ a, 8);                   \
    c = c + d;                              \
    b = rotr32(b ^ c, 7);

#define ROUND(r)                    \
    G(r,0,v[ 0],v[ 4],v[ 8],v[12]); \
    G(r,1,v[ 1],v[ 5],v[ 9],v[13]); \
    G(r,2,v[ 2],v[ 6],v[10],v[14]); \
    G(r,3,v[ 3],v[ 7],v[11],v[15]); \
    G(r,4,v[ 0],v[ 5],v[10],v[15]); \
    G(r,5,v[ 1],v[ 6],v[11],v[12]); \
    G(r,6,v[ 2],v[ 7],v[ 8],v[13]); \
    G(r,7,v[ 3],v[ 4],v[ 9],v[14]);

// no key and digest length of 32 bytes
#define MASK0 0x01010020UL
// 64 bytes of input
#define MASKc 0x40UL
// final block
#define MASKe 0xffffffffUL

void
blake2s1(const uint32_t m[16], const uint32_t salt[4], uint32_t out[8]){
	uint32_t v[16] = {
		IV[0] ^ MASK0,
		IV[1],
		IV[2],
		IV[3],
		IV[4] ^ salt[0],
		IV[5] ^ salt[1],
		IV[6] ^ salt[2],
		IV[7] ^ salt[3],
		IV[0],
		IV[1],
		IV[2],
		IV[3],
		IV[4] ^ MASKc,
		IV[5],
		IV[6] ^ MASKe,
		IV[7],
	};

	ROUND(0);

	ROUND(1);
	ROUND(2);
	ROUND(3);
	ROUND(4);
	ROUND(5);
	ROUND(6);
	ROUND(7);
	ROUND(8);
	ROUND(9);

//	printf("%08x %08x %08x %08x %08x %08x %08x %08x %08x %08x %08x %08x %08x %08x %08x %08x\n", v[0], v[1], v[2], v[3], v[4], v[5], v[6], v[7], v[8], v[9], v[10], v[11], v[12], v[13], v[14], v[15]);
	out[0] = v[0] ^ v[8]  ^ IV[0] ^ MASK0;
	out[1] = v[1] ^ v[9]  ^ IV[1];
	out[2] = v[2] ^ v[10] ^ IV[2];
	out[3] = v[3] ^ v[11] ^ IV[3];
	out[4] = v[4] ^ v[12] ^ IV[4] ^ salt[0];
	out[5] = v[5] ^ v[13] ^ IV[5] ^ salt[1];
	out[6] = v[6] ^ v[14] ^ IV[6] ^ salt[2];
	out[7] = v[7] ^ v[15] ^ IV[7] ^ salt[3];
}

void
blake2s1_hex(const uint32_t hash[8], char out[64]){
	const char hex[17] = "0123456789abcdef";
	// little endian bytes, big endian nibbles
	for(int i = 0; i < 8; i++){
		out[8*i+0] = hex[(hash[i] >>  4) & 0x0f];
		out[8*i+1] = hex[(hash[i] >>  0) & 0x0f];
		out[8*i+2] = hex[(hash[i] >> 12) & 0x0f];
		out[8*i+3] = hex[(hash[i] >>  8) & 0x0f];
		out[8*i+4] = hex[(hash[i] >> 20) & 0x0f];
		out[8*i+5] = hex[(hash[i] >> 16) & 0x0f];
		out[8*i+6] = hex[(hash[i] >> 28) & 0x0f];
		out[8*i+7] = hex[(hash[i] >> 24) & 0x0f];
	}
}
