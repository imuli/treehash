#include <stdint.h>
#include <stdio.h>
#include <string.h>

#include "../blake2s1.h"

char *
get_check(FILE *f, char **buf, size_t *n){
	ssize_t s = getline(buf, n, f);
	if(s < 0) return NULL;
	for(char *p = *buf; s > 65; s--, p++){
		if(*p == '"') return p+1;
	}
	return get_check(f, buf, n);
}

int
main(int argc, char **argv){
	uint32_t data[16];
	const uint32_t salt[4] = {0,0,0,0};
	char hex[64], *check;

	char *buf = NULL;
	size_t n = 0;

	FILE *f = fopen("blake2s1_vectors.jsonp", "r");
	if(f == NULL){
		fprintf(stderr, "Must be run in the same directory as blake2s1_vectors.jsonp\n");
		return 1;
	}

	// just to be safe
	for(int i = 0; i < sizeof(data)/sizeof(*data); i++){
		data[i] = 0;
	}

	while((check = get_check(f, &buf, &n))){
		blake2s1(data, salt, data);
		for(int j = 0; j < 8; j++){
			data[j+8] = data[j];
		}

		blake2s1_hex(data, hex);
		if(memcmp(hex, check, sizeof(hex)) == 0){
			printf("%.*s\n", sizeof(hex), hex);
		} else {
			printf("%.*s != %.*s\n", sizeof(hex), check, sizeof(hex), hex);
		}
	}

}
