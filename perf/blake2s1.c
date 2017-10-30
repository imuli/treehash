#include <stdint.h>
#include <time.h>
#include <stdio.h>

#include "../blake2s1.h"

static double
time_diff(struct timespec *a, struct timespec *b){
	return (double)(b->tv_sec - a->tv_sec) + (double)(b->tv_nsec - a->tv_nsec)/(1.0e9);
}

int
main(int argc, char **argv){
	uint32_t data[16];
	const uint32_t salt[4] = {0,0,0,0};

	// just to be safe
	for(int i = 0; i < sizeof(data)/sizeof(*data); i++){
		data[i] = 0;
	}

	const int n = 1000000;
	struct timespec start, end;
	clock_gettime(CLOCK_MONOTONIC_RAW, &start);
	for(int i = 0; i < n; i++){
		blake2s1(data, salt, data);
		for(int j = 0; j < 8; j++){
			data[j+8] = data[j];
		}
	}
	clock_gettime(CLOCK_MONOTONIC_RAW, &end);
	double hps = n/time_diff(&start, &end);
	printf("%.3f KH/s (%.3f MB/s)\n", hps/1.0e3, 64 * hps/1.0e6);
	char hex[64];
	blake2s1_hex(data, hex);
	printf("%.*s\n", sizeof(hex), hex);
}

