CFLAGS?=-O3 -march=native
CFLAGS+=-Wall

GHC?=ghc
GHCFLAGS?=-O
GHCFLAGS+=-Wall

all: test

bin:
	mkdir -p bin

Ctest=$(patsubst %.c,%.c.test,$(wildcard test/*.c))
JStest=$(patsubst %.js,%.js.test,$(wildcard test/*.js))
HStest=$(patsubst %.hs,%.hs.test,$(wildcard test/*.hs))
test: ctest jstest hstest
ctest: $(Ctest)
jstest: $(JStest)
hstest: $(HStest)

bin/test_c_%: %.c test/%.c %.h | bin
	$(CC) $(CFLAGS) -o $@ $*.c test/$*.c

bin/test_haskell_%: %.hs test/%.hs | bin
	$(GHC) $(GHCFLAGS) -o $@ $*.hs test/$*.hs

test/%_vectors.jsonp: test/generate_test_vectors.sh
	(cd test; ./generate_test_vectors.sh)

test/%.c.test: bin/test_c_% test/%_vectors.jsonp
	@echo $* C Test:
	@(cd test;../$<)

test/%.js.test: test/%.js %.js test/%_vectors.jsonp
	@echo $* JS Test:
	@node $<

test/%.hs.test: bin/test_haskell_% %.hs test/%_vectors.jsonp
	@echo $* Haskell Test:
	@./$<

Cperf=$(patsubst %.c,%.c.perf,$(wildcard perf/*.c))
JSperf=$(patsubst %.js,%.js.perf,$(wildcard perf/*.js))
perf: cperf jsperf
cperf: $(Cperf)
jsperf: $(JSperf)

bin/perf_c_%: %.c perf/%.c %.h | bin
	$(CC) $(CFLAGS) -o $@ $*.c perf/$*.c

perf/%.c.perf: bin/perf_c_%
	@echo $* C Performance:
	@./$<

perf/%.js.perf: perf/%.js %.js
	@echo $* JS Performance:
	@node $<

clean:
	rm -rf bin
