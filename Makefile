CFLAGS?=-O3 -march=native
CFLAGS+=-Wall

GHC?=ghc
GHCFLAGS?=-O
GHCFLAGS+=-Wall

IDRIS?=idris
IDRISFLAGS?=-O3 --cpu native

all: test

bin:
	mkdir -p bin

Ctest=$(patsubst %.c,%.c.test,$(wildcard test/*.c))
JStest=$(patsubst %.js,%.js.test,$(wildcard test/*.js))
HStest=$(patsubst %.hs,%.hs.test,$(wildcard test/*.hs))
IDRtest=$(patsubst %.idr,%.idr.test,$(wildcard test/*.idr))
test: ctest jstest hstest idrtest
ctest: $(Ctest)
jstest: $(JStest)
hstest: $(HStest)
idrtest: $(IDRtest)

bin/test_c_%: %.c test/%.c %.h | bin
	$(CC) $(CFLAGS) -o $@ $*.c test/$*.c

bin/test_haskell_%: %.hs test/%.hs | bin
	$(GHC) $(GHCFLAGS) -o $@ $*.hs test/$*.hs

bin/test_idris_%: %.idr test/%.idr | bin
	$(IDRIS) $(IDRISFLAGS) -o $@ $*.idr test/$*.idr

test/%_vectors.jsonp: test/generate_test_vectors.sh
	(cd test; ./generate_test_vectors.sh)

test/%.c.test: bin/test_c_% test/%_vectors.jsonp
	@echo $* C Test:
	@./$<

test/%.js.test: test/%.js %.js test/%_vectors.jsonp
	@echo $* JS Test:
	@node $<

test/%.hs.test: bin/test_haskell_% %.hs test/%_vectors.jsonp
	@echo $* Haskell Test:
	@./$<

test/%.idr.test: bin/test_idris_% %.idr test/%_vectors.jsonp
	@echo $* Idris Test:
	@./$<

Cperf=$(patsubst %.c,%.c.perf,$(wildcard perf/*.c))
JSperf=$(patsubst %.js,%.js.perf,$(wildcard perf/*.js))
HSperf=$(patsubst %.hs,%.hs.perf,$(wildcard perf/*.hs))
IDRperf=$(patsubst %.idr,%.idr.perf,$(wildcard perf/*.idr))
perf: cperf jsperf
cperf: $(Cperf)
jsperf: $(JSperf)
hsperf: $(HSperf)
idrperf: $(IDRperf)

bin/perf_c_%: %.c perf/%.c %.h | bin
	$(CC) $(CFLAGS) -o $@ $*.c perf/$*.c

bin/perf_haskell_%: %.hs perf/%.hs | bin
	$(GHC) $(GHCFLAGS) -o $@ $*.hs perf/$*.hs

bin/perf_idris_%: %.idr perf/%.idr | bin
	$(IDRIS) $(IDRISFLAGS) -o $@ $*.idr perf/$*.idr

perf/%.c.perf: bin/perf_c_%
	@echo $* C Performance:
	@./$<

perf/%.js.perf: perf/%.js %.js
	@echo $* JS Performance:
	@node $<

perf/%.hs.perf: bin/perf_haskell_%
	@echo $* Haskell Performance:
	@./$<

perf/%.idr.perf: bin/perf_idris_%
	@echo $* Idris Performance:
	@./$<

clean:
	rm -rf bin
