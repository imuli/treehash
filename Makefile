CFLAGS?=-O3 -march=native
CFLAGS+=-Wall
all: test

bin:
	mkdir -p bin

Ctest=$(patsubst %.c,%.c.test,$(wildcard test/*.c))
JStest=$(patsubst %.js,%.js.test,$(wildcard test/*.js))
test: ctest jstest
ctest: $(Ctest)
jstest: $(JStest)

bin/test_c_%: %.c test/%.c %.h | bin
	$(CC) $(CFLAGS) -o $@ $*.c test/$*.c

test/%.c.test: bin/test_c_%
	@echo $* C Test:
	@(cd test;../$<)

test/%.js.test: test/%.js %.js
	@echo $* JS Test:
	@node $<

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
