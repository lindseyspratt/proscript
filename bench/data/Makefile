SWIPL=/usr/local/bin/swipl --traditional

all: compare

compare:
		$(SWIPL) -q -f compare_bench.pl -g "compare_bench(1, 2, iter), halt"
