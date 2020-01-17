:- module(bench_3, [bench_3_description/2, bench_3/4]).

bench_3_description("macros in wam, full macro unify", 2020-01-16).

% bench_3(test,iter,total,heap).
bench_3(cal,680,2042,50783).
bench_3(chat_parser,505,1517,1897).
bench_3(crypt,13,41,1391).
bench_3(ham,2637,7913,1271).
bench_3(meta_qsort,53,160,7216).
bench_3(nand,158,475,9523).
bench_3(nrev,2744,8232,11780).
bench_3(poly_10,446,1338,113479).
bench_3(qsort,3,11,1861).
bench_3(queens,3806,11420,2453).
bench_3(queensn,9826,29480,1573).
bench_3(query,4,12,733).
bench_3(reducer,267,802,41299).
bench_3(sendmore,482,1448,979).
bench_3(tak,666,1998,318802).
bench_3(zebra,99,297,1014).
bench_3(boyer,5294,15883,996486).
bench_3(browse,6905,20716,63225).
