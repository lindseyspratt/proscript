:- module(bench_1, [bench_1_description/2, bench_1/4]).

bench_1_description("index branch with indexing", '2019-10-22').

% bench_1(test,iter,total,heap).

bench_1(cal,599,599,575).
bench_1(chat_parser,563,563,447).
bench_1(crypt,16,16,1105).
bench_1(ham,2748,2748,447).
bench_1(meta_qsort,62,62,6930).
bench_1(nand,199,199,753).
bench_1(nrev,2866,2866,632).
bench_1(poly_10,305,305,113193).
bench_1(qsort,4,4,1575).
bench_1(queens,3692,3692,2167).
bench_1(queensn,9405,9405,1287).
bench_1(query,12,12,447).
bench_1(reducer,311,311,41013).
bench_1(sendmore,494,494,447).
bench_1(tak,890,890,318516).
bench_1(zebra,99,99,728).
bench_1(boyer,5309,5309,996200).
bench_1(browse,7695,7695,62939).
