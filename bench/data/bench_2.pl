:- module(bench_2, [bench_2_description/2, bench_2/4]).

bench_2_description("master branch before indexing", '2019-10-22').

% bench_2(test,iter,total,heap).

bench_2(cal,1762,1762,575).
bench_2(chat_parser,1328,1328,447).
bench_2(crypt,54,54,1062).
bench_2(ham,7210,7210,447).
bench_2(meta_qsort ,109,109,6930).
bench_2(nand,220,220,753).
bench_2(nrev,8316,8316,632).
bench_2(poly_10,353,353,113193).
bench_2(qsort,6,6,1575).
bench_2(queens,5125,5125,2167).
bench_2(queensn,14619,14619,1287).
bench_2(query,42,42,447).
bench_2(reducer,325,325,41013).
bench_2(sendmore,491,491,447).
bench_2(zebra,99,99,728).
bench_2(browse,8299,8299,62939).
