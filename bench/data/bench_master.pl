:- module(bench_master, [bench_description/3, bench/5]).

:- use_module(bench_1).
:- use_module(bench_2).

bench_description(1, Description, Date) :-
    bench_1_description(Description, Date).
bench_description(2, Description, Date) :-
    bench_2_description(Description, Date).

bench(1, Test, Iter, Total, Heap) :-
    bench_1(Test, Iter, Total, Heap).
bench(2, Test, Iter, Total, Heap) :-
    bench_2(Test, Iter, Total, Heap).
