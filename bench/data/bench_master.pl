:- module(bench_master, [bench_description/3, bench/5]).

:- use_module(bench_1).
:- use_module(bench_2).
:- use_module(bench_3).
:- use_module(bench_4).
:- use_module(bench_5).

bench_description(1, Description, Date) :-
    bench_1_description(Description, Date).
bench_description(2, Description, Date) :-
    bench_2_description(Description, Date).
bench_description(3, Description, Date) :-
    bench_3_description(Description, Date).
bench_description(4, Description, Date) :-
    bench_4_description(Description, Date).
bench_description(5, Description, Date) :-
    bench_5_description(Description, Date).

bench(1, Test, Iter, Total, Heap) :-
    bench_1(Test, Iter, Total, Heap).
bench(2, Test, Iter, Total, Heap) :-
    bench_2(Test, Iter, Total, Heap).
bench(3, Test, Iter, Total, Heap) :-
    bench_3(Test, Iter, Total, Heap).
bench(4, Test, Iter, Total, Heap) :-
    bench_4(Test, Iter, Total, Heap).
bench(5, Test, Iter, Total, Heap) :-
    bench_5(Test, Iter, Total, Heap).
