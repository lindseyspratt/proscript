% A generic benchmark interface

main_test(Test, Functor, Count) :-
    !,
	do_bench(Test, Functor, Count).


do_bench(Test, Functor, Count) :-
    statistics,
	get_cpu_time(T1),
	iterate_bench(Count),
	get_cpu_time(T2),
    statistics,
	Time is T2-T1,
	TimeIt is Time // Count,
	write(TimeIt),
	write(' msec per iter, '),
	write(Count),
	write(' iters, total time : '),
	write(Time),
	write(' msec'),
	nl,
	statistics_max_heap(Heap),
	record_info(Functor, Test,TimeIt,Time,Heap).

record_info(Functor, Test,Iter,Total,Heap) :-
    %atom_concat(Functor, '.pl', File),
    %open(File, write, Stream),
    Goal =.. [Functor,Test,Iter,Total,Heap],
    writeq(Goal), write('.\n').
    %close(Stream).



iterate_bench(Count) :-
	rep(Count, Last),
	ShowResult = Last,
	exec_bench(ShowResult),
	Last = true.


exec_bench(ShowResult) :-
	benchmark(ShowResult),
	!.


rep(1, true):-
	!.

rep(_, false).

rep(N, Last) :-
	N1 is N - 1,
	rep(N1, Last).

/*
 * this file should define:
 * get_count/1
 * get_cpu_time/1
 * and launch q/0
 */

:- include(hook).





