:- module(compare_bench,[compare_bench/2, compare_bench/3]).

:- use_module(bench_master).

compare_bench(ID1, ID2) :-
    compare_bench(ID1, ID2, test).

compare_bench(ID1, ID2, Field) :-
    bench_description(ID1, Desc1, Date1),
    bench_description(ID2, Desc2, Date2),
    atom_codes(Desc1Atom, Desc1),
    atom_codes(Desc2Atom, Desc2),
    format('Compare ~w and ~w:~n~w: ~w (~w)~n~w: ~w (~w)~n~n', [ID1, ID2, ID1, Desc1Atom, Date1, ID2, Desc2Atom, Date2]),
    format('Ratios of bench data ~w to ~w, sorted by ~w:~n~nTest~t~15+~tPerIteration~15+~tTotal~8+~tHeap~+~n~n', [ID1, ID2, Field]),
    setof(r(Test,IterRatio, TotalRatio, HeapRatio),
        compare1(ID1, ID2, Test,IterRatio, TotalRatio, HeapRatio),
        Comparisons),
    sort_comparisons(Comparisons, Field, SortedComparisons),
    forall(member(r(Test,IterRatio, TotalRatio, HeapRatio), SortedComparisons),
        compare_bench(Test, IterRatio, TotalRatio, HeapRatio)).

compare1(ID1, ID2, Test,IterRatio, TotalRatio, HeapRatio) :-
    bench(ID1, Test, Iter1, Total1, Heap1),
    bench(ID2, Test, Iter2, Total2, Heap2),
    IterRatio is Iter1 / Iter2,
    TotalRatio is Total1 / Total2,
    HeapRatio is Heap1 / Heap2.

sort_comparisons(Comparisons, Field, SortedComparisons) :-
    tag_by_field(Comparisons, Field, TaggedComparisons),
    sort(TaggedComparisons, SortedTaggedComparisons),
    strip(SortedTaggedComparisons, SortedComparisons).

% tag_by_field(Comparisons, Field, TaggedComparisons)
tag_by_field([], _Field, []).
tag_by_field([H|T], Field, [Tag-H|TT]) :-
    tag_by_field1(Field, H, Tag),
    tag_by_field(T, Field, TT).

tag_by_field1(test, r(Test, _IterRatio, _TotalRatio, _HeapRatio), Test).
tag_by_field1(iter, r(_Test, IterRatio, _TotalRatio, _HeapRatio), IterRatio).
tag_by_field1(total, r(_Test, _IterRatio, TotalRatio, _HeapRatio), TotalRatio).
tag_by_field1(heap, r(_Test, _IterRatio, _TotalRatio, HeapRatio), HeapRatio).

strip([], []).
strip([_-H|T], [H|TT]) :-
    strip(T, TT).

compare_bench(Test, IterRatio, TotalRatio, HeapRatio) :-
    format('~w:~t~15+~t~3f~15+~t~3f~8+~t~3f~+~n', [Test, IterRatio, TotalRatio, HeapRatio]).

