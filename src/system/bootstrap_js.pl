:- module(bootstrap_js,
        [append/3, assert/1, reverse/2, reverse/3, save_clausea/1, handle_term_expansion/1,
         include/1, (dynamic)/1, consult_atom/1, ensure_loaded/1, format/2,
         compile_message/1, (??) / 1, (?) / 1, otherwise/0, end_block/2, findall/3, setof/3, bagof/3,
         asserta/1, assertz/1, retract/1,
         unify_with_occurs_check/2, (\=) / 2, (\==) / 2, atomic/1, nonvar/1, number/1,
         open/3, close/1, flush_output/0, stream_property/2, get_char/1, get_code/1, peek_char/1,
         put_char/1, put_code/1, get_byte/1, peek_byte/1, put_byte/1, read_term/2, read/1, read/2,
         write_term/2, write/1, write/2, writeq/2, write_canonical/1, write_canonical/2,
         halt/0,
         callable/1, retractall/1, sort/2, keysort/2, length/2, delete/3,
         call_with_module/2,
         call/1, call/2, call/3, call/4, call/5, call/6, call/7, call/8,
         write_list/2, write_list/3
         ]).

:- use_module(not).

:- meta_predicate((
            call(0), call(1,?), call(2,?, ?), call(3, ?, ?, ?), call(4, ?, ?, ?, ?), call(5, ?, ?, ?, ?, ?), call(6, ?, ?, ?, ?, ?, ?), call(7, ?, ?, ?, ?, ?, ?, ?),
            retractall(0), asserta(0), assertz(0), retract(0),
            findall(?, ^, ?), setof(?, ^, ?), bagof(?, ^, ?)
            )).

module(Name, Exports) :-
        define_current_module(Name, Exports).

use_module(Spec) :-
        define_use_module(Spec).

assert(Term):-
        assertz(Term).

save_clausea(Head:-Body):-
       !,
       Head =.. [Name|Args],
       length(Args, Arity),
       wam_compiler:current_compilation_module(ModuleName),
       wam_compiler:transform_predicate_name(Name, Arity, ModuleName, TransformedName),
       TransformedHead =.. [TransformedName|Args],
       prepend_clause_to_predicate(TransformedName/Arity, TransformedHead, Body).

save_clausea(Fact):-
        !,
        Fact =.. [Name|Args],
        length(Args, Arity),
        wam_compiler:current_compilation_module(ModuleName),
        wam_compiler:transform_predicate_name(Name, Arity, ModuleName, TransformedName),
        TransformedFact =.. [TransformedName|Args],
        prepend_clause_to_predicate(TransformedName/Arity, TransformedFact, true).

handle_term_expansion(Head) :- true.

include(_).

call([H|T]) :-
        call(wam_compiler:consult([H|T])).

call(Goal):-
        call_with_module(user, Goal).

call_with_module(Module, Goal) :-
        term_variables(Goal, Vars),
        % Compile this into a predicate, but do not actually declare it anywhere.
        % The functor is therefore irrelevant.
        (Module = user
          -> wam_compiler:compile_clause_2(query(Vars):-Goal)
        ;
         push_current_compilation_module(Module, call),
         wam_compiler:compile_clause_2(query(Vars):-Goal),
         pop_current_compilation_module(Module, call)
        ),
        !,
        % Now we need to call our anonymous predicate. $jmp does the trick here
        '$jmp'(Vars),
        % But jmp must never be the last thing in a body, because foreign execute() will cause P <- CP after it succeeds
        % and it is safer to not mess with CP inside $jmp.
        true.


dynamic(Name/Arity) :-
        !,
        define_dynamic_predicate(Name/Arity).
dynamic([]).
dynamic([H|T]) :-
        !,
        dynamic(H),
        dynamic(T).
dynamic((A,B)) :-
        dynamic(A),
        dynamic(B).

consult_atom(Atom):-
        % FIXME: Needs to abolish the old clauses!
        wam_compiler:compile_atom(Atom).

ensure_loaded(URL) :-
  wam_compiler:canonical_source(URL, CanonicalURL),
  (
  wam_compiler:'$loaded'(CanonicalURL)
    -> true
  ;
  wam_compiler:consult([CanonicalURL])
  ).

format(Format, Args):-
        current_output(Stream), format(Stream, Format, Args).

%compile_message(X):-writeln(X).
compile_message(_).

??(Goal):-
        setup_call_catcher_cleanup(format('CALL ~q~n', [Goal]),
                                   call(Goal),
                                   Catcher,
                                   ( Catcher == fail ->
                                       format('FAIL ~q~n', [Goal])
                                   ; Catcher == exit ->
                                       format('EXIT ~q~n', [Goal])
                                   ; Catcher == ! ->
                                       format('CUT  ~q~n', [Goal])
                                   ; Catcher = error(Error)->
                                       format('ERROR ~q ~p~n', [Goal, Error])
                                   )),
        ( var(Catcher)->
            format('PEND ~q~n', [Goal])
        ; otherwise->
            true
        ).

?(Goal):-
        functor(Goal, Functor, Arity),
        setup_call_catcher_cleanup(format('CALL ~q~n', [Functor/Arity]),
                                   call(Goal),
                                   Catcher,
                                   ( Catcher == fail ->
                                       format('FAIL ~q~n', [Goal])
                                   ; Catcher == exit ->
                                       format('EXIT ~q~n', [Functor/Arity])
                                   ; Catcher == ! ->
                                       format('CUT  ~q~n', [Functor/Arity])
                                   ; Catcher = error(Error)->
                                       format('ERROR ~q ~p~n', [Functor/Arity, Error])
                                   )),
        ( var(Catcher)->
            format('PEND ~q~n', [Functor/Arity])
        ; otherwise->
            true
        ).

otherwise.

% Exceptions are implement as per Bart Demoen's 1989 paper
% http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.57.4354&rep=rep1&type=pdf
/* This is now compiled directly to save on having call/1 in the code
catch(Goal, Catcher, Recovery):-
        get_current_block(Block),
        catch_1(Goal, Catcher, Recovery, Block).
catch_1(Goal, Catcher, Recovery, Block):-
        install_new_block(NewBlock),
        call(Goal),
        end_block(Block, NewBlock).
catch_1(Goal, Catcher, Recovery, Block):-
        reset_block(Block),
        get_exception(Ball),
        catch_2(Ball, Catcher, Recovery).

catch_2(Ball, Ball, Recovery):-
        clear_exception,
        !,
        call(Recovery).

catch_2(_, _, _):-
        unwind_stack.
*/

end_block(Block, NewBlock):-
        clean_up_block(NewBlock),
        reset_block(Block).

end_block(_, NewBlock):-
        reset_block(NewBlock),
        fail.


% setof/3, bagof/3, findall/3 and findall/4 as implemented by Richard O'Keefe and David Warren.
% http://www.j-paine.org/prolog/tools/files/setof.pl
% free_variables/4 is defined in not.pl, also from dec10 prolog tools.


findall(Template, Generator, List) :-
	save_instances(-Template, Generator),
	list_instances([], List).

findall(Template, Generator, SoFar, List) :-
	save_instances(-Template, Generator),
	list_instances(SoFar, List).

set_of(Template, Filter, Set) :-
	bag_of(Template, Filter, Bag),
	sort(Bag, Set).

bag_of(Template, Generator, Bag) :-
	free_variables(Generator, Template, [], Vars),
	Vars \== [],
	!,
	Key =.. [.|Vars],
	functor(Key, ., N),
	save_instances(Key-Template, Generator),
	list_instances(Key, N, [], OmniumGatherum),
	keysort(OmniumGatherum, Gamut), !,
	concordant_subset(Gamut, Key, Answer),
	Bag = Answer.
bag_of(Template, Generator, Bag) :-
	save_instances(-Template, Generator),
	list_instances([], Bag),
	Bag \== [].

save_instances(Template, Generator) :-
	wam_util:existential_variables(Generator, _, Goal),
	recorda(., -, _),
	call(Goal),
	recorda(., Template, _),
	fail.
save_instances(_, _).

list_instances(SoFar, Total) :-
	recorded(., Term, Ref),
	erase(Ref), !,		%   must not backtrack
	list_instances(Term, SoFar, Total).

list_instances(-, SoFar, Total) :- !,
	Total = SoFar.		%   = delayed in case Total was bound
list_instances(-Template, SoFar, Total) :-
	list_instances([Template|SoFar], Total).

list_instances(Key, NVars, OldBag, NewBag) :-
	recorded(., Term, Ref),
	erase(Ref), !,		%  must not backtrack!
	list_instances(Term, Key, NVars, OldBag, NewBag).

list_instances(-, _, _, AnsBag, AnsBag) :- !.
list_instances(NewKey-Term, Key, NVars, OldBag, NewBag) :-
        replace_key_variables(NVars, Key, NewKey), !,
        list_instances(Key, NVars, [NewKey-Term|OldBag], NewBag).

replace_key_variables(0, _, _) :- !.
replace_key_variables(N, OldKey, NewKey) :-
	arg(N, NewKey, Arg),
	nonvar(Arg), !,
	M is N-1,
	replace_key_variables(M, OldKey, NewKey).
replace_key_variables(N, OldKey, NewKey) :-
	arg(N, OldKey, OldVar),
	arg(N, NewKey, OldVar),
	M is N-1,
	replace_key_variables(M, OldKey, NewKey).


concordant_subset([Key-Val|Rest], Clavis, Answer) :-
	concordant_subset(Rest, Key, List, More),
	concordant_subset(More, Key, [Val|List], Clavis, Answer).

concordant_subset([Key-Val|Rest], Clavis, [Val|List], More) :-
	Key == Clavis,
	!,
	concordant_subset(Rest, Clavis, List, More).
concordant_subset(More, _, [], More).

concordant_subset([],   Key, Subset, Key, Subset) :- !.
concordant_subset(_,    Key, Subset, Key, Subset).
concordant_subset(More, _,   _,   Clavis, Answer) :-
	concordant_subset(More, Clavis, Answer).


% ISO predicates
% 8.2
% =/2 (foreign)
unify_with_occurs_check(A, A):- acyclic_term(A).
\=(A,B):- \+(A=B).

% 8.3 (Complete)
% var/1 (foreign)
% atom/1 (foreign)
% integer/1 (foreign)
% float/1 (foreign)
atomic(X):- (atom(X)-> true ; number(X)).
% compound/1 (foreign)
nonvar(X):- \+var(X).
number(X):- (integer(X)-> true; float(X)).

% 8.4
% @=</2 (foreign)
% ==/2 (foreign)
\==(A,B):- \+(A == B).
% @</2 (foreign)
% @>/2 (foreign)
% @>=/2 (foreign)

% 8.5
% functor/3 (foreign)
% arg/3 (foreign)
% =../2 (foreign)
% copy_term/2 (foreign)

% 8.6: Arithmetic.
% is/2 (foreign)

% 8.7: Arithmetic comparison
% =:=/2 (foreign)
% =\=/2 (foreign)
% (<)/2 (foreign)
% (=<)/2 (foreign)
% (>)/2 (foreign)
% (>=)/2 (foreign)

% 8.8 
% clause/2 (foreign)
% current_predicate/1 (foreign)

% 8.9
asserta(Term):- wam_compiler:compile_clause_2(Term), save_clausea(Term).
assertz(Term):- wam_compiler:compile_clause_2(Term), wam_compiler:save_clause(Term).
retract(Head:-Body):- !, retract_clause(Head, Body).
retract(Fact):- !, retract_clause(Fact, true).
% abolish/1 (foreign)

% 8.10
% findall/3 (Implemented above)
setof(A,B,C):- set_of(A,B,C).
bagof(A,B,C):- bag_of(A,B,C).

% 8.11 streams
% current_input/1 (foreign)
% current_output/1 (foreign)
% set_input/1 (foreign)
% set_output/1 (foreign)
open(Resource, Mode, Stream):- open(Resource, Mode, Stream, []).
%open(_,_,_,_):- throw(no_files_in_javascript). % defined as foreign predicate for nodejs, undefined for browser.
% close/2 (foreign)
close(Stream):- close(Stream, []).
flush_output:- current_output(S), flush_output(S).
% flush_output/1 (foreign)
stream_property(Stream, Property):- var(Stream), !, current_stream(Stream), stream_property_1(Stream, Property).
stream_property(Stream, Property):- stream_property_1(Stream, Property).
at_end_of_stream:- current_output(S), at_end_of_stream(S).
% at_end_of_stream/1 (foreign)
% set_stream_position/2 (foreign)

% 8.12 char IO. 
% get_char/2 (foreign)
get_char(C):- current_input(S), get_char(S, C).
% get_code/2 (foreign)
get_code(C):- current_input(S), get_code(S, C).
% peek_char/2 (foreign)
peek_char(C):- current_input(S), peek_char(S, C).
% peek_code/2 (foreign)
peek_code(C):- current_input(S), peek_code(S, C).
% put_char/2 (foreign)
put_char(C):- current_output(S), put_char(S, C).
% put_code/2 (foreign)
put_code(C):- current_output(S), put_code(S, C).

% 8.13
% get_byte/2 (foreign)
get_byte(B):- current_input(S), get_byte(S, B).
% peek_byte/2 (foreign)
peek_byte(B):- current_input(S), peek_byte(S, B).
% put_byte/2 (foreign)
put_byte(B):- current_output(S), put_byte(S, B).

% 8.14 Term IO. 
% read_term/3 (foreign)
read_term(Term, Options):- current_input(S), read_term(S, Term, Options).
read(Term):-current_input(S), read(S, Term, []).
read(Stream, Term):- read_term(Stream, Term, []).
% write_term/3 (foreign)
write_term(Term, Options):- current_output(Stream), write_term(Stream, Term, Options).
write(Term):- current_output(S), write_term(S, Term, [quoted(false), ignore_ops(false), numbervars(true)]).
write(Stream, Term):- write_term(Stream, Term, [quoted(false), ignore_ops(false), numbervars(true)]).
writeq(Term):- current_output(Stream), write_term(Stream, Term, [quoted(true), ignore_ops(false), numbervars(true)]).
writeq(Stream, Term):- write_term(Stream, Term, [quoted(true), ignore_ops(false), numbervars(true)]).
write_canonical(Term):- current_output(Stream), write_term(Stream, Term, [quoted(true), ignore_ops(true), numbervars(false)]).
write_canonical(Stream, Term):- write_term(Stream, Term, [quoted(true), ignore_ops(true), numbervars(false)]).
% op/3 (foreign)
% current_op/3 (foreign)
% char_conversion/2 (foreign)
% current_char_conversion/2 (foreign)

% 8.15
% (\+)/1 (foreign)
% once/1 (foreign)
% repeat/0 (foreign)

% 8.16
% atom_length/2 (foreign)
% atom_concat/3 (foreign)
% sub_atom/5 (foreign)
% char_code/2 (foreign)
% atom_chars/2 (foreign)
% atom_codes/2 (foreign)
% number_codes/2 (foreign)
% number_chars/2 (foreign)

% 8.17
% set_prolog_flag/2 (foreign)
% current_prolog_flag/2 (foreign)
halt:- halt(0).
% halt/1 (foreign).

% Corrigendum
% compare/3 (foreign)
% sort/2 (above)
% keysort/2 (above)
% ground/1 (foreign)
% call/2-8 Implemented in this file
% false/0 (foreign)
callable(X):- (atom(X) -> true ; compound(X)).
% subsumes_term/2 (foreign)
% acyclic_term/1 (foreign)
% term_variables/2 (foreign)
retractall(Goal):- retract(Goal), fail.
retractall(_).


sort([X|Xs],Ys) :-
        partition(Xs,X,Left,Right),
        sort(Left,Ls),
        sort(Right,Rs),
        append(Ls,[X|Rs],Ys).
sort([],[]).

keysort([Key-X|Xs],Ys) :-
        key_partition(Xs,Key,Left,Right),
        keysort(Left,Ls),
        keysort(Right,Rs),
        append(Ls,[Key-X|Rs],Ys).
keysort([],[]).

partition([X|Xs],Y,Ls,Rs) :-
        X == Y,
        !,
        partition(Xs, Y, Ls, Rs).
partition([X|Xs],Y,[X|Ls],Rs) :-
        X @=< Y,
        partition(Xs,Y,Ls,Rs).

partition([X|Xs],Y,Ls,[X|Rs]) :-
        X @> Y,
        partition(Xs,Y,Ls,Rs).
partition([],_,[],[]).

key_partition([XKey-_|Xs],YKey,Ls,Rs) :-
        XKey == YKey,
        !,
        key_partition(Xs,YKey,Ls,Rs).
key_partition([XKey-X|Xs],YKey,[XKey-X|Ls],Rs) :-
        XKey @=< YKey,
        key_partition(Xs,YKey,Ls,Rs).
key_partition([XKey-X|Xs],YKey,Ls,[XKey-X|Rs]) :-
        XKey @> YKey,
        key_partition(Xs,YKey,Ls,Rs).
key_partition([],_,[],[]).


append([],Ys,Ys).
append([X|Xs],Ys,[X|Zs]) :-
        append(Xs,Ys,Zs).

reverse(List, Reversed) :-
	reverse(List, [], Reversed).

reverse([], Reversed, Reversed).
reverse([Head|Tail], Sofar, Reversed) :-
	reverse(Tail, [Head|Sofar], Reversed).


length(L, N) :-
        length(L, 0, N).

length([], K, K) :- !.
length([_|T], J, K) :-
        N is J + 1,
        length(T, N, K).

%   delete(List, Elem, Residue)
%   is true when List is a list, in which Elem may or may not occur, and
%   Residue is a copy of List with all elements equal to Elem deleted.

delete([], _, []) :- !.
delete([Kill|Tail], Kill, Rest) :- !,
	delete(Tail, Kill, Rest).
delete([Head|Tail], Kill, [Head|Rest]) :- !,
	delete(Tail, Kill, Rest).

call(A, B):-
        A =.. [Functor|Args],
        append(Args, [B], NewArgs),
        AA =.. [Functor|NewArgs],
        call(AA).

call(A, B, C):-
        A =.. [Functor|Args],
        append(Args, [B, C], NewArgs),
        AA =.. [Functor|NewArgs],
        call(AA).


call(A, B, C, D):-
        A =.. [Functor|Args],
        append(Args, [B, C, D], NewArgs),
        AA =.. [Functor|NewArgs],
        call(AA).


call(A, B, C, D, E):-
        A =.. [Functor|Args],
        append(Args, [B, C, D, E], NewArgs),
        AA =.. [Functor|NewArgs],
        call(AA).


call(A, B, C, D, E, F):-
        A =.. [Functor|Args],
        append(Args, [B, C, D, E, F], NewArgs),
        AA =.. [Functor|NewArgs],
        call(AA).


call(A, B, C, D, E, F, G):-
        A =.. [Functor|Args],
        append(Args, [B, C, D, E, F, G], NewArgs),
        AA =.. [Functor|NewArgs],
        call(AA).


call(A, B, C, D, E, F, G, H):-
        A =.. [Functor|Args],
        append(Args, [B, C, D, E, F, G, H], NewArgs),
        AA =.. [Functor|NewArgs],
        call(AA).



write_list(List, Separator) :-
    current_output(Stream),
    write_list(List, Separator, Stream).

write_list([], _, _).
write_list([H|T], Separator, Stream) :-
        write(Stream, H),
        write_list1(T, Separator, Stream).

write_list1([], _, _).
write_list1([H|T], Separator, Stream) :-
        write(Stream, Separator),
        write(Stream, H),
        write_list1(T, Separator, Stream).
