/*----------------------------------------------------------
 This file is intended for use when bootstrapping only.
 It should not be loaded into the generated compiler.
 Consequently the following predicates must be implemented
 by the target system:
   * lookup_atom(+Atom, -AtomIndex)
   * lookup_float(+Float, -FloatIndex)
   * lookup_functor(+FunctorName, +FunctorArity, -FunctorIndex)
      * Note that FunctorName is an atom, not an index
   * add_clause_to_predicate(Name/Arity, +Head, +Body)
   * emit_code(+Address, +Code)
---------------------------------------------------------- */

:-dynamic(ftable/2). % functors
:-dynamic(fltable/2). % floats
:-dynamic(atable/2). % atoms
:-dynamic(clause_table/3). % clauses
:-dynamic(fptable/2). % foreign predicates
:-dynamic(ctable/2). % code
:-dynamic(itable/1). % initialization directive predicates
:-dynamic(stable/1). % system directive predicates
:-dynamic(dtable/1). % dynamic predicates

lookup_functor(Functor, Arity, N):-
        lookup_atom(Functor, F),
        ( ftable(F/Arity, N)->
            true
        ; otherwise->
            flag(ftable, N, N+1),
            assert(ftable(F/Arity, N))
        ).

lookup_dynamic_functor(Functor, Arity, N):-
        lookup_functor(Functor, Arity, N),
        ( dtable(N)->
            true
        ; otherwise->
            assert(dtable(N))
        ).

lookup_atom([], 0):- !.
lookup_atom(Atom, N):-
        ( atable(Atom, N)->
            true
        ; otherwise->
            flag(atable, N, N+1),
            assert(atable(Atom, N))
        ).

lookup_float(Float, N):-
        ( fltable(Float, N)->
            true
        ; otherwise->
            flag(fltable, N, N+1),
            assert(fltable(Float, N))
        ).

generate_system_goal(Init) :-
        flag(stable, N, N+1),
        number_codes(N, NCs),
        append("$sys_", NCs, ICs),
        atom_codes(Init, ICs),
        lookup_functor(Init, 0, FN),
        assert(stable(FN)).

generate_initialization_goal(Init) :-
        flag(itable, N, N+1),
        number_codes(N, NCs),
        append("$init_", NCs, ICs),
        atom_codes(Init, ICs),
        lookup_functor(Init, 0, FN),
        assert(itable(FN)).

emit_code(N, Code):-
        assert(ctable(N, Code)).

% dynamic implies public. In proscript, public also implies dynamic.
% the is_public flag will be set to true in the saved state
% for predicate Name/Arity.

define_dynamic_predicate(Name/Arity) :-
        lookup_dynamic_functor(Name, Arity, Predicate),
        (clause_table(Predicate, _, _)
          -> true
        ;
         assertz(clause_table(Predicate, 0, []))
        ).

add_clause_to_predicate(Name/Arity, _, _):-
        setof(N-Code, T^(ctable(T, Code), N is T /\ 0x7fffffff), SortedCodes),
        findall(Code, member(_-Code, SortedCodes), Codes),
        lookup_functor(Name, Arity, Predicate),
        ( retract(clause_table(Predicate, I, [254,0|PreviousCodes]))->
            % If there is a NOP clause, then we have only one clause. Make it try_me_else, then add our new one as trust_me.
            II is I+1,
            assertz(clause_table(Predicate, I, [28, II|PreviousCodes])),
            assertz(clause_table(Predicate, II, [30, 0|Codes]))
        ; retract(clause_table(Predicate, I, [30,0|PreviousCodes]))->
            II is I+1,
            % If we have a trust_me, then make it retry_me_else (since there must have been another clause to try first, if we then have to trust_me)
            assertz(clause_table(Predicate, I, [29, II|PreviousCodes])),
            assertz(clause_table(Predicate, II, [30, 0|Codes]))
        ; retract(clause_table(Predicate, 0, []))->
            % Predicate defined with no clauses, possibly due to dynamic directive defining Predicate.
            % add ours as a <NOP,0>
            assertz(clause_table(Predicate, 0, [254, 0|Codes]))
        ; otherwise->
            % Otherwise Predicate not defined so there are no clauses yet. So just add ours as a <NOP,0>
            assertz(clause_table(Predicate, 0, [254, 0|Codes]))
        ).

add_clause_to_aux(AuxLabel, N, L, LT):-
        ( nonvar(AuxLabel),
          AuxLabel = defined(A) ->
            NN is N xor 0x80000000,
            add_clause_to_existing(A, NN),
            L = LT
        ; otherwise->
            % Brand new aux! This gets <NOP,0> and sets L
            NN is N+1,
            assert(ctable(N, 254)),
            assert(ctable(NN, 0)),
            AuxLabel = defined(N),
            L = [label(AuxLabel, N)|LT]
        ).


add_clause_to_existing(A, N):-
        AA is A+1,
        NN is N+1,
        ( ctable(A, 254)->
            % Change <NOP,0> -> <try_me_else N>
            retract(ctable(A, _)),
            retract(ctable(AA, _)),
            assert(ctable(A, 28)),
            assert(ctable(AA, N)),
            % Add <trust_me, 0> at N
            assert(ctable(N, 30)),
            assert(ctable(NN, 0))
        ; ctable(A, 28)->
            % Follow path
            ctable(AA, Link),
            add_clause_to_existing(Link, N)
        ; ctable(A, 29)->
            % Follow link
            ctable(AA, Link),
            add_clause_to_existing(Link, N)
        ; ctable(A, 30)->
            % Change <trust_me, 0> -> <retry_me_else, N>
            retract(ctable(A, _)),
            retract(ctable(AA, _)),
            assert(ctable(A, 29)),
            assert(ctable(AA, N)),
            % Add <trust_me, 0> at N
            assert(ctable(N, 30)),
            assert(ctable(NN, 0))
        ).


quote_atom_for_javascript([], '"[]"'):- !.
quote_atom_for_javascript(Atom, QuotedAtom):-
        atom_codes(Atom, Codes),
        quote_atom_for_javascript_1(QuotedCodes, Codes, []),
        atom_codes(QuotedAtom, QuotedCodes).

quote_atom_for_javascript_1([34|Codes])-->
        quote_atom_for_javascript_2(Codes).

quote_atom_for_javascript_2([92, 110|Codes])-->
        "\n", !,
        quote_atom_for_javascript_2(Codes).

quote_atom_for_javascript_2([92, 92|Codes])-->
        "\\", !,
        quote_atom_for_javascript_2(Codes).

quote_atom_for_javascript_2([92, 34|Codes])-->
        [34], !, % '
        quote_atom_for_javascript_2(Codes).


quote_atom_for_javascript_2([Code|Codes])-->
        [Code],
        quote_atom_for_javascript_2(Codes).

quote_atom_for_javascript_2([34], [], []):- !.


dump_tables(S):-
        ( setof(N-Atom, atable(Atom, N), Atoms)-> true ; otherwise-> Atoms = []),
        findall(QuotedAtom,
                ( member(_-Atom, Atoms),
                  quote_atom_for_javascript(Atom, QuotedAtom)
                ),
                SortedAtoms),
        atomic_list_concat(SortedAtoms, ', ', AtomAtom),
        format(S, 'atable = [~w];~n', [AtomAtom]),
        ( setof(N-Float, fltable(Float, N), Floats)-> true ; otherwise-> Floats = []),
        findall(Float, member(_-Float, Floats), FloatAtoms),
        atomic_list_concat(FloatAtoms, ', ', FloatAtom),
        format(S, 'floats = [~w];~n', [FloatAtom]),

        ( setof(N-F, ftable(F, N), Functors)-> true ; otherwise-> Functors = []),
        findall(Functor, (member(_-F/A, Functors),
                          format(atom(Functor), '[~w,~w]', [F, A])),
                SortedFunctors),
        atomic_list_concat(SortedFunctors, ', ', FunctorAtom),
        format(S, 'ftable = [~w];~n', [FunctorAtom]),
        ( setof(F, dtable(F), DynamicFunctors)-> true ; otherwise-> DynamicFunctors = []),
        atomic_list_concat(DynamicFunctors, ', ', DynamicFunctorAtom),
        format(S, 'dtable = [~w];~n', [DynamicFunctorAtom]),
        findall(PredicateAtom,
                dump_predicate(PredicateAtom),
                Predicates),
        atomic_list_concat(Predicates, ', ', PredicatesAtom),
        format(S, 'predicates = {~w};~n', [PredicatesAtom]),
        findall(PredicateAtom,
                ( fptable(Predicate, Symbol),
                  format(atom(PredicateAtom), '~w: ~w', [Predicate, Symbol])
                ),
                FPredicates),
        atomic_list_concat(FPredicates, ', ', FPredicatesAtom),
        format(S, 'foreign_predicates = {~w};~n', [FPredicatesAtom]),
        findall(G, stable(G), SGs),
        atomic_list_concat(SGs, ', ', SystemAtom),
        format(S, 'system = [~w];~n', [SystemAtom]),
        findall(G, itable(G), IGs),
        atomic_list_concat(IGs, ', ', InitializationAtom),
        format(S, 'initialization = [~w];~n', [InitializationAtom]).

dump_predicate(PredicateAtom) :-
       bagof(Clause-Index,
            clause_table(Functor, Index, Clause), % Clause = [] only holds for dynamic predicate pseudo-clause.
            Clauses),
       (Clauses = [[]-0]
         -> ClauseAtom = '', IndexAtom = '', I = 0
       ;
       delete(Clauses, []-0, RealClauses),
       Clauses = RealClauses,
       aggregate_all(r(bag(ClauseAtom),
                      bag(I)),
                    ( member(Clause-I, RealClauses),
                      format(atom(ClauseAtom), '~w:{code:~w, key:~w}', [I, Clause, I])
                    ),
                    r(ClauseAtoms, IndexAtoms)),
       atomic_list_concat(IndexAtoms, ', ', IndexAtom),
       atomic_list_concat(ClauseAtoms, ', ', ClauseAtom),
       list_length(RealClauses, I)
       )
       ,
       (dtable(Functor) -> Dynamic = 'true';Dynamic = 'false'),
       format(atom(PredicateAtom), '~w: {is_public:~w, clauses:{~w}, clause_keys:[~w], next_key:~w, key:~w}', [Functor, Dynamic, ClauseAtom, IndexAtom, I, Functor]).

reserve_predicate(Functor/Arity, Foreign):-
        lookup_functor(Functor, Arity, F),
        assert(fptable(F, Foreign)).

reset_compile_buffer:-
        retractall(ctable(_, _)).

reset:-
        retractall(ctable(_, _)),
        retractall(clause_table(_,_,_)),
        retractall(atable(_,_)),
        retractall(ftable(_,_)),
        retractall(fptable(_,_)),
        % [] is always 0
        assert(atable('[]', 0)),

        flag(ftable, _, 0),
        flag(atable, _, 1),

        % Then add in some reserved predicates

        % ISO foreign predicates
        reserve_predicate(acyclic_term/1, predicate_acyclic_term),
        reserve_predicate(subsumes_term/2, predicate_subsumes_term),
        reserve_predicate(compare/3, predicate_compare),
        reserve_predicate(var/1, predicate_var),
        reserve_predicate(atom/1, predicate_atom),
        reserve_predicate(integer/1, predicate_integer),
        reserve_predicate(float/1, predicate_float),
        reserve_predicate(compound/1, predicate_compound),
        reserve_predicate(ground/1, predicate_ground),
        reserve_predicate((=)/2, predicate_unify),
        reserve_predicate((==)/2, predicate_match),
        reserve_predicate(functor/3, predicate_functor),
        reserve_predicate(arg/3, predicate_arg),
        reserve_predicate((=..)/2, predicate_univ),
        reserve_predicate(copy_term/2, predicate_copy_term),
        reserve_predicate(halt/1, predicate_halt),
        reserve_predicate(current_prolog_flag/2, predicate_current_prolog_flag),
        reserve_predicate(set_prolog_flag/2, predicate_set_prolog_flag),
        reserve_predicate(repeat/0, predicate_repeat),
        reserve_predicate(atom_length/2, predicate_atom_length),
        reserve_predicate(atom_concat/3, predicate_atom_concat),
        reserve_predicate(sub_atom/5, predicate_sub_atom),
        reserve_predicate(char_code/2, predicate_char_code),
        reserve_predicate(atom_chars/2, predicate_atom_chars),
        reserve_predicate(atom_codes/2, predicate_atom_codes),
        reserve_predicate(number_chars/2, predicate_number_chars),
        reserve_predicate(number_codes/2, predicate_number_codes),

        reserve_predicate(char_conversion/2, predicate_char_conversion),
        reserve_predicate(current_char_conversion/2, predicate_current_char_conversion),

        reserve_predicate(current_predicate/1, predicate_current_predicate),

        reserve_predicate((@>)/2, predicate_term_gt),
        reserve_predicate((@>=)/2, predicate_term_egt),
        reserve_predicate((@<)/2, predicate_term_lt),
        reserve_predicate((@=<)/2, predicate_term_elt),

        reserve_predicate(is/2, predicate_is),
        reserve_predicate((>)/2, predicate_gt),
        reserve_predicate((<)/2, predicate_lt),
        reserve_predicate((=<)/2, predicate_elt),
        reserve_predicate((>=)/2, predicate_egt),
        reserve_predicate((=:=)/2, predicate_eq),
        reserve_predicate((=\=)/2, predicate_ne),

        reserve_predicate(set_input/1, predicate_set_input),
        reserve_predicate(set_output/1, predicate_set_output),
        reserve_predicate(current_output/1, predicate_current_output),
        reserve_predicate(current_input/1, predicate_current_input),
        reserve_predicate(get_char/2, predicate_get_char),
        reserve_predicate(get_code/2, predicate_get_code),
        reserve_predicate(peek_char/2, predicate_peek_char),
        reserve_predicate(peek_code/2, predicate_peek_code),
        reserve_predicate(put_char/2, predicate_put_char),
        reserve_predicate(put_code/2, predicate_put_code),

        reserve_predicate(get_byte/2, predicate_get_byte),
        reserve_predicate(peek_byte/2, predicate_peek_byte),
        reserve_predicate(put_byte/2, predicate_put_byte),

        reserve_predicate(flush_output/1, predicate_flush_output),
        reserve_predicate(at_end_of_stream/1, predicate_at_end_of_stream),
        reserve_predicate(set_stream_position/2, predicate_set_stream_position),
        reserve_predicate(stream_property_1/2, predicate_stream_property),
        reserve_predicate(current_stream/1, predicate_current_stream),
        reserve_predicate(write_term/3, predicate_write_term),
        reserve_predicate(current_op/3, predicate_current_op),

        reserve_predicate(fail/0, predicate_fail),
        reserve_predicate(true/0, predicate_true),
        reserve_predicate(term_variables/2, predicate_term_variables),
        reserve_predicate(writeln/1, writeln),
        reserve_predicate(gensym/2, predicate_gensym),
        reserve_predicate(atom_to_term/3, atom_to_term),
        reserve_predicate(clause/2, predicate_clause),
        reserve_predicate(abolish/1, predicate_abolish),
        reserve_predicate(retract_clause/2, predicate_retract_clause),
        reserve_predicate(read_term/3, read_term),
        reserve_predicate(close/2, predicate_close),
        reserve_predicate(op/3, predicate_op),

        % Some handy extensions
        reserve_predicate(atom_to_memory_file/2, atom_to_memory_file),
        reserve_predicate(memory_file_to_atom/2, memory_file_to_atom),
        reserve_predicate(new_memory_file/1, new_memory_file),
        reserve_predicate(open_memory_file/3, open_memory_file),
        reserve_predicate(free_memory_file/1, free_memory_file),
        reserve_predicate(format/3, predicate_format),
        reserve_predicate(flag/3, predicate_flag),

        % Stuff related to actually compiling
        reserve_predicate(reset_compile_buffer/0, reset_compile_buffer),
        reserve_predicate(emit_code/2, emit_code),
        reserve_predicate(lookup_atom/2, predicate_lookup_atom),
        reserve_predicate(lookup_float/2, predicate_lookup_float),
        reserve_predicate(lookup_functor/3, predicate_lookup_functor),
        reserve_predicate(add_clause_to_predicate/3, add_clause_to_predicate),
        reserve_predicate(add_clause_to_aux/4, add_clause_to_aux),
        reserve_predicate(prepend_clause_to_predicate/3, prepend_clause_to_predicate),
        reserve_predicate(flush_stdout/0, predicate_flush_stdout),
        reserve_predicate(debug/0, predicate_debug),
        reserve_predicate(nodebug/0, predicate_nodebug),
        reserve_predicate('$jmp'/1, predicate_jmp),
        reserve_predicate(generate_initialization_goal/1, predicate_generate_initialization_goal),
        reserve_predicate(generate_system_goal/1, predicate_generate_system_goal),
        reserve_predicate(define_dynamic_predicate/1, predicate_define_dynamic_predicate),

        % Promises
        reserve_predicate(request_result/1, predicate_request_result),
        reserve_predicate(handle_result/2, predicate_handle_result),
        reserve_predicate(fetch_promise/2, predicate_fetch_promise),

        % Debugging
        reserve_predicate(trace_unify/2, predicate_trace_unify),
        reserve_predicate('$trace_set'/1, predicate_trace_set),
        reserve_predicate('$trace_value'/1, predicate_trace_value),
        reserve_predicate('$trace_set_info'/1, predicate_trace_set_info),
        reserve_predicate('$suspend_set'/1, predicate_suspend_set),
        reserve_predicate(get_terminal_char/1, predicate_get_terminal_char),
        reserve_predicate('$trace_set_retry'/1, predicate_trace_set_retry),
        reserve_predicate('$trace_retry_value'/1, predicate_trace_retry_value),
        reserve_predicate('$trace_set_prompt'/1, predicate_trace_set_prompt),
        reserve_predicate('$get_backtrack_frame'/1, predicate_get_backtrack_frame),
        reserve_predicate('$set_backtrack_frame'/1, predicate_set_backtrack_frame),
        reserve_predicate('$trace_instruction_set'/1, predicate_trace_instruction_set),

        % Testing
        reserve_predicate(member/2, member),

        % Call cleanup
        reserve_predicate(mark_top_choicepoint/2, mark_top_choicepoint),
        reserve_predicate(unmark_choicepoint/1, unmark_choicepoint),
        reserve_predicate(unmark_top_choicepoint/0, unmark_top_choicepoint),

        % Exceptions
        reserve_predicate(get_current_block/1, get_current_block),
        reserve_predicate(install_new_block/1, install_new_block),
        reserve_predicate(reset_block/1, reset_block),
        reserve_predicate(unwind_stack/0, unwind_stack),
        reserve_predicate(clean_up_block/1, clean_up_block),
        reserve_predicate(throw/1, predicate_throw),
        reserve_predicate(get_exception/1, get_exception),
        reserve_predicate(clear_exception/0, clear_exception),

        % Recorded database
        reserve_predicate(recorda/3, recorda),
        reserve_predicate(recordz/3, recordz),
        reserve_predicate(recorded/3, recorded),
        reserve_predicate(erase/1, erase),

        % GC
        reserve_predicate(gc/0, predicate_gc),
        reserve_predicate(statistics/0, predicate_statistics),
        reserve_predicate(wam_duration/1, predicate_wam_duration),

        % Javascript
        reserve_predicate(eval_javascript/1, predicate_eval_javascript),
        reserve_predicate(eval_javascript/2, predicate_eval_javascript),

        % DOM
        reserve_predicate(remove_dom_element_class/2, predicate_remove_dom_element_class),
        reserve_predicate(replace_dom_element_class/3, predicate_replace_dom_element_class),
        reserve_predicate(toggle_dom_element_class/3, predicate_toggle_dom_element_class),
        reserve_predicate(set_dom_element_attribute_value/3, predicate_set_dom_element_attribute_value),
        reserve_predicate(dom_element_attribute_value/3, predicate_dom_element_attribute_value),
        reserve_predicate(create_dom_element/2, predicate_create_dom_element),
        reserve_predicate(create_dom_text_node/2, predicate_create_dom_text_node),
        reserve_predicate(append_dom_node_child/2, predicate_append_dom_node_child),
        reserve_predicate(insert_before_dom_node/3, predicate_insert_before_dom_node),
        reserve_predicate(dom_select_element/2, predicate_dom_select_element),
        reserve_predicate(dom_select_all_elements/2, predicate_dom_select_all_elements),
        reserve_predicate(dom_object_property/4, predicate_dom_object_property),
        reserve_predicate(dom_object_method/2, predicate_dom_object_method),
        reserve_predicate(set_dom_object_property/3, predicate_set_dom_object_property),
        reserve_predicate(alert/1, predicate_alert),

        true.


build_saved_state(SourceFiles, TopLevelQuery):-
        reset,
        assemble([call(toplevel/0,0), execute(halt/0), retry_foreign], 2),
        setof(N-Code, ctable(N, Code), SortedBootCodes),
        findall(Code, member(_-Code, SortedBootCodes), BootCodes),
        atomic_list_concat(BootCodes, ',', BootCode),
        compile_clause(toplevel:-TopLevelQuery),
        ( compile_files(SourceFiles)->
            true
        ; otherwise->
            writeln(failed_to_compile),
            halt,
            fail
        ),
        !,
        open('bootstrap.js', write, S1),
        format(S1, 'function load_state() {~n', []),
        format(S1, 'bootstrap_code = [0,255,~w];~n', [BootCode]),
        format(S1, 'retry_foreign_offset = 7;~n', []),
%        format(S1, 'retry_foreign = {code: bootstrap_code, offset:7};~n', []),
        dump_tables(S1),
        format(S1, '}~n', []),
        close(S1),
        !.


% eg bootstrap('demo.pl', (factorial(5, X), writeln(X))).
% Ultimately, bootstrap('prolog.pl', prolog_toplevel).
bootstrap(Source, Query):-
        bootstrap('', [Source], Query).

bootstrap(CorePrefix, Sources, Query):-
        current_prolog_flag(version_data, swi(Mj, Mn, P, E)),
        (Mj >= 7 -> true;throw(wrong_swi_version('expected >= 7', swi(Mj, Mn, P, E)))),
        % Since javascript will not support open/3, we must load it into an atom and pass it.
        % Ultimately we could use XmlHTTPRequest, but probably that is less useful anyway
        files_to_atoms(Sources, Atoms),
        atom_concat(CorePrefix, 'wam_compiler.pl', WAM),
        atom_concat(CorePrefix, 'debugger.pl', Debugger),
        atom_concat(CorePrefix, 'bootstrap_js.pl', Bootstrap),
        build_saved_state([WAM,
                           Debugger,
                           Bootstrap],
                          ( writeln(toplevel),
                            compile_clause(bootstrap:-Query),
                            statistics,
                            compile_atoms(Atoms),
                            statistics,
                            !,
                            bootstrap
                            )).


files_to_atoms([], []).
files_to_atoms([H|T], [HA|TA]) :-
        file_to_atom(H, HA),
        files_to_atoms(T, TA).

file_to_atom(Filename, Atom):-
  with_output_to(atom(Atom),
    (current_output(W),
     open(Filename, read, R),
     copy_stream_data(R, W),
     close(R)
    )).

trace_unify(A, A).

/*
compile_message(A):-
    A = [H|T] -> write(H), compile_message(T)
    ;
    A = [] -> writeln('')
    ;
    writeln(A).
*/

compile_message(_).
flush_stdout.
gc.

% The module/2 predicate is a no-op to allow the wam_compiler.pl source to be compiled
% where the wam_compiler:compile_clause_2/N predicate attempts to call the directives
% it encounters.

module(_,_).

concat_list(L, A) :-
    concat_list(L, '', A).

concat_list([], A, A).
concat_list([H|T], A, B) :-
    (atom(H) ->
      atom_concat(A, H, X)
    ;
    number(H) ->
      number_codes(H, HC),
      atom_codes(HA, HC),
      atom_concat(A, HA, X)
    ;
    format(atom(HA), '~w', [H]),
    atom_concat(A, HA, X)
    ),
    concat_list(T, X, B).
