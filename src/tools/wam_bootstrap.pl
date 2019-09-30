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
:- module(wam_bootstrap,
    [add_clause_to_predicate/3, emit_code/2,
    generate_system_goal/2, generate_initialization_goal/2, define_dynamic_predicate/1,
    add_module_export/2, module_export/2, add_module_import/2, module_import/2, add_meta_predicate/3, pls_meta_predicate/3,
    handle_term_expansion/1,
    fetch_promise/2, promise_result/2, open_memory_file/3, reset/0, gc/0, reset_compile_buffer/0, dump_tables/1]).

:- use_module(wam_bootstrap_util).
:- use_module('../system/wam_util').
:- use_module('../system/wam_assemble').

generate_system_goal(Module, Init) :-
        flag(stable, N, N+1),
        number_codes(N, NCs),
        atom_codes(Module, ModuleCodes),
        append(ModuleCodes, ":$sys_", PrefixCodes),
        append(PrefixCodes, NCs, ICs),
        atom_codes(Init, ICs),
        lookup_functor(Init, 0, FN),
        assert(stable(FN)).

generate_initialization_goal(Module, Init) :-
        flag(itable, N, N+1),
        number_codes(N, NCs),
        atom_codes(Module, ModuleCodes),
        append(ModuleCodes, "$init_", PrefixCodes),
        append(PrefixCodes, NCs, ICs),
        atom_codes(Init, ICs),
        lookup_functor(Init, 0, FN),
        assert(itable(FN)).

% dynamic implies public. In proscript, public also implies dynamic.
% the is_public flag will be set to true in the saved state
% for predicate Name/Arity.

define_dynamic_predicate(Module : (Name/Arity)) :-
        wam_compiler:transform_predicate_name(Name, Arity, Module, TransformedFunctor),
        lookup_dynamic_functor(TransformedFunctor, Arity, Predicate),
        (clause_table(Predicate, _, _, _, _)
          -> true
        ;
         assertz(clause_table(Predicate, 0, [], none, none))
        ).

add_module_export(Name, F/A) :-
        lookup_atom(Name, NameID),
        lookup_atom(F, FID),
        assertz('pls$module_export'(NameID, FID, A)).
add_module_export(_Name, op(_,_,_)). % ignore op exports for now.

module_export(Name, F/A) :-
        var(Name)
          -> (var(F)
               -> 'pls$module_export'(NameID, FID, A),
                  atable(Name, NameID),
                  atable(F, FID)
              ;
               lookup_atom(F, FID),
               'pls$module_export'(NameID, FID, A),
               atable(Name, NameID)
              )
        ;
        var(F)
          -> lookup_atom(Name, NameID),
             'pls$module_export'(NameID, FID, A),
             atable(F, FID)
        ;
        lookup_atom(Name, NameID),
        lookup_atom(F, FID),
        'pls$module_export'(NameID, FID, A).

add_module_import(Importing, Imported) :-
        lookup_atom(Importing, ImportingID),
        lookup_atom(Imported, ImportedID),
        assertz('pls$import'(ImportingID, ImportedID)).

module_import(Importing, Imported) :-
        var(Importing)
          -> (var(Imported)
               -> 'pls$import'(ImportingID, ImportedID),
                  lookup_atom(Importing, ImportingID),
                  lookup_atom(Imported, ImportedID)
             ;
              true
               -> lookup_atom(Imported, ImportedID),
                  'pls$import'(ImportingID, ImportedID),
                  lookup_atom(Importing, ImportingID)
             )
        ;
        var(Imported)
          -> lookup_atom(Importing, ImportingID),
             'pls$import'(ImportingID, ImportedID),
             lookup_atom(Imported, ImportedID)
        ;
        lookup_atom(Importing, ImportingID),
        lookup_atom(Imported, ImportedID),
        'pls$import'(ImportingID, ImportedID).

add_meta_predicate(Functor, Arity, ArgTypes) :-
        lookup_atom(Functor, FunctorID),
        lookup_atoms(ArgTypes, ArgTypeIDs, Redo),
        arg_type_redo_check('add_meta_predicate ArgTypes', ArgTypes, Redo),
        assertz('pls$meta_predicate'(FunctorID, Arity, ArgTypeIDs)).

lookup_atoms([], [], []).
lookup_atoms([ArgType|OtherArgTypes], [ArgTypeID|OtherArgTypeIDs], Redo) :-
        (var(ArgType), var(ArgTypeID)
         -> Redo = [ArgType-ArgTypeID|OtherRedo]
        ;
         lookup_type_atom(ArgType, ArgTypeID),
         Redo = OtherRedo
        ),
        lookup_atoms(OtherArgTypes, OtherArgTypeIDs, OtherRedo).

lookup_atoms_redo([]).
lookup_atoms_redo([ArgType-ArgTypeID|OtherRedo]) :-
        lookup_type_atom(ArgType, ArgTypeID),
        lookup_atoms_redo(OtherRedo).

lookup_type_atom(Type, TypeID) :-
        var(Type)
          -> lookup_atom(BaseType, TypeID),
             atom_codes(BaseType, BaseTypeCodes),
             (append("t", R, BaseTypeCodes)
               -> number_codes(Type, R)
             ;
              BaseType = Type
             )
        ;
        number(Type)
          -> number_codes(Type, TypeCodes),
             append("t", TypeCodes, BaseTypeCodes),
             atom_codes(BaseType, BaseTypeCodes),
             lookup_atom(BaseType, TypeID)
        ;
        lookup_atom(Type, TypeID).

arg_type_redo_check(Msg, ErrorTerm, Redo) :-
        Redo \= []
        -> atom_concat(Msg, ' partially bound ', FullMsg),
           throw(engine_error(FullMsg, ErrorTerm))
        ;
        true.

pls_meta_predicate(Functor, Arity, ArgTypes) :-
        var(Functor)
          -> (var(ArgTypes)
               -> 'pls$meta_predicate'(FunctorID, Arity, ArgTypeIDs),
                  lookup_atoms(ArgTypes, ArgTypeIDs, Redo),
                  arg_type_redo_check('pls_meta_predicate ArgTypeIDs', ArgTypeIDs, Redo),
                  lookup_atom(Functor, FunctorID)
             ;
              lookup_atoms(ArgTypes, ArgTypeIDs, Redo),
              'pls$meta_predicate'(FunctorID, Arity, ArgTypeIDs),
              lookup_atoms_redo(Redo),
              lookup_atom(Functor, FunctorID)
             )
        ;
        var(ArgTypes)
          -> lookup_atom(Functor, FunctorID),
             'pls$meta_predicate'(FunctorID, Arity, ArgTypeIDs),
             lookup_atoms(ArgTypes, ArgTypeIDs, Redo),
             arg_type_redo_check('pls_meta_predicate ArgTypeIDs', ArgTypeIDs, Redo)
        ;
        lookup_atom(Functor, FunctorID),
        lookup_atoms(ArgTypes, ArgTypeIDs, Redo),
        'pls$meta_predicate'(FunctorID, Arity, ArgTypeIDs),
        lookup_atoms_redo(Redo).

handle_term_expansion(Clause) :-
    assertz(Clause).

fetch_promise(_,_).
promise_result(_,_).
open_memory_file(_,_,_).

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
        format(S, 'initialization = [~w];~n', [InitializationAtom]),
        setof(X,
              MN^ENs^
              (setof([FN,A],
                    'pls$module_export'(MN, FN, A),
                    ENs
                 ),
               format(atom(X), '~w : ~w', [MN, ENs]) % { ModuleN1 : [Export1, ...], ModuleN2 : [Export1, ...]...}
              ), R),
        atomic_list_concat(R, ', ', ModuleExportsAtom),
        format(S, 'module_exports = {~w};~n', [ModuleExportsAtom]),
        setof(X,
            G^Ds^
            (setof(D, 'pls$import'(G, D), Ds),
             format(atom(X), '~w : ~w', [G, Ds]) % { ModuleN1 : [Import1, ...], ModuleN2 : [Import1, ...]...}
            ),
            ModuleImports),
        atomic_list_concat(ModuleImports, ', ', ModuleImportsAtom),
        format(S, 'module_imports = {~w};~n', [ModuleImportsAtom]),
        setof(X,
            F^Ws^Core^
            (setof(W,
                A^Ts^
                ('pls$meta_predicate'(F, A, Ts),
                 format(atom(W), '~w : ~w', [A, Ts]) % A1 : [T1, ...]
                ),
                Ws),
             list_to_core_string(Ws, Core),
             format(atom(X), '~w : {~w}', [F, Core]) % F1 : {A1 : [T1...], A2 : [T1, ...]}
            ),
            MetaPredicates),
        atomic_list_concat(MetaPredicates, ', ', MetaPredicatesAtom),
        format(S, 'meta_predicate_signatures = {~w};~n', [MetaPredicatesAtom]).

list_to_core_string(List, Core) :-
        format(atom(V), '~w', [List]),
        % remove first and last characters; '[' and ']'.
        atom_codes(V, [_|VCodes]),
        append(CoreCodes, [_], VCodes),
        atom_codes(Core, CoreCodes).

dump_predicate(PredicateAtom) :-
       bagof(c(Clause, Index, Head, Body),
            clause_table(Functor, Index, Clause, Head, Body), % Clause = [] only holds for dynamic predicate pseudo-clause.
            Clauses),
       (Clauses = [c([], 0, _, _)]
         -> ClauseAtom = '', IndexAtom = '', I = 0
       ;
       delete(Clauses, c([], 0, _, _), RealClauses),
       Clauses = RealClauses,
       aggregate_all(r(bag(ClauseAtom),
                      bag(I)),
                    ( member(c(Clause, I, Head, Body), RealClauses),
                      (dtable(Functor)
                        -> record_term(Head, HeadString),
                           record_term(Body, BodyString),
                           format(atom(ClauseAtom), '~w:{code:~w, key:~w, head:~w, body:~w}', [I, Clause, I, HeadString, BodyString])
                       ;
                       format(atom(ClauseAtom), '~w:{code:~w, key:~w}', [I, Clause, I])
                      )
                    ),
                    r(ClauseAtoms, IndexAtoms)),
       atomic_list_concat(IndexAtoms, ', ', IndexAtom),
       atomic_list_concat(ClauseAtoms, ', ', ClauseAtom),
       list_length(RealClauses, I)
       )
       ,
       (dtable(Functor) -> Dynamic = 'true';Dynamic = 'false'),
       format(atom(PredicateAtom), '~w: {is_public:~w, clauses:{~w}, clause_keys:[~w], next_key:~w, key:~w}', [Functor, Dynamic, ClauseAtom, IndexAtom, I, Functor]).

% add_meta_predicate(Functor, Arity, ArgTypes)

reserve_predicate(Functor/Arity, Foreign):-
        reserve_predicate(Functor/Arity, Foreign, []).

reserve_predicate(Functor/Arity, Foreign, Meta):-
        [ColonCode] = ":",
        atom_codes(Functor, FunctorCodes),
        transform_predicate_name1(system, ColonCode, FunctorCodes, TransformedFunctor),
        lookup_functor(TransformedFunctor, Arity, F),
        assert(fptable(F, Foreign)),
        lookup_atom(system, SystemID),
        lookup_atom(Functor, FunctorID),
        assertz('pls$module_export'(SystemID, FunctorID, Arity)),
        (Meta = [_|_]
          -> add_meta_predicate(TransformedFunctor, Arity, Meta)
        ;
         true
        ).

% record_term returns a string which is a javascript representation of the term
record_term(Term, String) :-
    (var(Term) -> JSON = var(Term)
    ; atom(Term) -> JSON = atom(Term)
    ; float(Term) -> JSON = float(Term)
    ; integer(Term) -> JSON = integer(Term)
    ; Term = [_|_]
       -> record_terms(Term, Terms, FinalTail),
          JSON = list(Terms, FinalTail)
    ; Term =.. [_,_|_]
       -> JSON = structure(Name, ArgsJSON),
          Term =.. [Name|Args],
          record_terms(Args, ArgsJSON, _)
    ; throw(unrecognized_term_type(Term))
    ),
    json_string(JSON, String).

record_terms(X, [], XR) :-
    (var(X) ; X \= [_|_]),
    !,
    record_term(X, XR).
record_terms([H|T], [HR|TR], FinalTail) :-
    record_term(H, HR),
    record_terms(T, TR, FinalTail).

json_string(JSON, String) :-
    JSON =.. [Type|Values],
    json_type(Type, TypeInteger, Format),
    format(atom(ValueString), Format, Values),
    escape_newlines(ValueString, EscapedValueString),
    format(atom(String), '{type: ~w, ~w}', [TypeInteger, EscapedValueString]).

json_type(var, 0, 'key: "~w"').
json_type(atom, 4, 'value: "~w"').
json_type(float, 5, 'value: ~w').
json_type(integer, 3, 'value: ~w').
json_type(list, 2, 'value: ~w, tail: ~w').
json_type(structure, 1, 'name: "~w",  args: ~w').

escape_newlines(ValueString, EscapedValueString) :-
    atom_codes(ValueString, Codes),
    escape_newlines1(Codes, EscapedCodes),
    atom_codes(EscapedValueString, EscapedCodes).

escape_newlines1([], []).
escape_newlines1([H|T], EscapedCodes) :-
    escape_newline([H], EscapedCodes, Tail),
    escape_newlines1(T, Tail).

escape_newline("\n", EscapedCodes, Tail) :-
    !,
    append("\\n", Tail, EscapedCodes).
escape_newline([C], [C|Tail], Tail).

reset_compile_buffer:-
        retractall(ctable(_, _)).

reset:-
        retractall(ctable(_, _)),
        retractall(clause_table(_,_,_,_,_)),
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

        reserve_predicate(current_predicate/1, predicate_current_predicate, [(:)]),

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
        reserve_predicate(clause/2, predicate_clause, [0,?]),
        reserve_predicate(abolish/1, predicate_abolish),
        reserve_predicate(retract_clause/2, predicate_retract_clause),
        reserve_predicate(read_term/3, read_term),
        reserve_predicate(open/4, predicate_open),
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
        reserve_predicate(memory_file_description/2, predicate_memory_file_description),
        reserve_predicate(absolute_file_name/3, predicate_absolute_file_name),

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
        reserve_predicate(generate_initialization_goal/2, predicate_generate_initialization_goal),
        reserve_predicate(generate_system_goal/2, predicate_generate_system_goal),
        reserve_predicate(define_dynamic_predicate/1, predicate_define_dynamic_predicate),
        reserve_predicate(compiled_state_boot_code/1, predicate_compiled_state_boot_code),
        reserve_predicate(dump_tables/1, predicate_dump_tables),
        reserve_predicate(add_module_export/2, predicate_add_module_export),
        reserve_predicate(module_export/2, predicate_module_export),
        reserve_predicate(add_module_import/2, predicate_add_module_import),
        reserve_predicate(module_import/2, predicate_module_import),
        reserve_predicate(add_meta_predicate/3, predicate_add_meta_predicate),
        reserve_predicate(pls_meta_predicate/3, predicate_pls_meta_predicate),

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
        reserve_predicate(record_term/2, record_term),

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
        reserve_predicate(dom_object_method/2, predicate_dom_object_method, [?, (:)]),
        reserve_predicate(dom_object_method/3, predicate_dom_object_method, [?, (:), ?]),
        reserve_predicate(dom_object_type/2, predicate_dom_object_type),
        reserve_predicate(dom_create_object/2, predicate_dom_create_object, [(:), ?]),
        reserve_predicate(dom_create_object/3, predicate_dom_create_object, [(:), ?, ?]),
        reserve_predicate(dom_type_reference/4, predicate_dom_type_reference),
        reserve_predicate(dom_release_object/1, predicate_dom_release_object),
        reserve_predicate(set_dom_object_property/3, predicate_set_dom_object_property),
        reserve_predicate(set_dom_object_property/4, predicate_set_dom_object_property),
        reserve_predicate(alert/1, predicate_alert),
        reserve_predicate(dom_window/1, predicate_dom_window),
        reserve_predicate(dom_type_property/4, predicate_dom_type_property),
        reserve_predicate(dom_type_method/5, predicate_dom_type_method),
        reserve_predicate(dom_type_parent/2, predicate_dom_type_parent),
        true.



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

flush_stdout.
gc.

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
