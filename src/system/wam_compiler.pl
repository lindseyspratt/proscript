/* To do list
    indexing
    Bignums?
    Modules
    ISO exceptions
    Fix the 0x80000000 hack. Just make new opcodes for try/retry/trust in aux clauses.
    Environment trimming is not quite right. We cannot trim the environment at the point where it gets deallocated, since
       then !(Var) will not work (since it cannot look up Var, as we deleted it). But at the same time, if we do NOT trim it, EnvSize is wrong
       though it is only too big, not too small, so at least it is just inefficient, rather than dangerous.
*/

/* Overview:
Compilation is fairly direct. Note that a query is a clause without a head, and a fact is a clause without a body.
The machine has only X-registers. I ignore the distinction between A- and X- registers - A0 is the same as X0 (this is true in the paper as well; the A- notation is
purely notational and has no semantic meaning. In other words, 'the arguments to a n-ary predicate are passed in the first n X-registers'.

Compilation has 4 parts: Head, Head-unification, Body, Body-unification.
   * First compile the head. This involves a get_* for each argument to the head.
      * If we get_structure, we may end up needing to get some of the arguments as variables, since foo(bar(x, baz(y))):- ... is effectively compiled into
        foo(X):- X = bar(x, Y), Y = baz(y), ...
        Therefore, after a get_structure we compile in a slightly different mode where we can unify with integers, unify with atoms, unify with existing variables
        and unify with fresh variables which we must later unify with structures via get_structure(FreshVar, ....)
   * Next, compile the body
      * If we put_structure, we must then unify the arguments of the body. This is a bit more complicated; we have to already have them on the heap if we want to use them.
        Because of this, we have to do things slightly backwards: If we want to put_structure and then N arguments to that structure, first we compile the arguments
        into M fresh variables (we only need to do this for arguments which are not variables, constants or integers), and emit opcodes for that BEFORE we do the
        put_structure and its associated arguments (which are now either all fresh variables, constants, integers, or existing variables we just created)

Register allocation is as follows:
   * First, determine permanent variables. I use Ait-Kaci's definition, rather than Warren's, because of the serious flaw pointed out in AK.
      * A variable is permanent if it occurs in more than one goal, where the first goal shares the variables with the head.
      * Any variable which is not permanent is temporary.
   * Before compiling each n-ary body goal, we must reserve n X-registers for the arguments. Anything above that is fair game
      * Since X-registers are temporary, their contents before compiling this particular body goal are irrelevant, and we are allowed to zap any of them we like
      * However, the M-ary head and N-ary first body goal have to be considered a single unit. Really the number of X-registers to reserve is max(N,M), but I use N+M.

In addition to just identifying permanent variables, we must progressively trim them from the list as execution proceeds. This enables us to keep track of what the argument
to call() and call_aux() should be which is used to do generalized LCO (ie environment trimming).

Disjuncts are handled via auxiliary clauses. That is, (A;B) is effectively compiled to aux_N, and later we must define
   * aux_N :- A.
   * aux_N :- B.
The argument sof aux_N are the variables in (A;B), plus optionally the cut variable for the current clause. This brings us to

Cuts are harder than they seem! If we didn't have auxiliary clauses, it would be a lot simpler. ! has to be transparent to these clauses, so we must pass in the point to
cut to as an argument to auxiliary clauses. This introduces quite a bit of complexity to the compilation. First, ! is translated into !(FreshVar) in the compilation. If there
is more than one cut in a body, FreshVar is unified. This is included in the list of 'arguments' to an auxiliary clause, which allows ! in an aux clause to find the right
cut point to cut in the actual clause. Consequently, a local cut is just a cut without this sharing - when compiling the aux clause, we check if a local cut has been detected
and then emit a get_level() there as well. To avoid complete confusion, (A->B->C) is translated with an aux_clause: (aux_1->C), and (aux_1:- A->B)

Multiple clauses are handled by a 2-word reserved space before each clause. The bytes are either:
   * trust_me, 0
   * try_me_else, L
   * retry_me_else, L
   * NOP, 0
The NOP case is for when there is only one clause and we do not need to create or destroy a choicepoint. This holds space for adding a try_me_else if we ever add more clauses
later.

The toplevel is a bit weird. There's no way to create an environment unless we are in a call, there's a chicken-and-egg situation: We cannot have a call unless there are 2
goals in a body, since otherwise it's either a fact or a chain, and neither of those have call() opcodes. Unfortunately, any 2+ goal clause requires an environment!
To resolve this, I have a faux-toplevel which just does call(toplevel/0, 0). Then I must ensure toplevel never returns, or we will be in a right mess, so I add execute(halt/0).
Note that this is essentially illegal, but it is safe since, despite the fact that toplevel/0 will have mucked up CP, it was originally set to garbage, so we don't need to save
it. Calling halt/0 never returns.

Some gotchas:
   * Even if there are no permanent variables, we still need an environment if there is more than one body goal so we can correctly save CP between calls

*/

:-module(wam_compiler, [op(920, fy, ?), op(920, fy, ??), compile_clause/1, compile_files/1, save_compiled_state/2]).

:- meta_predicate(compile_clause_system_directive(0)).

:- if(\+ (current_predicate(wam_compiler:current_compile_url/1), current_compile_url(_))).
    :- use_module('../tools/wam_bootstrap').
    :- use_module('../tools/wam_bootstrap_util').
    :- use_module(url). % in proscriptls system.
:- endif.

:- use_module(wam_assemble).
:- use_module(wam_util).


:-ensure_loaded('../tools/testing').

:-dynamic(delayed_initialization/1).
:-dynamic('$loaded'/1).
:-dynamic('$current_compilation_module'/1).
:-dynamic('$current_compilation_stream'/1).
:-dynamic('$import'/2).
:-dynamic('$meta_predicate'/3).

expand_term(In, Out):-
        current_predicate(term_expansion/2),
        term_expansion(In, Out),
        !.
expand_term(In, In).

compile_clause(Term) :-
        compile_clause(Term, mode(0, compile(0)), _).

compile_clause(Term, ModeIn, ModeOut):-
        compile_message(['Compiling ', Term]),
        expand_term(Term, Terms),
        gc,
        compile_message(done_gc),
        (compile_clause_1(Terms, ModeIn, ModeOut)
          -> true
        ;
         write('Failed compilation of '), writeln(Term)
          -> fail
        ).

compile_clause_1([], Mode, Mode):- !.
compile_clause_1([Head|Tail], ModeIn, ModeOut):-
        !,
        compile_clause_save(Head, ModeIn, ModeNext),
        compile_clause_1(Tail, ModeNext, ModeOut).
compile_clause_1(Term, ModeIn, ModeOut):-
        compile_clause_save(Term, ModeIn, ModeOut).

compile_clause_save(:- Body, ModeIn, ModeOut) :-
        !,
        compile_clause_directive(Body, ModeIn, ModeOut).
compile_clause_save(Term, Mode, Mode) :-
        mode_skip(Mode)
           -> true
        ;
        compile_clause_2(Term),
        save_clause(Term).

mode_skip(mode(_, skip(_))).

mode_compile(mode(_, compile(_))).

compile_clause_directive(Directive, ModeIn, ModeOut) :-
        compile_clause_directive_macro(Directive, ModeIn, ModeOut)
          -> true
        ;
        mode_skip(ModeIn)
          -> ModeIn = ModeOut
        ;
        compile_clause_directive_nonmacro(Directive, ModeIn, ModeOut).

compile_clause_directive_nonmacro(op(A,B,C), Mode, Mode) :- compile_clause_system_directive(op(A,B,C)).
compile_clause_directive_nonmacro(dynamic(PredIndicator), Mode, Mode) :- compile_clause_compilation_directive(dynamic(PredIndicator), Mode, Mode).
compile_clause_directive_nonmacro(initialization(Goal), Mode, Mode) :- compile_clause_initialization_directive(Goal).
compile_clause_directive_nonmacro(ensure_loaded(Spec), Mode, Mode) :- compile_clause_compilation_directive(ensure_loaded(Spec), Mode, Mode).
compile_clause_directive_nonmacro(include(_), Mode, Mode). % Currently a no-op in ProscriptLS.
compile_clause_directive_nonmacro(module(A,B), Mode, Mode) :- compile_clause_compilation_directive(module(A,B), Mode, Mode).
compile_clause_directive_nonmacro(use_module(A), Mode, Mode) :- compile_clause_compilation_directive(use_module(A), Mode, Mode).
compile_clause_directive_nonmacro(reexport(A), Mode, Mode) :- compile_clause_compilation_directive(reexport(A), Mode, Mode).
compile_clause_directive_nonmacro(meta_predicate(Term), Mode, Mode) :- compile_clause_compilation_directive(meta_predicate(Term), Mode, Mode).

compile_clause_directive_macro(if(Goal), ModeIn, ModeOut) :- compile_clause_compilation_directive(if(Goal), ModeIn, ModeOut).
compile_clause_directive_macro(else, ModeIn, ModeOut) :- compile_clause_compilation_directive(else, ModeIn, ModeOut).
compile_clause_directive_macro(elseif(Goal), ModeIn, ModeOut) :- compile_clause_compilation_directive(elseif(Goal), ModeIn, ModeOut).
compile_clause_directive_macro(endif, ModeIn, ModeOut) :- compile_clause_compilation_directive(endif, ModeIn, ModeOut).

% A compilation directive is evaluated immediately
% during compilation of a source unit (e.g. a file).
% There is no evaluation of the directive when the
% compiled WAM state is loaded and initialized.

compile_clause_compilation_directive(dynamic(PredIndicator), Mode, Mode) :-
        define_dynamic_predicates(PredIndicator).
compile_clause_compilation_directive(ensure_loaded(Spec), Mode, Mode) :-
        ensure_loaded(Spec).
compile_clause_compilation_directive(module(ModuleName, Exports), Mode, Mode) :-
        define_current_module(ModuleName, Exports).
compile_clause_compilation_directive(use_module(Spec), Mode, Mode) :-
        define_use_module(Spec).
compile_clause_compilation_directive(reexport(Spec), Mode, Mode) :-
        define_reexport(Spec).
compile_clause_compilation_directive(meta_predicate(Term), Mode, Mode) :-
        define_meta_predicate(Term).
compile_clause_compilation_directive(if(Goal), ModeIn, ModeOut) :-
        macro_if(Goal, ModeIn, ModeOut).
compile_clause_compilation_directive(else, ModeIn, ModeOut) :-
        macro_else(ModeIn, ModeOut).
compile_clause_compilation_directive(elseif(Goal), ModeIn, ModeOut) :-
        macro_elseif(Goal, ModeIn, ModeOut).
compile_clause_compilation_directive(endif, ModeIn, ModeOut) :-
        macro_endif(ModeIn, ModeOut).

macro_if(Goal, ModeIn, ModeOut) :-
        ModeIn = mode(IfLevel, Action), % IfLevel = 0 outside outermost :- if...
        IfLevelNext is IfLevel +1,
        (Action = skip(SkipLevel), SkipLevel < IfLevelNext
           -> ModeOut = mode(IfLevel, skip(SkipLevel))
         ;
         Action = skip(SkipLevel), SkipLevel >= IfLevelNext
           -> throw('if skip level >= iflevelnext')
         ;
         call(Goal)
           -> ModeOut = mode(IfLevelNext, compile(IfLevelNext))
         ;
         ModeOut = mode(IfLevelNext, skip(IfLevelNext)) % look for else, elseif, endif directives.
        ).

macro_else(ModeIn, ModeOut) :-
        ModeIn = mode(IfLevel, Action), % IfLevel = 0 outside outermost :- if...
        (
        Action = skip(SkipLevel), IfLevel > SkipLevel % IfLevel of this :- else is deeper than the skip target level.
          -> ModeOut = ModeIn
        ;
        Action = skip(SkipLevel), IfLevel = SkipLevel % IfLevel of this :- else is same as the skip target level.
          -> ModeOut = mode(IfLevel, compile(IfLevel)) % compile to the next :- endif at target level IfLevel.
        ;
        Action = skip(SkipLevel), IfLevel < SkipLevel % this is an error. Malformed endif/else
          -> throw('else ifLevel less than skiplevel')
        ;
        Action = compile(CompileLevel), IfLevel > CompileLevel
          -> throw('else ifLevel greater than compileLevel')
        ;
        Action = compile(CompileLevel), IfLevel = CompileLevel
          -> ModeOut = mode(IfLevel, skip(IfLevel)) % skip the ':- else' clause to the next :- endif at IfLevel.
        ;
        Action = compile(CompileLevel), IfLevel < CompileLevel
          -> throw('else ifLevel less than compileLevel')
        ).

macro_elseif(Goal, ModeIn, ModeOut) :-
        ModeIn = mode(IfLevel, Action), % IfLevel = 0 outside outermost :- if...
        (Action = compile(CompileLevel), IfLevel > CompileLevel
           -> throw('else ifLevel greater than compileLevel')
        ;
        Action = compile(CompileLevel), IfLevel = CompileLevel
           -> ModeOut = mode(IfLevel, skip(IfLevel)) % skip the ':- else' clause to the next :- endif at IfLevel.
        ;
        Action = compile(CompileLevel), IfLevel < CompileLevel
          -> throw('else ifLevel less than compileLevel')
        ;
        Action = skip(SkipLevel), SkipLevel < IfLevel
          -> ModeOut = mode(IfLevel, skip(SkipLevel))
        ;
        Action = skip(SkipLevel), SkipLevel > IfLevel
          -> throw('elseif skip level > iflevel')
        ;
        call(Goal)
          -> ModeOut = mode(IfLevel, compile(IfLevel))
        ;
        ModeOut = mode(IfLevel, skip(IfLevel)) % look for else, elseif, endif directives.
        ).

macro_endif(ModeIn, ModeOut) :-
        ModeIn = mode(IfLevel, Action), % IfLevel = 0 outside outermost :- if...
        IfLevelNext is IfLevel - 1,
        (Action = skip(SkipLevel), SkipLevel = IfLevel
          -> ModeOut = mode(IfLevelNext, compile(IfLevelNext))
        ;
        Action = skip(SkipLevel), SkipLevel \= IfLevel
          -> throw('if skip level not = iflevel')
        ;
        Action = compile(CompileLevel), CompileLevel = IfLevel
          -> ModeOut = mode(IfLevelNext, compile(IfLevelNext))
        ;
        Action = compile(CompileLevel), CompileLevel \= IfLevel
          -> throw('if compile level not = iflevel')
        ;
        true
          -> throw('endif unknown action')
        ).

define_dynamic_predicates(Name/Arity) :-
        !,
        define_dynamic_predicate1(Name/Arity).
define_dynamic_predicates([]).
define_dynamic_predicates([H|T]) :-
        !,
        define_dynamic_predicates(H),
        define_dynamic_predicates(T).
define_dynamic_predicates((A,B)) :-
        define_dynamic_predicates(A),
        define_dynamic_predicates(B).

define_dynamic_predicate1(Functor/Arity) :-
        current_compilation_module(Module),
        transform_predicate_name(Functor, Arity, Module, TransformedFunctor),
        define_dynamic_predicate(TransformedFunctor/Arity).


define_current_module(Name, Exports) :-
        current_compilation_stream(Stream),
        push_current_compilation_module(Name, Stream),
        define_module_export(Exports, Name),
        (Name \= system
          -> define_use_module(library(system)) % autoload system module into every (non-system) module.
        ;
         true
        ).

define_module_export([], _).
define_module_export([H|T], Name) :-
        add_module_export(Name, H),
        define_module_export(T, Name).

current_compilation_module(Name) :-
        current_compilation_module(Name, _).

current_compilation_module(Name, Stream) :-
        '$current_compilation_module'([Name-Stream|_]),
        !.
current_compilation_module(user, none).

push_current_compilation_module(Name, Stream) :-
  (retract('$current_compilation_module'(Names))
    -> true
   ;
   Names = []
  ),
  asserta('$current_compilation_module'([Name-Stream|Names])).

pop_current_compilation_module(Name, Stream) :-
  retract('$current_compilation_module'([Name-Stream|Names])),
  asserta('$current_compilation_module'(Names)).

define_use_module(Spec) :-
        setup_use_module(Spec, ImportName),
        current_compilation_module(Name, _),
        (Name = ImportName
          -> throw(cyclic_module(Name))
        ;
        assertz('$import'(Name, ImportName))
        ).

setup_use_module(library(LibraryName), LibraryName) :-
        !,
        (module_export(LibraryName, _)
          -> true
        ;
        LibraryName = system
          -> true
        ;
        library_name_to_path(LibraryName, Path),
        load_file_for_use_module(Path)
        ).
setup_use_module(Path, ImportName) :-
        path_to_module_name(Path, ImportName),
        canonical_source(Path, CanonicalPath),
        ('$loaded'(CanonicalPath)
          -> true
        ;
        compile_file(Path)
        ).

:- if((current_predicate(wam_compiler:current_compile_url/1), current_compile_url(_))).
load_file_for_use_module(Path) :-
        consult([Path]).
:- else.
load_file_for_use_module(Path) :-
        compile_file(Path).
:- endif.

use_module_imports(library(LibraryName), Imports) :-
        !,
        setof(Import, module_export(LibraryName, Import), Imports).
use_module_imports(Path, Imports) :-
        path_to_module_name(Path, ImportName),
        setof(Import, module_export(ImportName, Import), Imports).

define_reexport([]).
define_reexport([Spec|Specs]) :-
        define_reexport(Spec, all),
        define_reexport(Specs).

define_reexport(Spec, all) :-
        define_use_module(Spec),
        (use_module_imports(Spec, Imports)
          -> current_compilation_module(Module),
             define_module_export(Imports, Module)
        ;
         writeln(no_imports(Spec))
        ).

define_meta_predicate(((A, B))) :-
        !,
        define_meta_predicate(A),
        define_meta_predicate(B).
define_meta_predicate(Term) :-
        Term =.. [Functor|MetaArgTypes],
        list_length(MetaArgTypes, Arity),
        current_compilation_module(Name),
        transform_predicate_name(Functor, Arity, Name, TransformedFunctor),
        assertz('$meta_predicate'(TransformedFunctor, Arity, MetaArgTypes)).

defined_meta_predicate(Functor, Arity, MetaArgTypes) :-
        '$meta_predicate'(Functor, Arity, MetaArgTypes)
        ;
        \+ var(Functor),
        \+ '$meta_predicate'(Functor, Arity, MetaArgTypes),
        list_length(MetaArgTypes, Arity),
        default_meta_arg_types(MetaArgTypes, (?)).

clear_imports :-
        retractall('$current_import'(_)).

current_import(UnqualifiedPredicateName, Arity, ImportModuleName) :-
        current_compilation_module(CurrentModuleName),
        current_import(UnqualifiedPredicateName, Arity, CurrentModuleName, ImportModuleName),
        !,
        CurrentModuleName \= ImportModuleName.

% The system module is implemented specially: it is assigned exported predicates directly by the
% reserve_predicate/2 predicate, and it is assigned exported predicates by reexport of
% 'primitive' modules. These primitive modules do not 'use' any other modules.
% Because of this special implementation the recursion through current_import/4
% stops at the primitive modules imported by 'system'.

current_import(UnqualifiedPredicateName, Arity, ImportingModuleName, ImportModuleName) :-
        (ImportingModuleName == user
          -> ImportModuleNameINTERIM = system
         ;
        '$import'(ImportingModuleName, ImportModuleNameINTERIM)
        ),
        module_export(ImportModuleNameINTERIM, UnqualifiedPredicateName/Arity)
          -> (ImportingModuleName = system
                -> ImportModuleNameINTERIM = ImportModuleName
             ;
              current_import1(UnqualifiedPredicateName, Arity, ImportModuleNameINTERIM, ImportModuleName)
             )
        ;
        ImportingModuleName = ImportModuleName.

current_import1(UnqualifiedPredicateName, Arity, ImportModuleNameINTERIM, ImportModuleName) :-
        current_import(UnqualifiedPredicateName, Arity, ImportModuleNameINTERIM, ImportModuleName).

default_meta_arg_types([], _Flag).
default_meta_arg_types([Flag|T], Flag) :-
        default_meta_arg_types(T, Flag).

% if Functor already has ':' in it, then do nothing.
% otherwise create 'Module:Functor' as new name.

transform_predicate_name(Functor, Arity, ModuleName, TransformedFunctor) :-
        atom_codes(Functor, FunctorCodes),
         ":" = [ColonCode],
        (append(_FunctorModuleNameCodes, [ColonCode|_FunctorNameCodes], FunctorCodes)
           -> Functor = TransformedFunctor
         ;
         current_import(Functor, Arity, ModuleName, ImportedModuleName)
           -> transform_predicate_name1(ImportedModuleName, ColonCode, FunctorCodes, TransformedFunctor)
         ;
         transform_predicate_name1(ModuleName, ColonCode, FunctorCodes, TransformedFunctor)
        ).

% A system directive is evaluated immediately
% during compilation of a source unit (e.g. a file).
% The directive is also evaluated when the
% compiled WAM state is loaded and initialized.
% All of the system directives are evaluated
% in the order encountered in the source.
% They are evaluated prior to evaluating
% any initialization directives.

compile_clause_system_directive(Goal) :-
        call(Goal),
        generate_system_goal(Init),
        Term = (Init :- Goal),
        compile_clause_2(Term),
        save_clause(Term).

% An initialization directive is evaluated after
% the compilation of the source unit (e.g. a file)
% in which it is defined.
% The initialization directive is also evaluated when the
% compiled WAM state is loaded and initialized.
% All of the initialization directives are evaluated
% in the order encountered in the source.
% They are evaluated after evaluating
% any system directives.

compile_clause_initialization_directive(Goal) :-
        assertz(delayed_initialization(Goal)),
        generate_initialization_goal(Init),
        Term = (Init :- Goal),
        compile_clause_2(Term),
        save_clause(Term).


compile_clause_2(?- Body):-
        !,
        transform_body(Body, first, Transformed1, ExtraClauses, [], CutVariable),
        commit_to_cut(CutVariable),
        entail(Transformed1, Transformed),
        permanent_variable_list(true(CutVariable), Transformed, PermanentVariables),
        environment_size_required(PermanentVariables, Transformed, EnvSize),
        allocate_environment(EnvSize, CutVariable, PermanentVariables, Opcodes, O1, [next(Arity)], State),
        first_goal_arity(Body, Arity),
        current_compilation_module(ModuleName),
        compile_body(Transformed, ModuleName, PermanentVariables, EnvSize, State, _, O1, O2),
        compile_auxiliary_goals(ExtraClauses, ModuleName, O2, []),
        reset_compile_buffer,
        assemble(Opcodes, 2). % Note that a query is just compiled into the buffer and left there

compile_clause_2(Head :- Body):-
        !,
        transform_body(Body, first, Transformed1, ExtraClauses, [], CutVariable),
        commit_to_cut(CutVariable),
        entail(Transformed1, Transformed),
        permanent_variable_list(Head-CutVariable, Transformed, PermanentVariables),
        environment_size_required(PermanentVariables, Transformed, EnvSize),
        first_goal_arity(Body, BodyArity),
        compile_head(Head, BodyArity, CutVariable, PermanentVariables, EnvSize, State, Opcodes, O1),
        % Do NOT try and change the state size here! Space for the first goal and head are both set
        % aside in compile_head. Just because the head has only N arguments does NOT mean that it
        % only uses N registers!
        current_compilation_module(ModuleName),
        compile_body(Transformed, ModuleName, PermanentVariables, EnvSize, State, _, O1, O2),
        compile_auxiliary_goals(ExtraClauses, ModuleName, O2, []),
        reset_compile_buffer,
        !,
        assemble(Opcodes, 2),
        (Head = term_expansion(_,_)
          -> handle_term_expansion(Head :- Body) % assert term_expansion(_,_) if compiler is being run by SWI-Prolog.
        ;
        true
        ).


compile_clause_2(Head):-
        compile_head(Head, 0, no_cut, [], none, _State, Opcodes, [proceed]),
        reset_compile_buffer,
        assemble(Opcodes, 2),
        (Head = term_expansion(_,_)
          -> handle_term_expansion(Head) % assert term_expansion(_,_) if compiler is being run by SWI-Prolog.
        ;
        true
        ).


next_free_variable([next(A)|_], A):- !.
next_free_variable([_|S], A):- next_free_variable(S, A).


commit_to_cut(no_cut):- !.
commit_to_cut(has_cut(_)).

first_goal_arity((A,_), N):- !, first_goal_arity(A, N).
first_goal_arity(A, N):- functor(A, _, N).

transform_body(Body, Position, NewBody, ExtraClauses, Tail, CutVariable):-
        current_predicate(goal_expansion/2),
        goal_expansion(Body, B1),
        B1 \= Body,
        !,
        transform_body(B1, Position, NewBody, ExtraClauses, Tail, CutVariable).

transform_body((Goal, Goals), Position, (NewGoal, NewGoals), ExtraClauses, Tail, CutVariable):-
        !,
        transform_body(Goal, Position, NewGoal, ExtraClauses, T1, CutVariable),
        transform_body(Goals, not_first, NewGoals, T1, Tail, CutVariable).

transform_body(once(Goal), _Position, aux_head(Aux, Variables), [aux_definition(Aux, Variables, local_cut(LocalCutVariable), (Goal1, !(LocalCutVariable)))|ExtraClauses], Tail, CutVariable):-
        !,
        transform_body(Goal, not_first, Goal1, ExtraClauses, Tail, CutVariable),
        term_variables(Goal, V1),
        include_cut_point_as_argument_if_needed(CutVariable, V1, Variables).

% setup_call_catcher_cleanup(Setup, Goal, Catcher, Cleanup).
% This requires a bit of explanation since it is far from obvious how I have implemented it.
% First, we compile the goal into the following: (Operations in angle-brackets are special-purpose opcodes)
% setup_call_catcher_cleanup(Setup, Goal, Catcher, Cleanup):- call(Setup), aux_1.
% aux_1:- <get_choicepoint(B)>, mark_top_choicepoint(CleanupVars, Mark), aux_3, call(Goal), <get_choicepoint(B1)>, aux2(B, B1, Mark).
% aux_1:- Catcher = fail, call(Cleanup), !, fail.
% aux_2(B, B1, Mark):- B == B1, Catcher = exit, call(Cleanup), unmark_choicepoint(Mark).
% aux_2(_,_,_).
% aux_3:- Catcher = !, call(Cleanup), halt.

% Some interesting things:
% A) aux_1, before even calling the goal, appears to call aux_3. The reason for this is that mark_top_choicepoint/2 is extralogical, and saves the information about the *next instruction* to the stack, so that we can go back to it when a cut is detected.
%    >> It then skips the next instruction!
% B) aux_3, the ! case, calls halt/0.
%    This is because we run the aux_3 code with a completely new state (almost - the heap and stack are still present, but all the registers, backtrack pointer, etc, are empty). When the cleanup has finished running, it must resume execution from where we
%    left off, and it does this by exiting out of the main wam() loop by executing halt/0.
% C) aux_3 is called with no arguments, but defined with some. This is because we must have aux_3 immediately after the mark_top_choicepoint/2 call, without any put_* or unify_* instructions. mark_top_choicepoint/2 knows how many arguments there are because
%    they're passed in as an argument.
% D) The catch/2 introduces 2 choicepoints. This is why to check for determinism we must compare state.B before the goal and the third-to-top state.B afterwards!
% E) Because Goal1 may include a !, we can only be sure that the top choicepoint after Goal1 (ie B2) is equal to *or less than* the original one if the goal is deterministic.
%       We can shield the ! from leaking out of the whole goal, but not leaking out of the aux itself. This is why we have >=(B,B1) and not =(B,B1) in aux2.
transform_body(setup_call_catcher_cleanup(Setup, Goal, Catcher, Cleanup), _Position, (Setup1, goal(get_current_block(Block)), aux_head(Aux1, [Block|V1])), AuxClauses, Tail, CutVariable):-
        !,
        AuxClauses = [aux_definition(Aux1, [B1|V1], no_local_cut, (goal(mark_top_choicepoint(MarkVariables, Mark)),
                                                                   aux_head(Aux3, []),
                                                                   goal(install_new_block(NewBlock)),
                                                                   aux_head(GoalAux, GoalVars),
                                                                   get_top_choicepoint(0, B2),
                                                                   goal(end_block(B1, NewBlock)),
                                                                   aux_head(Aux2, [NewBlock, B2, Mark|V2]))),
                      aux_definition(Aux1, [B1|V1], local_cut(CutVar), (goal(reset_block(B1)),
                                                                        goal(get_exception(Ball)),
                                                                        !(CutVar),
                                                                        goal(Catcher = exception(Ball)),
                                                                        Cleanup1,
                                                                        goal(unwind_stack))), % Since we did not clear the exception, this is like rethrowing it
                      aux_definition(Aux1, [_|V1], local_cut(CutVar), (goal(Catcher = fail),
                                                                       goal(unmark_top_choicepoint),
                                                                       Cleanup1,
                                                                       !(CutVar),
                                                                       goal(fail))),
                      aux_definition(GoalAux, GoalVars, MaybeLocalCut, Goal1),
                      aux_definition(Aux2, [B, B1, Mark|V2], no_local_cut, (goal(=(B, B1)),
                                                                                  goal(Catcher = exit),
                                                                                  goal(unmark_choicepoint(Mark)),
                                                                                  Cleanup1)),
                      aux_definition(Aux2, _, no_local_cut, goal(true)),
                      aux_definition(Aux3, MarkVariables, no_local_cut, (goal(Catcher = !), Cleanup1, goal(halt)))|Clauses],
        compile_message(need(Aux1)),
        term_variables(Catcher-Cleanup, MarkVariables),
        transform_body(Setup, not_first, Setup1, Clauses, C1, CutVariable),
        % Shield the parent choicepoint from cuts. Cuts inside the goal should be considered local to the aux1 clause (oddly enough?)
        transform_body(Goal, not_first, Goal1, C1, C2, NewVar),
        instantiate_local_cut(MaybeLocalCut, NewVar), % This is run backwards!
        term_variables(Goal, GoalVars),
        transform_body(Cleanup, not_first, Cleanup1, C2, Tail, CutVariable),
        term_variables(Goal-Catcher-Cleanup, V1a),
        include_cut_point_as_argument_if_needed(CutVariable, V1a, V1),
        term_variables(Catcher-Cleanup, V2a),
        include_cut_point_as_argument_if_needed(CutVariable, V2a, V2).


% catch/3
% catch(Goal, Catcher, Recovery) is translated as follows:
% get_current_block(Block), aux_1(Goal, Catcher, Recovery, Block).
% aux_1(Goal, Catcher, Recovery, Block):- install_new_block(NewBlock), <compilation of Goal>, end_block(Block, NewBlock).
% aux_1(_, Catcher, Recovery, Block):- reset_block(Block), get_exception(Ball), aux_2(Ball, Catcher, Recovery).
% aux_2(Ball, Ball, Recovery):- !, clear_exception, <compilation of Recovery>
% aux_2(_, _, _):- unwind_stack.

transform_body(catch(Goal, Catcher, Recovery), _Position, (goal(get_current_block(Block)), aux_head(Aux1, [Block, Catcher|V1])), [aux_definition(Aux1, [B1, _|V1], no_local_cut, (goal(install_new_block(NewBlock)), Goal1, goal(end_block(B1, NewBlock)))),
                                                                                                                                                               aux_definition(Aux1, [B2, Catcher|V1], no_local_cut, (goal(reset_block(B2)), goal(get_exception(Ball)), aux_head(Aux2, [Ball, Catcher|V1]))),
                                                                                                                                                               aux_definition(Aux2, [Ball, Catcher|V1], local_cut(CutVar), (goal(Ball = Catcher), !(CutVar), goal(clear_exception), Recovery1)),
                                                                                                                                                               aux_definition(Aux2, [_, _|V1], no_local_cut, (goal(unwind_stack)))|Clauses], Tail, CutVariable):-
        transform_body(Goal, not_first, Goal1, Clauses, C1, CutVariable),
        transform_body(Recovery, not_first, Recovery1, C1, Tail, CutVariable),
        term_variables(Goal-Recovery, V1a),
        include_cut_point_as_argument_if_needed(CutVariable, V1a, V1).

% forall/2
% forall(Cond, Action) is simply translated as \+(Cond, \+Action).
transform_body(forall(Cond, Action), Position, Translated, [aux_definition(Aux, Variables, no_local_cut, AuxBody)|ExtraClauses], Tail, CutVariable):-
        !,
        transform_body(\+aux_head(Aux, Variables), Position, Translated, ExtraClauses, E1, CutVariable),
        transform_body((Cond, \+Action), not_first, AuxBody, E1, Tail, CutVariable),
        term_variables(Cond-Action, V1),
        include_cut_point_as_argument_if_needed(CutVariable, V1, Variables).

transform_body((A->B ; C), _Position, aux_head(Aux, Variables), [aux_definition(Aux, Variables, local_cut(LocalCutVariable), (AA, !(LocalCutVariable), BB)),
                                                                 aux_definition(Aux, Variables, no_local_cut, CC)|ExtraClauses], Tail, CutVariable):-
        !,
        transform_body(A, not_first, AA, ExtraClauses, T1, CutVariable),
        transform_body(B, not_first, BB, T1, T2, CutVariable),
        transform_body(C, not_first, CC, T2, Tail, CutVariable),
        term_variables(AA-BB-CC, V1),
        include_cut_point_as_argument_if_needed(CutVariable, V1, Variables).


transform_body((A -> B), _Position, aux_head(Aux, Variables), [aux_definition(Aux, Variables, local_cut(LocalCutVariable), (AA, !(LocalCutVariable), BB))|ExtraClauses], Tail, CutVariable):-
        !,
        transform_body(A, not_first, AA, ExtraClauses, T1, CutVariable),
        transform_body(B, not_first, BB, T1, Tail, CutVariable),
        term_variables(AA-BB, V1),
        include_cut_point_as_argument_if_needed(CutVariable, V1, Variables).

transform_body((LHS ; RHS), _Position, aux_head(Aux, Variables), [aux_definition(Aux, Variables, no_local_cut, LHS1), aux_definition(Aux, Variables, no_local_cut, RHS1)|Clauses], Tail, CutVariable):-
        !,
        transform_body(LHS, not_first, LHS1, Clauses, C1, CutVariable),
        transform_body(RHS, not_first, RHS1, C1, Tail, CutVariable),
        term_variables(LHS-RHS, V1),
        include_cut_point_as_argument_if_needed(CutVariable, V1, Variables).

transform_body(\+(Goal), _Position, aux_head(Aux, Variables), [aux_definition(Aux, Variables, local_cut(LocalCutVariable), (Goal1, !(LocalCutVariable), goal(fail))), aux_definition(Aux, Variables, no_local_cut, goal(true))|T1], Tail, _):-
        !,
        transform_body(Goal, not_first, Goal1, T1, Tail, CutVariable),
        term_variables(Goal, V1),
        include_cut_point_as_argument_if_needed(CutVariable, V1, Variables).

transform_body(!, not_first, !(CutVariable), Tail, Tail, has_cut(CutVariable)):- !.
transform_body(aux_head(Aux, Variables), _Position, aux_head(Aux, Variables), Tail, Tail, _):- !.
transform_body(goal(Goal), _Position, goal(Goal), Tail, Tail, _):- !.
transform_body(Goal, _Position, goal(Goal), Tail, Tail, _).

include_cut_point_as_argument_if_needed(CutVariable, V1, Variables):-
        var(CutVariable), !, Variables = V1.
% If Variables already includes CutVariable then do not include it a second time. This can happen if auxiliary clauses are nested several layers deep - we only need
% a single link to the main clause
include_cut_point_as_argument_if_needed(has_cut(CutVariable), V1, V1):-
        variable_is_in_list(CutVariable, V1), !.
include_cut_point_as_argument_if_needed(has_cut(CutVariable), V1, [CutVariable|V1]).

compile_auxiliary_goals([], _ModuleName, O, O):- !.
compile_auxiliary_goals([aux_definition(Label, Variables, LocalCutVariable, Body1)|Aux], ModuleName, [aux_label(Label)|Opcodes], Tail):-
        entail(Body1, Body),
        instantiate_local_cut(LocalCutVariable, LocalCut),
        permanent_variable_list(Variables-LocalCut, Body, PermanentVariables),
        environment_size_required(PermanentVariables, Body, EnvSize),
        list_length(Variables, Arity),
        first_goal_arity(Body, BodyArity),
        include_local_cut_in_arity(Arity, LocalCut, A0),
        A1 is A0 + BodyArity,
        allocate_environment(EnvSize, LocalCut, PermanentVariables, Opcodes, O1, [next(A1)], State),
        compile_head_args(Variables, State, S1, 0, PermanentVariables, O1, O2),
        compile_body(Body, ModuleName, PermanentVariables, EnvSize, S1, _, O2, O3),
        compile_auxiliary_goals(Aux, ModuleName, O3, Tail).

include_local_cut_in_arity(N, no_cut, N):- !.
include_local_cut_in_arity(N, has_cut(_), NN):- NN is N+1.

instantiate_local_cut(no_local_cut, no_cut):- !.
instantiate_local_cut(local_cut(V), has_cut(V)).

environment_size_required(Variables, (_,_), Length):-
        !,
        list_length(Variables, Length).
environment_size_required(Variables, (A;_), Length):-
        environment_size_required(Variables, A, Length),
        Length \== none,
        !.
environment_size_required(Variables, (_;A), Length):-
        environment_size_required(Variables, A, Length),
        Length \== none,
        !.
environment_size_required(Variables, _, Length):-
        list_length(Variables, Length),
        Length > 0,
        !.
environment_size_required(_, _, none).

entail(Goal, Flattened):-
        last_goal(Goal, Head, Last),
        !,
        entail(Head, Last, Flattened).
entail(Goal, Goal).

last_goal((A,B), (A, Z), C):- last_goal(B, Z, C), !.
last_goal((A,B), A, B):- !.

entail((Item, Tail), L, Flattened):-
        !,
        entail(Item, L1, Flattened),
        entail(Tail, L, L1).
entail(Item, Flattened, (Item, Flattened)).


permanent_variable_list(Head, (Goal1, Goals), Permanent):-
        !,
        grab_variables_from_goals((Head-Goal1, Goals), [], VariablesInSubgoals),
        list_length(VariablesInSubgoals, L),
        term_variables(VariablesInSubgoals, AllVariables),
        classify_variables(AllVariables, 0, _, VariablesInSubgoals, L, Permanent).

% Chain rules have no permanent variables.
permanent_variable_list(_, _, []):- !.

classify_variables([], I, I, _, _, []):- !.
classify_variables([Variable|Variables], I, N, VariablesInSubgoals, L, [permanent_var(Variable, y(I), last_occurrence(LastOccurrence))|Permanent]):-
        variable_is_permanent(Variable, 0, VariablesInSubgoals, 0, [], RevLastOccurrence),
        % If there are 2 goals, then a variable occurring in the last goal occurs in goal 1, since LastOccurrence is 0-based
        LastOccurrence is L - RevLastOccurrence - 1,
        !,
        II is I+1,
        classify_variables(Variables, II, N, VariablesInSubgoals, L, Permanent).

classify_variables([_|Variables], I, II, VariablesInSubgoals, L, Permanent):-
        classify_variables(Variables, I, II, VariablesInSubgoals, L, Permanent).

variable_is_permanent(_, 2, _, _, [_,I], I):- !.
variable_is_permanent(Variable, OccurrencesSoFar, [VariablesInAGoal|VariablesInOtherGoals], P, T, LastOccurrence):-
        variable_is_in_list(Variable, VariablesInAGoal),
        !,
        Occurrences is OccurrencesSoFar + 1,
        PP is P+1,
        variable_is_permanent(Variable, Occurrences, VariablesInOtherGoals, PP, [P|T], LastOccurrence).
variable_is_permanent(Variable, OccurrencesSoFar, [_|VariablesInOtherGoals], P, T, LastOccurrence):-
        PP is P+1,
        variable_is_permanent(Variable, OccurrencesSoFar, VariablesInOtherGoals, PP, T, LastOccurrence).

% grab_variables_from_goals returns the list of variables in reverse order so as to
% facilitate LCO
grab_variables_from_goals((Goal, Goals), A, B):-
        term_variables(Goal, Vars),
        !,
        grab_variables_from_goals(Goals, [Vars|A], B).
grab_variables_from_goals(LastGoal, A, [Vars|A]):-
        term_variables(LastGoal, Vars).

variable_is_in_list(Var, [V|_]):- V == Var, !.
variable_is_in_list(Var, [_|Vars]):- variable_is_in_list(Var, Vars).

%% compile_head(+Head, +BodyArgCount +HasCut, +PermanentVariables, +EnvSize, -State, -Opcodes, +Tail)
compile_head(Head, BodyArgCount, HasCut, PermanentVariables, EnvSize, State, Opcodes, Tail):-
        allocate_environment(EnvSize, HasCut, PermanentVariables, Opcodes, O1, [next(Reserved)], S1),
        Head =.. [_Functor|Args],
        list_length(Args, Arity),
        Reserved is Arity + BodyArgCount,
        compile_head_args(Args, S1, State, 0, PermanentVariables, O1, Tail).

allocate_environment(none, _, _, A, A, S, S):-!.
allocate_environment(_, no_cut, PermanentVariables, [allocate|Allocations], B, S, S1):- !,
        ensure_vars_allocated(PermanentVariables, Allocations, B, S, S1).
allocate_environment(_, has_cut(Var), PermanentVariables, [allocate|Allocations], C, S, [var(Var, get, R)|ST]):-
        ensure_vars_allocated(PermanentVariables, Allocations, [get_level(R)|C], S, ST),
        variable_is_known_permanent(Var, PermanentVariables, R), !.

%ensure_vars_allocated(_, T, T, S, S):- !.
ensure_vars_allocated([], Tail, Tail, S, S):- !.
ensure_vars_allocated([permanent_var(Variable, y(I), _)|PermanentVariables], [put_variable(y(I))|Opcodes], Tail, S, STail):-
        ensure_vars_allocated(PermanentVariables, Opcodes, Tail, [var(Variable, put, y(I))|S], STail).

deallocate_environment(none, A, A):-!.
deallocate_environment(_, [deallocate|A], A).


atom_or_empty_list(A):- A == [].
atom_or_empty_list(A):- atom(A).

compile_head_args([], S, S, _, _, A, A):-!.
compile_head_args([Arg|Args], State, S2, I, PermanentVariables, Opcodes, Tail):-
        compile_head_arg(Arg, State, S1, x(I), PermanentVariables, Opcodes, O1),
        II is I+1,
        compile_head_args(Args, S1, S2, II, PermanentVariables, O1, Tail).

compile_head_arg(Arg, State, State, Ai, _PermanentVariables, [get_constant(Arg, Ai)|Tail], Tail):-
        atom_or_empty_list(Arg),
        !.
compile_head_arg(Arg, State, State, Ai, _PermanentVariables, [get_integer(Arg, Ai)|Tail], Tail):-
        integer(Arg),
        !.
compile_head_arg(Arg, State, State, Ai, _PermanentVariables, [get_float(Arg, Ai)|Tail], Tail):-
        float(Arg),
        !.
compile_head_arg(Arg, State, S1, Ai, PermanentVariables, Opcodes, Tail):-
        var(Arg),
        !,
        get_variable(Arg, Ai, PermanentVariables, State, S1, Opcodes, Tail).

compile_head_arg([Head|Tail], State, S2, Ai, PermanentVariables, [get_list(Ai)|O1], OpcodesTail):-
        compile_head_unification([Head, Tail], State, S1, PermanentVariables, O1, O2, Unifications),
        complete_head_unification(Unifications, S1, S2, PermanentVariables, O2, OpcodesTail).

compile_head_arg(Arg, State, S2, Ai, PermanentVariables, [get_structure(Functor/Arity, Ai)|O1], Tail):-
        Arg =.. [Functor|Args],
        list_length(Args, Arity),
        compile_head_unification(Args, State, S1, PermanentVariables, O1, O2, Unifications),
        complete_head_unification(Unifications, S1, S2, PermanentVariables, O2, Tail).


compile_head_unification([], S, S, _, O, O, []):- !.
compile_head_unification([Arg|Args], State, S1, PermanentVariables, [unify_constant(Arg)|O1], Tail, Unifications):-
        atom_or_empty_list(Arg),
        !,
        compile_head_unification(Args, State, S1, PermanentVariables, O1, Tail, Unifications).
compile_head_unification([Arg|Args], State, S1, PermanentVariables, [unify_integer(Arg)|O1], Tail, Unifications):-
        integer(Arg),
        !,
        compile_head_unification(Args, State, S1, PermanentVariables, O1, Tail, Unifications).
compile_head_unification([Arg|Args], State, S1, PermanentVariables, [unify_float(Arg)|O1], Tail, Unifications):-
        float(Arg),
        !,
        compile_head_unification(Args, State, S1, PermanentVariables, O1, Tail, Unifications).
compile_head_unification([Arg|Args], State, S2, PermanentVariables, Opcodes, Tail, Unifications):-
        var(Arg),
        !,
        unify_variable(Arg, PermanentVariables, State, S1, Opcodes, O1),
        compile_head_unification(Args, S1, S2, PermanentVariables, O1, Tail, Unifications).
compile_head_unification([Arg|Args], State, S2, PermanentVariables, [unify_variable(Xi)|O1], Tail, [unify(Xi, Arg)|U]):-
        !,
        fresh_variable(State, S1, Xi),
        compile_head_unification(Args, S1, S2, PermanentVariables, O1, Tail, U).
complete_head_unification([], S, S, _PermanentVariables, Tail, Tail).
complete_head_unification([unify(Xi, Arg)|U], S1, S3, PermanentVariables, Opcodes, Tail):-
        compile_head_arg(Arg, S1, S2, Xi, PermanentVariables, Opcodes, O1),
        complete_head_unification(U, S2, S3, PermanentVariables, O1, Tail).

get_variable(Arg, Source, _PermanentVariables, State, State, [get_value(Register, Source)|Tail], Tail):-
        already_used(Arg, State, Register, _), !.
get_variable(Arg, Source, PermanentVariables, State, [var(Arg, get, y(I))|State], [get_variable(y(I), Source)|Tail], Tail):-
        variable_is_known_permanent(Arg, PermanentVariables, y(I)), !.
get_variable(Arg, Source, _PermanentVariables, State, [var(Arg, get, Xi)|S1], [get_variable(Xi, Source)|Tail], Tail):-
        !, fresh_variable(State, S1, Xi).

unify_variable(Arg, _PermanentVariables, State, S1, Opcodes, Tail):-
        already_used(Arg, State, Register, How),
        !,
        unify_possibly_local_variable(How, Register, Opcodes, Arg, State, S1, Tail).
unify_variable(Arg, PermanentVariables, State, [var(Arg, unify, y(I))|State], [unify_variable(y(I))|Tail], Tail):-
        variable_is_known_permanent(Arg, PermanentVariables, y(I)), !.
unify_variable(Arg, _PermanentVariables, State, [var(Arg, unify, Xi)|S1], [unify_variable(Xi)|Tail], Tail):-
        !, fresh_variable(State, S1, Xi).

put_variable(Arg, Position, Source, PermanentVariables, State, S1, Opcodes, Tail):-
        already_used(Arg, State, Register, How),
        !,
        put_possibly_unsafe_value(How, PermanentVariables, Position, Register, Source, Opcodes, Arg, State, S1, Tail).
put_variable(Arg, _, Source, PermanentVariables, State, [var(Arg, put, y(I))|State], [put_variable(y(I), Source)|Tail], Tail):-
        variable_is_known_permanent(Arg, PermanentVariables, y(I)), !.
put_variable(Arg, _, Source, _PermanentVariables, State, [var(Arg, put, Xi)|S1], [put_variable(Xi, Source)|Tail], Tail):-
        !, fresh_variable(State, S1, Xi).

% We need unify_local_value(Vn) rather than unify_value(Vn) if:
%   the variable Vn has not been initialized in this clause with set variable or unify variable, nor, if Vn is temporary, with put variable
unify_possibly_local_variable(unify, Register, [unify_value(Register)|Tail], _, S, S, Tail):- !.
unify_possibly_local_variable(set, Register, [unify_value(Register)|Tail], _, S, S, Tail):- !.
unify_possibly_local_variable(put, x(I), [unify_value(x(I))|Tail], _, S, S, Tail):- !.
unify_possibly_local_variable(_, Register, [unify_local_value(Register)|Tail], Arg, S, S1, Tail):-
        mark_variable_as_safe(Arg, S, S1).

% I think an X register can never be unsafe. We get this with chain rules like foo:- bar(X,X).
% In this case, we have put_variable, put_value. put_unsafe_value is designed to avoid using data in an environment which has been discarded. This is not happening here.
put_possibly_unsafe_value(_, _, _, x(I), Source, [put_value(x(I), Source)|Tail], _, S, S, Tail).
put_possibly_unsafe_value(get, _, _, Register, Source, [put_value(Register, Source)|Tail], _, S, S, Tail).
put_possibly_unsafe_value(unify, _, _, Register, Source, [put_value(Register, Source)|Tail], _, S, S, Tail).
put_possibly_unsafe_value(put, PermanentVariables, Position, Register, Source, [put_value(Register, Source)|Tail], Arg, S, S, Tail):-
        variable_has_not_been_trimmed(Arg, Position, PermanentVariables), !.

put_possibly_unsafe_value(put, _, _, Register, Source, [put_unsafe_value(Register, Source)|Tail], _, S, S, Tail).

variable_has_not_been_trimmed(Arg, Position, [permanent_var(A, _, last_occurrence(L))|_]):- Arg == A, !, L > Position.
variable_has_not_been_trimmed(Arg, Position, [_|PermanentVariables]):- variable_has_not_been_trimmed(Arg, Position, PermanentVariables).

% Variables initialized via put are never safe from the perspective of put[_local]_variable
% If they were initialized via unify, that is another story.
% FIXME: What happens then if we have foo(A):- bar(A), baz(A). A is permanent but the GC requires us to put in the put_variable(Yn). Will this wreck things?
mark_variable_as_safe(Arg, [var(A, put, Register)|Tail], [var(A, put, Register)|Tail]):- Arg == A, !.
mark_variable_as_safe(Arg, [var(A, _, Register)|Tail], [var(A, unify, Register)|Tail]):- Arg == A, !.
mark_variable_as_safe(Arg, [Head|Tail], [Head|Tail1]):- mark_variable_as_safe(Arg, Tail, Tail1).

variable_is_known_permanent(Variable, [permanent_var(V, R, _)|_], R):- Variable == V, !.
variable_is_known_permanent(Variable, [_|Vars], R):- variable_is_known_permanent(Variable, Vars, R).

fresh_variable([next(I)|Tail], [next(II)|Tail], x(I)):- II is I+1, !.
fresh_variable([A|B], [A|C], R):- fresh_variable(B, C, R).

already_used(Arg, [var(A, How, Register)|_], Register, How):- A == Arg, !.
already_used(Arg, [_|A], Register, How):- already_used(Arg, A, Register, How).

% The first goal in the body must NOT resize the state, since it is effectively part of the head still!
resize_state(0, S, _, S):- !.
resize_state(_, [next(_)|T], I, [next(I)|T]):- !.
resize_state(P, [A|T], I, [A|T2]):- resize_state(P, T, I, T2).


trim_environment(_, [], [], EnvSize, EnvSize):- !.
trim_environment(P, [permanent_var(_, _, last_occurrence(C))|PermanentVars], P1, EnvSize, TrimmedEnvSize):-
        C =< P,
        !,
        E1 is EnvSize - 1,
        trim_environment(P, PermanentVars, P1, E1, TrimmedEnvSize).

trim_environment(P, [Var|PermanentVars], [Var|P1], EnvSize, TrimmedEnvSize):-
        !,
        trim_environment(P, PermanentVars, P1, EnvSize, TrimmedEnvSize).

compile_body(Body, ModuleName, PermanentVariables, EnvSize, State, S2, Opcodes, OpcodesTail):-
        compile_body_goals(Body, ModuleName, 0, depart, PermanentVariables, EnvSize, State, S2, Opcodes, OpcodesTail).

% LCO is either 'call' or 'depart' and governs whether we permit the last goal to do LCO or whether it must actually be a call as well.
compile_body_goals((Goal, Goals), ModuleName, Position, LCO, PermanentVariables, EnvSize, State, S2, Opcodes, OpcodesTail):-
        !,
        compile_goal(Goal, ModuleName, Position, call, PermanentVariables, EnvSize, State, S1, Opcodes, O1),
        PP is Position + 1,
        trim_environment(Position, PermanentVariables, RemainingPermanentVariables, EnvSize, TrimmedEnvSize),
        compile_body_goals(Goals, ModuleName, PP, LCO, RemainingPermanentVariables, TrimmedEnvSize, S1, S2, O1, OpcodesTail).
compile_body_goals(LastGoal, ModuleName, Position, LCO, PermanentVariables, EnvSize, State, S1, Opcodes, OpcodesTail):-
        compile_goal(LastGoal, ModuleName, Position, LCO, PermanentVariables, EnvSize, State, S1, Opcodes, OpcodesTail).

compile_goal(aux_head(Label, Variables), ModuleName, Position, LCO, PermanentVariables, EnvSize, State, S2, Opcodes, OpcodesTail):-
        !,
        list_length(Variables, Arity),
        list_length(MetaArgTypes, Arity),
        default_meta_arg_types(MetaArgTypes, (?)),
        resize_state(Position, State, Arity, S1),
        compile_body_args(Variables, MetaArgTypes, ModuleName, Position, 0, PermanentVariables, S1, S2, Opcodes, O1),
        compile_aux_call(LCO, Arity, EnvSize, Label, O1, OpcodesTail).

% Note that get_top_choicepoint(n, Yn) must mark Yn as seen (similar to get_level).
compile_goal(get_top_choicepoint(N, B), _ModuleName, _Position, _LCO, PermanentVariables, _EnvSize, State, [var(B, get, y(Register))|State], [get_choicepoint(N, y(Register))|Tail], Tail):-
        variable_is_known_permanent(B, PermanentVariables, y(Register)).


% If ! is the first goal but NOT a depart, it is a neck_cut.
compile_goal(goal(!), _ModuleName, 0, call, _PermanentVariables, _EnvSize, State, State, [neck_cut|OpcodesTail], OpcodesTail):-
        !.
% If ! is the first goal AND depart, then this is basically a fact of the form head:- !.
compile_goal(goal(!), _ModuleName, 0, depart, _PermanentVariables, _EnvSize, State, State, [neck_cut, proceed|OpcodesTail], OpcodesTail):-
        !.
% If ! is not the first goal, then it is a deep cut
compile_goal(!(Var), _ModuleName, _Position, call, PermanentVariables, _EnvSize, State, State, [cut(y(Register))|OpcodesTail], OpcodesTail):-
        !,
        variable_must_be_known_permanent(Var, PermanentVariables, y(Register)).
% This case kind of wrecks LCO
compile_goal(!(Var), _ModuleName, _Position, depart, PermanentVariables, _EnvSize, State, State, [cut(y(Register)), deallocate, proceed|OpcodesTail], OpcodesTail):-
        !,
        variable_must_be_known_permanent(Var, PermanentVariables, y(Register)).

compile_goal(goal(SpecifiedModule:Goal), _ModuleName, Position, LCO, PermanentVariables, EnvSize, State, S2, Opcodes, OpcodesTail):-
        !,
        Goal =.. [Functor|Args],
        list_length(Args, Arity),
        resize_state(Position, State, Arity, S1),
        transform_predicate_name(Functor, Arity, SpecifiedModule, TransformedFunctor),
        defined_meta_predicate(TransformedFunctor, Arity, MetaArgTypes),
        compile_body_args(Args, MetaArgTypes, SpecifiedModule, Position, 0, PermanentVariables, S1, S2, Opcodes, O1),
        compile_predicate_call(LCO, EnvSize, TransformedFunctor, Arity, O1, OpcodesTail).

compile_goal(goal(Goal), ModuleName, Position, LCO, PermanentVariables, EnvSize, State, S2, Opcodes, OpcodesTail):-
        !,
        Goal =.. [Functor|Args],
        list_length(Args, Arity),
        resize_state(Position, State, Arity, S1),
        transform_predicate_name(Functor, Arity, ModuleName, TransformedFunctor),
        defined_meta_predicate(TransformedFunctor, Arity, MetaArgTypes),
        compile_body_args(Args, MetaArgTypes, ModuleName, Position, 0, PermanentVariables, S1, S2, Opcodes, O1),
        compile_predicate_call(LCO, EnvSize, TransformedFunctor, Arity, O1, OpcodesTail).

% This is possible. For example, consider ((foo, bar), baz).
compile_goal((LHS, RHS), ModuleName, Position, LCO, PermanentVariables, EnvSize, State, S2, Opcodes, OpcodesTail):-
        !,
        compile_body_goals((LHS, RHS), ModuleName, Position, LCO, PermanentVariables, EnvSize, State, S2, Opcodes, OpcodesTail).

compile_goal(AnythingElse, _, _, _, _, _, _, _, _, _):-
        throw(wrong_type_of_functor(compile_goal, AnythingElse)).

variable_must_be_known_permanent(Var, PermanentVariables, Register):-
        variable_is_known_permanent(Var, PermanentVariables, Register), !.
variable_must_be_known_permanent(Var, PermanentVariables, _):-
        throw(missing_permanent_var(Var, PermanentVariables)).

compile_predicate_call(depart, EnvSize, Functor, Arity, O1, O2):-
        !,
        deallocate_environment(EnvSize, O1, [execute(Functor/Arity)|O2]).
compile_predicate_call(call, EnvSize, Functor, Arity, [call(Functor/Arity, EnvSize)|O2], O2).

compile_aux_call(depart, ArgCount, EnvSize, Label, O1, O2):-
        deallocate_environment(EnvSize, O1, [execute_aux(Label, ArgCount)|O2]).
compile_aux_call(call, ArgCount, EnvSize, Label, O1, O2):-
        O1 = [call_aux(Label, ArgCount, EnvSize)|O2].

compile_body_args([], [], _ModuleName, _, _, _PermanentVariables, State, State, Tail, Tail):- !.
compile_body_args([Arg|Args], [MetaArgType|MetaArgTypes], ModuleName, Position, I, PermanentVariables, S1, S3, Opcodes, Tail):-
        compile_body_arg(Arg, MetaArgType, ModuleName, Position, x(I), PermanentVariables, S1, S2, OpcodesX, O1, GUV),
        compile_message(guv(GUV)),
        %OpcodesX = Opcodes,
        compile_body_arg_adjust(GUV, OpcodesX, Opcodes),
        II is I+1,
        compile_body_args(Args, MetaArgTypes, ModuleName, Position, II, PermanentVariables, S2, S3, O1, Tail).

compile_body_arg_adjust(GUV, OpcodesX, Opcodes) :-
        var(GUV),
        !,
        OpcodesX=Opcodes.
compile_body_arg_adjust(_GUV, OpcodesX, Opcodes) :-
        adjust_unify_variable(OpcodesX, Opcodes).

compile_body_arg(Arg, MetaArgType, ModuleName, Position, x(I), PermanentVariables, S1, S2, OpcodesX, O1) :-
        compile_body_arg(Arg, MetaArgType, ModuleName, Position, x(I), PermanentVariables, S1, S2, OpcodesX, O1, _).

compile_body_arg(Arg, MetaArgType, ModuleName, _Position, Dest, _PermanentVariables, State, State, [put_constant(TransformedArg, Dest)|Tail], Tail, _):-
        atom_or_empty_list(Arg), !,
        (atom(Arg),
         MetaArgType = (:)
           -> TransformedArg = Arg
         ;
         atom(Arg),
         integer(MetaArgType) % 0..9
           -> Arg =.. [Functor|SubArgs],
              length(SubArgs, ArgArity),
              Arity is ArgArity + MetaArgType,
              transform_predicate_name(Functor, Arity, ModuleName, TransformedFunctor),
              TransformedArg =.. [TransformedFunctor|SubArgs]
         ;
         Arg = TransformedArg
        ).
compile_body_arg(Arg, _MetaArgType, _ModuleName, _Position, Dest, _PermanentVariables, State, State, [put_integer(Arg, Dest)|Tail], Tail, _):-
        integer(Arg), !.
compile_body_arg(Arg, _MetaArgType, _ModuleName, _Position, Dest, _PermanentVariables, State, State, [put_float(Arg, Dest)|Tail], Tail, _):-
        float(Arg), !.
compile_body_arg(Arg, _MetaArgType, _ModuleName, Position, Dest, PermanentVariables, State, S1, Opcodes, Tail, _):-
        var(Arg), !, put_variable(Arg, Position, Dest, PermanentVariables, State, S1, Opcodes, Tail).
compile_body_arg([Head|Tail], _MetaArgType, ModuleName, Position, Dest, PermanentVariables, State, S1, Opcodes, OpcodesTail, GUV):-
        !,
        compile_body_unification([Head, Tail], ModuleName, Position, State, S1, PermanentVariables, Opcodes, [put_list(Dest)|R], R, OpcodesTail, GUV).

compile_body_arg(Arg, MetaArgType, ModuleName, Position, Dest, PermanentVariables, State, S1, Opcodes, Tail, GUV):-
        Arg =.. [Functor|Args],
        list_length(Args, Arity),
        (MetaArgType = (:)
           -> TransformedArgs = [ModuleName, Arg],
              TransformedFunctor = (:),
              TransformedArity = 2
         ;
         integer(MetaArgType) % 0..9
           -> MetaArity is Arity + MetaArgType,
              transform_predicate_name(Functor, MetaArity, ModuleName, TransformedFunctor),
              TransformedArgs = Args,
              TransformedArity = Arity
         ;
         MetaArgType = (^) % Arg is assumed to be MetaArgType of 0 - no extra args to be applied.
           -> existential_variables(Arg, Vars, Expression),
              transform_meta_expression(Expression, ModuleName, TransformedExpression),
              existential_variables(TransformedArg, Vars, TransformedExpression),
              TransformedArg =.. [TransformedFunctor|TransformedArgs],
              list_length(TransformedArgs, TransformedArity)
         ;
         Functor = TransformedFunctor,
         TransformedArgs = Args,
         TransformedArity = Arity
        ),
        compile_body_unification(TransformedArgs, ModuleName, Position, State, S1, PermanentVariables, Opcodes, [put_structure(TransformedFunctor/TransformedArity, Dest)|R], R, Tail, GUV).

transform_meta_expression((A, B), ModuleName, (TransformedA, TransformedB)) :-
        !,
        transform_meta_expression(A, ModuleName, TransformedA),
        transform_meta_expression(B, ModuleName, TransformedB).
transform_meta_expression(once(A), ModuleName, once(TransformedA)) :-
        !,
        transform_meta_expression(A, ModuleName, TransformedA).
% setup_call_catcher_cleanup(Setup, Goal, Catcher, Cleanup)
transform_meta_expression(setup_call_catcher_cleanup(Setup, Goal, Catcher, Cleanup), ModuleName,
            setup_call_catcher_cleanup(TransformedSetup, TransformedGoal, Catcher, TransformedCleanup)) :-
        !,
        transform_meta_expression(Setup, ModuleName, TransformedSetup),
        transform_meta_expression(Goal, ModuleName, TransformedGoal),
        transform_meta_expression(Cleanup, ModuleName, TransformedCleanup).
%catch(Goal, Catcher, Recovery)
transform_meta_expression(catch(Goal, Catcher, Recovery), ModuleName,
            catch(TransformedGoal, Catcher, TransformedRecovery)) :-
        !,
        transform_meta_expression(Goal, ModuleName, TransformedGoal),
        transform_meta_expression(Recovery, ModuleName, TransformedRecovery).
%forall(Cond, Action)
transform_meta_expression(forall(Cond, Action), ModuleName,
            forall(TransformedCond, TransformedAction)) :-
        !,
        transform_meta_expression(Cond, ModuleName, TransformedCond),
        transform_meta_expression(Action, ModuleName, TransformedAction).
%(A->B ; C)
transform_meta_expression((A->B ; C), ModuleName, (TransformedA->TransformedB;TransformedC)) :-
        !,
        transform_meta_expression(A, ModuleName, TransformedA),
        transform_meta_expression(B, ModuleName, TransformedB),
        transform_meta_expression(C, ModuleName, TransformedC).
%(A -> B)
transform_meta_expression((A->B), ModuleName, (TransformedA->TransformedB)) :-
        !,
        transform_meta_expression(A, ModuleName, TransformedA),
        transform_meta_expression(B, ModuleName, TransformedB).
%(LHS ; RHS)
transform_meta_expression((A; B), ModuleName, (TransformedA; TransformedB)) :-
        !,
        transform_meta_expression(A, ModuleName, TransformedA),
        transform_meta_expression(B, ModuleName, TransformedB).
%\+(Goal)
transform_meta_expression(\+(A), ModuleName, \+(TransformedA)) :-
        !,
        transform_meta_expression(A, ModuleName, TransformedA).
transform_meta_expression(Expression, ModuleName, TransformedExpression) :-
        Expression =.. [Functor|Args],
        list_length(Args, Arity),
        transform_predicate_name(Functor, Arity, ModuleName, TransformedFunctor),
        TransformedExpression =.. [TransformedFunctor|Args].


compile_body_unification([], _, _, State, State, _PermanentVariables, Opcodes, Opcodes, R, R, _):- !.
compile_body_unification([Arg|Args], ModuleName, Position, State, S1, PermanentVariables, O, OT, [unify_constant(Arg)|R], RT, GUV):-
        atom_or_empty_list(Arg), !, compile_body_unification(Args, ModuleName, Position, State, S1, PermanentVariables, O, OT, R, RT, GUV).
compile_body_unification([Arg|Args], ModuleName, Position, State, S1, PermanentVariables, O, OT, [unify_integer(Arg)|R], RT, GUV):-
        integer(Arg), !, compile_body_unification(Args, ModuleName, Position, State, S1, PermanentVariables, O, OT, R, RT, GUV).
compile_body_unification([Arg|Args], ModuleName, Position, State, S2, PermanentVariables, O, OT, R, RT, generated_unify_variable):-
        var(Arg), !,
        unify_variable(Arg, PermanentVariables, State, S1, R, R1),
        compile_body_unification(Args, ModuleName, Position, S1, S2, PermanentVariables, O, OT, R1, RT, _).
compile_body_unification([Arg|Args], ModuleName, Position, State, S3, PermanentVariables, O, OT, [unify_value(Xi)|R], RT, GUV):-
        fresh_variable(State, S1, Xi),
        compile_body_arg(Arg, (?), ModuleName, Position, Xi, PermanentVariables, S1, S2, O, O1, GUV),
        compile_body_unification(Args, ModuleName, Position, S2, S3, PermanentVariables, O1, OT, R, RT, GUV).

% if unify_value(x(N)) precedes unify_variable(x(N)) then swap them.
adjust_unify_variable(O, A) :-
        adjust_unify_variable(O, A,[], []).

adjust_unify_variable(O, O, Swap, UnifyVariables) :-
        var(O),
        !,
        compile_message(adjust(Swap, UnifyVariables)),
        adjust_unify_variable1(Swap, UnifyVariables).
adjust_unify_variable([unify_value(x(N))|O], [InstA|A], Swap, UnifyVariables) :-
        Inst = unify_value(x(N)),
         \+ member(_ - _/N, UnifyVariables),
         \+ member(Inst-_/_, Swap),
        !,
        adjust_unify_variable(O, A, [Inst-InstA/N|Swap], UnifyVariables).
adjust_unify_variable([unify_variable(x(N))|O], [InstA|A], Swap, UnifyVariables) :-
        Inst = unify_variable(x(N)),
        !,
        adjust_unify_variable(O, A, Swap, [Inst-InstA/N|UnifyVariables]).
adjust_unify_variable([Inst|O], [Inst|A], Swap, UnifyVariables) :-
        adjust_unify_variable(O, A, Swap, UnifyVariables).


adjust_unify_variable1(Swap, []) :-
        !,
        adjust_unify_variable11(Swap).
adjust_unify_variable1(Swap, UV) :-
        adjust_unify_variable10(Swap, UV).

adjust_unify_variable10([], UnifyVariables) :-
        adjust_unify_variable2(UnifyVariables).
adjust_unify_variable10([Inst-InstVar/N|SwapT], UnifyVariables) :-
        member(InstVar-Inst/N, UnifyVariables),
        !,
        adjust_unify_variable10(SwapT, UnifyVariables).
adjust_unify_variable10([Inst-Inst/_N|SwapT], UnifyVariables) :-
        adjust_unify_variable10(SwapT, UnifyVariables).

adjust_unify_variable11([]).
adjust_unify_variable11([Inst-Inst/_N|SwapT]) :-
        adjust_unify_variable11(SwapT).

adjust_unify_variable2([]).
adjust_unify_variable2([InstVar-InstVar/_N|T])  :-
        !,
        adjust_unify_variable2(T).
adjust_unify_variable2([_|T])  :-
        adjust_unify_variable2(T).

compile_files([]):- !.
compile_files([File|Files]):-
        compile_file(File),
        !,
        compile_files(Files).


compile_file(Source):-
  %writeln(compile_file(Source)),
  canonical_source(Source, CanonicalSource),
  push_current_compile_url(CanonicalSource),
        open(CanonicalSource, read, S),
        compile_stream(S),
        close(S),
  pop_current_compile_url(CanonicalSource),
  retractall('$loaded'(CanonicalSource)), % clear old loaded fact, if any.
  assertz('$loaded'(CanonicalSource)).

current_compilation_stream(Stream) :-
        '$current_compilation_stream'([Stream|_]).

push_current_compilation_stream(Stream) :-
  (retract('$current_compilation_stream'(Streams))
    -> true
   ;
   Streams = []
  ),
  asserta('$current_compilation_stream'([Stream|Streams])).

pop_current_compilation_stream(Stream) :-
  retract('$current_compilation_stream'([Stream|Streams])),
  asserta('$current_compilation_stream'(Streams)).

compile_stream(Stream) :-
        push_current_compilation_stream(Stream),
        compile_stream(Stream, mode(0, compile(0))),
        pop_current_compilation_stream(Stream), % should pop the same stream that was pushed.
        (current_compilation_module(Module, Stream)
          -> pop_current_compilation_module(Module, Stream)
         ;
         true
        ).

compile_stream(Stream, Mode):-
        read_term(Stream, Term, []),
        %write('  '), writeln(read(Term)),
        compile_stream_term(Stream, Term, Mode).

% Originally this used 'forall(retract(delayed_initialization(Goal)), call(Goal))'.
% However, this caused some tests to perform very poorly. E.g. bench/nrev took almost 3 times longer (speed decreased from 3 Mlips to 1.2Mlips).
% The slowdown is puzzling since there were no initialization goals defined in the test.
compile_stream_term(_, end_of_file, Mode):-
        !,
        (Mode = mode(0, compile(0))
          -> (retract(delayed_initialization(Goal)),
              call_init(Goal),
              fail
             ;
              true
             )
        ;
        throw('macro error: missing endif directive.')
        ).
compile_stream_term(Stream, Term, ModeIn):-
        compile_clause(Term, ModeIn, ModeNext),
        !,
        compile_stream(Stream, ModeNext).

save_compiled_state(SavedStateFile) :-
        compiled_state_boot_code(BootCode),
        save_compiled_state(BootCode, SavedStateFile).

save_compiled_state(BootCode, SavedStateFile) :-
        open(SavedStateFile, write, S1),
        format(S1, 'function load_state() {~n', []),
        format(S1, 'bootstrap_code = ~w;~n', [BootCode]),
        format(S1, 'retry_foreign_offset = 7;~n', []),
%        format(S1, 'retry_foreign = {code: bootstrap_code, offset:7};~n', []),
        dump_tables(S1),
        format(S1, '}~n', []),
        close(S1),
        !.

call_init(Goal) :-
    %writeln(init(Goal)),
    call(Goal),
    !.


call_list([]).
call_list([H|T]) :-
        call(H),
        call_list(T).

compile_atoms([]).
compile_atoms([H|T]) :-
        compile_atom(H),
        compile_atoms(T).

compile_atom(Atom):-
        atom_to_memory_file(Atom, MemoryFile),
        compile_and_free_memory_file(MemoryFile),
        writeln('Compiled atom').

compile_and_free_memory_file(MemoryFile) :-
        %memory_file_description(MemoryFile, Description),
        %writeln(start_compile_memfile(Description)),
        compile_memory_file(MemoryFile),
        free_memory_file(MemoryFile).
        %writeln(end_compile_memfile(Description)).

compile_memory_file(MemoryFile) :-
%        wam_duration(D1),
%write('Start compile '), write(MemoryFile), write(': WAM duration = '), writeln(D1),
        open_memory_file(MemoryFile, read, Stream),
        compile_stream(Stream),
        close(Stream).
%        wam_duration(D2),
%write('Finish compile '), write(MemoryFile), write(': WAM duration = '), writeln(D2).

save_clause(Head:-Body):-
        !,
        functor(Head, Name, Arity),
        current_compilation_module(ModuleName),
        transform_predicate_name(Name, Arity, ModuleName, TransformedName),
        add_clause_to_predicate(TransformedName/Arity, Head, Body).

save_clause(Fact):-
        !,
        functor(Fact, Name, Arity),
        current_compilation_module(ModuleName),
        transform_predicate_name(Name, Arity, ModuleName, TransformedName),
        add_clause_to_predicate(TransformedName/Arity, Fact, true).

% first fetch all of the URLs, then compile them.
% This allows the client to get the URLs in parallel.

consult(URLs) :-
  canonical_sources(URLs, CanonicalURLs),
  fetch_promises(CanonicalURLs, Ps),
  compile_results(Ps).

canonical_sources([], []).
canonical_sources([H|T], [HC|TC]) :-
  canonical_source(H, HC),
  canonical_sources(T, TC).

canonical_source(S, BSC) :-
  atom_codes(S, Scodes),
  (append(_, ".pl", Scodes)
    -> SC = S
   ;
    append(Scodes, ".pl", SCcodes),
    atom_codes(SC, SCcodes)
  ),
  convert_URL_to_base(SC, BSC).

fetch_promises([], []).
fetch_promises([H|T], [H-HP|TP]) :-
    % writeln(fetching(H)),
    fetch_promise(H, HP),
    fetch_promises(T, TP).

compile_results([]).
compile_results([URL-Promise|T]) :-
  promise_result(Promise, R),
  push_current_compile_url(URL),
  compile_and_free_memory_file(R),
  pop_current_compile_url(URL),
  retractall('$loaded'(URL)), % clear old loaded fact, if any.
  assertz('$loaded'(URL)),
  compile_results(T).

%-------------------------- Finally we have a crude toplevel ----------------------
% swipl -f wam_compiler.pl -t "make, bootstrap('demo.pl', call_test), halt" && /System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc foreign.js wam.js bootstrap.js read.js -e "demo(false)"
print_bindings([]):- writeln('true.'), !.
print_bindings(Vars):-
        print_bindings_1(Vars).
print_bindings_1([]):- !.
print_bindings_1([Name=Value|Bindings]):-
        writeln(Name=Value),
        print_bindings_1(Bindings).

repl(Atom):-
        repl_1(Atom),
        true.

repl_1(Atom):-
        atom_to_term(Atom, Query, Bindings),
        wam_duration(Start),
        call(Query),
        notrace_backtrackable,
        wam_duration(End),
        flush_stdout,
        print_bindings(Bindings),
        Duration is (1000 * (End - Start)) // 1000,
        write_list(['(', Duration, 'ms)'], ''),
        writeln('').

call_atom(QueryAtom, Bindings) :-
    atom_to_term(QueryAtom, Query, Bindings),
    call(Query).
