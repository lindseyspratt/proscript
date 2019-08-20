:- module(wam_bootstrap_master, [build_saved_state/2, build_saved_state/3, bootstrap/2, bootstrap/3, bootstrap/4]).

:- meta_predicate((build_saved_state(+,0), build_saved_state(+,+,0))).

:- use_module('../system/wam_compiler').
:- use_module('../system/wam_assemble').
:- use_module('wam_bootstrap').
:- use_module('wam_bootstrap_util').

build_saved_state(SourceFiles, TopLevelQuery) :-
    build_saved_state(SourceFiles, 'proscriptls_state.js', TopLevelQuery).

build_saved_state(SourceFiles, SavedStateFile, TopLevelQuery):-
        reset_and_build_boot_code(BootCode),
        compile_clause(toplevel:-TopLevelQuery),
        ( compile_files(SourceFiles)->
            true
        ; otherwise->
            writeln(failed_to_compile),
            halt,
            fail
        ),
        !,
        save_compiled_state(BootCode, SavedStateFile).

reset_and_build_boot_code([0,255|MainBootCodes]) :-
        reset,
        assemble([call(toplevel/0,0), execute(halt/0), retry_foreign], 2),
        setof(N-Code, ctable(N, Code), SortedBootCodes),
        findall(Code, member(_-Code, SortedBootCodes), MainBootCodes).


% eg bootstrap('demo.pl', (factorial(5, X), writeln(X))).
% Ultimately, bootstrap('prolog.pl', prolog_toplevel).
bootstrap(Source, Query):-
        bootstrap('', [Source], Query).

bootstrap(CorePrefix, Sources, Query):-
        bootstrap(CorePrefix, Sources, 'proscriptls_state.js', Query).

bootstrap(CorePrefix, Sources, SavedStateFile, Query):-
        current_prolog_flag(version_data, swi(Mj, Mn, P, E)),
        (Mj >= 7 -> true;throw(wrong_swi_version('expected >= 7', swi(Mj, Mn, P, E)))),
        % Since javascript will not support open/3, we must load it into an atom and pass it.
        % Ultimately we could use XmlHTTPRequest, but probably that is less useful anyway
%        atom_concat(CorePrefix, 'debugger.pl', Debugger),
%        atom_concat(CorePrefix, 'bootstrap_js.pl', Bootstrap),
%        atom_concat(CorePrefix, 'url.pl', URL),
%        atom_concat(CorePrefix, 'not.pl', Not),
%        atom_concat(CorePrefix, 'promise.pl', Promise),
        atom_concat(CorePrefix, 'system.pl', System),
        atom_concat(CorePrefix, 'wam_compiler.pl', WAM),
        append([
%              Debugger,
%              Bootstrap,
%              URL,
%              Not,
%              Promise,
              System,
              WAM], Sources, AllFiles),

        build_saved_state(AllFiles, SavedStateFile, 'wam_compiler:bootstrap_toplevel'(Query)).
