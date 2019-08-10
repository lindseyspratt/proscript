:- module(wam_util, [transform_predicate_name1/4, path_to_module_name/2, list_length/2, library_name_to_path/2, library_path/1]).

transform_predicate_name1(ModuleName, ColonCode, FunctorCodes, TransformedFunctor) :-
         atom_codes(ModuleName, ModuleNameCodes),
         append(ModuleNameCodes, [ColonCode|FunctorCodes], TransformedCodes),
         atom_codes(TransformedFunctor, TransformedCodes).

library_path('../../library/').

library_name_to_path(Name, Path) :-
        library_path(LibraryPath),
        atom_concat(LibraryPath, Name, Path).

path_to_module_name(Path, ImportName) :-
        atom_codes(Path,PathCodes),
        path_to_file_name(PathCodes, NameCodes),
        strip_suffix(NameCodes, ".pl", StrippedCodes),
        atom_codes(ImportName, StrippedCodes).

path_to_file_name(PathCodes, NameCodes) :-
        "/" = [SlashCode],
        (append(_, [SlashCode|NameCodes], PathCodes),
        \+ member(SlashCode, NameCodes)
          -> true
        ;
        PathCodes = NameCodes
        ).

strip_suffix(NameCodes, SuffixCodes, StrippedCodes) :-
        append(StrippedCodes, SuffixCodes, NameCodes)
          -> true
        ;
        StrippedCodes = NameCodes.

list_length(A, B):-
        list_length_1(A, 0, B).

list_length_1([], N, N):- !.
list_length_1([_|A], N, L):-
        NN is N+1,
        list_length_1(A, NN, L).
