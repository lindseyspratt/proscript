:-ensure_loaded('../src/system/url').
:-ensure_loaded(listut).

:-op(300, xfx, =>).

substitute_template(InputFilePath, OutputFilePath) :-
    substitute_template1(InputFilePath, OutputCodes),
    open(OutputFilePath, write, OutputStream),
    write_codes(OutputCodes, OutputStream),
    close(OutputStream).

substitute_template1(InputFilePath, OutputCodes) :-
    file_to_list(InputFilePath, FileList),
    url_directory(InputFilePath, FileDir),
    process(FileDir, [], OutputCodes, [], FileList, []).

file_to_list(FilePath, FileList) :-
    open(FilePath, read, Stream),
    get_code(Stream, C),
    file_to_list1(C, Stream, FileList),
    close(Stream).

file_to_list1(-1, _, []) :- !.
file_to_list1(C, Stream, [C|T]) :-
    get_code(Stream, NextC),
    file_to_list1(NextC, Stream, T).

% Info = [dir - Dir, args - Args]. template_file(Path, [home]). template_arg(1, home) => 'class="active"'.
% Path : [home]. 1 : home => 'class="active"'.

template_file(FilePath, Args, Info) :-
    member(dir - FileDir, Info),
    member(result - Result, Info),
    member(result_tail - ResultTail, Info),
     resolve_url(FileDir, FilePath, ResolvedPath),
     file_to_list(ResolvedPath, SubList),
     url_directory(ResolvedPath, ResolvedDir),
     process(ResolvedDir, Args, Result, ResultTail, SubList, []).

template_arg(ID, Value, Info) :-
    member(args - Args, Info),
    nth1(ID, Args, Value).

template_not_arg(ID, Value, Info) :-
    member(args - Args, Info),
    \+ nth1(ID, Args, Value).

process(FileDir, Args, ProcessedFileList, ProcessedTail) -->
    "{{",
    file_path(FilePathCodes),
    !,
    {atom_codes(FilePath, FilePathCodes),
     term_to_atom(Term, FilePath),
     process_substitution(Term, FileDir, Args, ProcessedFileList, SubProcessedTail)
    },
    process(FileDir, Args, SubProcessedTail, ProcessedTail).
process(FileDir, Args, [Code|ProcessedFileList], ProcessedTail) -->
    [Code],
    !,
    process(FileDir, Args, ProcessedFileList, ProcessedTail).
process(_, _, T, T) -->
    [].

file_path([]) -->
    "}}".
file_path([H|T]) -->
    [H],
    file_path(T).

process_substitution(Term, FileDir, Args, ProcessedFileList, ProcessedTail) :-
    process_substitution1(Term, FileDir, Args, ProcessedFileList, ProcessedTail),
    !.

process_substitution1((Goal => Result), FileDir, Args, ProcessedFileList, ProcessedTail) :-
    !,
    (
    call(Goal, [dir - FileDir, args - Args])
      -> append_result(Result, ProcessedTail, ProcessedFileList)
    ;
    ProcessedTail = ProcessedFileList
    ).
process_substitution1(Goal, FileDir, Args, ProcessedFileList, ProcessedTail) :-
    call(Goal, [dir - FileDir, args - Args, result - ProcessedFileList, result_tail - ProcessedTail])
      -> true
    ;
    ProcessedTail = ProcessedFileList.

append_result(Term, Tail, List) :-
    (
    atom(Term)
      -> atom_codes(Term, TermCodes)
    ;
    number(Term)
      -> number_codes(Term, TermCodes)
    ;
    Term = TermCodes
    ),
    append(TermCodes, Tail, List).

write_codes([], _).
write_codes([H|T], Stream) :-
    put_code(Stream, H),
    write_codes(T, Stream).