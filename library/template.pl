:-ensure_loaded('../src/system/url').
:-ensure_loaded(listut).

:-op(300, xfx, =>).
:-op(250, xfx, @).
:-op(250, xfx, @\+ ).
:-op(250, xfx, #).
:-op(350, xfy, &).


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

transform_template_goal(@(ID, Value), template_arg(ID, Value)) :- !.
transform_template_goal(@\+(ID, Value), template_not_arg(ID, Value)) :- !.
transform_template_goal(#(File, Args), template_file(File, Args)) :- !.
transform_template_goal(&(Goal1, Goal2), template_and(Goal1, Goal2)) :- !.
transform_template_goal(Goal, Goal).

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
    nth1(ID, Args, Value),
    default_result(Info).

template_not_arg(ID, Value, Info) :-
    member(args - Args, Info),
    \+ nth1(ID, Args, Value),
    default_result(Info).

template_and(Goal1, Goal2, Info) :-
    member(dir - FileDir, Info),
    member(args - Args, Info),
    member(result - Result, Info),
    member(result_tail - ResultTail, Info),
    (process_substitution(Goal1, FileDir, Args, stop_on_fail, Result, InterimTail)
      -> process_substitution(Goal2, FileDir, Args, continue_on_fail, InterimTail, ResultTail)
    ;
     default_result(Info)
    ).

default_result(Info) :-
    member(result - Result, Info),
    member(result_tail - ResultTail, Info)
      -> Result = ResultTail
    ;
    true.

process(FileDir, Args, ProcessedFileList, ProcessedTail) -->
    "{{",
    file_path(FilePathCodes),
    !,
    {atom_codes(FilePath, FilePathCodes),
     term_to_atom(Term, FilePath),
     process_substitution(Term, FileDir, Args, continue_on_fail, ProcessedFileList, SubProcessedTail)
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

process_substitution(Term, FileDir, Args, FailMode, ProcessedFileList, ProcessedTail) :-
    process_substitution1(Term, FileDir, Args, FailMode, ProcessedFileList, ProcessedTail),
    !.

process_substitution1((Goal => Result), FileDir, Args, FailMode, ProcessedFileList, ProcessedTail) :-
    !,
    (
    transform_template_goal(Goal, TGoal),
    call(TGoal, [dir - FileDir, args - Args])
      -> append_result(Result, ProcessedTail, ProcessedFileList)
    ;
    FailMode = continue_on_fail
      -> ProcessedTail = ProcessedFileList
    ).
process_substitution1(Goal, FileDir, Args, FailMode, ProcessedFileList, ProcessedTail) :-
    transform_template_goal(Goal, TGoal),
    call(TGoal, [dir - FileDir, args - Args, result - ProcessedFileList, result_tail - ProcessedTail])
      -> true
    ;
    FailMode = continue_on_fail
      -> ProcessedTail = ProcessedFileList.

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