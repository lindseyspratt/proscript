:-ensure_loaded('../src/system/url').
:-ensure_loaded(listut).
:-ensure_loaded(listut2).

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
% Path # [home]. 1 @ home => 'class="active"'.

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
    {\+ member(expand_templates-false, Args)},
    "{{",
    file_path(FilePathCodes),
    !,
    {atom_codes(FilePath, FilePathCodes),
     term_to_atom(Term, FilePath),
     process_substitution(Term, FileDir, Args, continue_on_fail, ProcessedFileList, SubProcessedTail)
    },
    process(FileDir, Args, SubProcessedTail, ProcessedTail).

process(FileDir, Args, ProcessedFileList, ProcessedTail) -->
    { member(language-prolog, Args)
      -> Start = "%"
      ;
      member(language-javascript, Args)
      -> Start = "//"},
    Start,
    rest_of_line(LineCodes),
    !,
    {
     append(Start, LineCodes, CommentLineCodes),
     append_lists(["<span style='color:green'>", CommentLineCodes, "</span><br>\n"], Prefix),
     append(Prefix, SubProcessedTail, ProcessedFileList)
    },
    process(FileDir, Args, SubProcessedTail, ProcessedTail).

process(FileDir, Args, NewList, ProcessedTail) -->
    {member(Keyword-Color, ["function"-"blue", ":-"-"blue"])},
    Keyword,
    !,
    {append_lists(["<span style='color:", Color, "'>", Keyword, "</span>"], AnnotatedKeyword),
     append(AnnotatedKeyword, ProcessedFileList, NewList)
    },
    process(FileDir, Args, ProcessedFileList, ProcessedTail).
process(FileDir, Args, NewList, ProcessedTail) -->
    [Code],
    !,
    {(member(html_escape-true, Args)
      -> html_escape([Code], OutCodes),
         append(OutCodes, ProcessedFileList, NewList)
    ;
     NewList = [Code|ProcessedFileList]
    )},
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

html_escape("<", "&lt;") :- !.
html_escape("\n", "<br>\n") :- !.
html_escape(" ", "&nbsp;") :- !.
html_escape(X, X).

rest_of_line(LineCodes) -->
    "\n"
      -> []
    ; [Code],
      {html_escape([Code], OutCodes),
       append(OutCodes, OtherCodes, LineCodes)},
      rest_of_line(OtherCodes).
