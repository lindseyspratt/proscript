:- ensure_loaded('../library/object'). % for >>/2, etc.
:- ensure_loaded('../library/listut2'). % for append_lists/2
:- dynamic(likes/2).
:- initialization(init).

init :-
    _ >> [id -:> button, addEventListener(click, on_click)],
    _ >> [id -:> select, addEventListener(change, change_name)],
    _ >> [id -:> program, addEventListener(keyup, consult_text)],
    consult_text,
    change_name,
    writeln(completed).

% Since consult_text/0 is called on every key, there are many compile failures
% due to editing to the textarea being incomplete.
% the catch/3 metagoal allows consult_text/0 to ignore these exceptions
% by treating them as simple failures instead of recording them to the console log.

consult_text :-
    _ >> [id -:> program, value +:> Program],
    retractall(likes(_,_)),
    catch('wam_compiler:compile_atom'(Program), _, fail),
    update_select.

update_select :-
    setof(Name, X^likes(Name, X), Names),
    Select >> [id -:> select, innerHTML <:+ ""],
    add_options([everyone|Names], Select).

add_options([], _).
add_options([H|T], Select) :-
    create_dom_element('OPTION', Option),
    atom_codes(H, HCodes),
    create_dom_text_node(HCodes, TextNode),
    Option >+> value <: H,
    append_dom_node_child(Option, TextNode),
    append_dom_node_child(Select, Option),
    add_options(T, Select).

on_click :-
    _ >> [id -:> select, selectedIndex +:> Index, item(Index) *:> Option],
    Option >+> value :> Name,
    show_likes(Name).

show_likes(Name) :-
    _ >> [id -:> result, innerHTML <:+ ""],
    (Name \= everyone
      -> LocalName = Name
     ;
     true),
    findall(LocalName-Object, likes(LocalName, Object), Answers),
    show(Answers).

show(Answers) :-
    Result >> [id -:> result, innerHTML <:+ ""],
    combine_answers(Answers, Text),
    Result >+> innerHTML <: Text.

combine_answers([], []).
combine_answers([Name-Object|T], Text) :-
    atom_codes(Name, NameCodes),
    atom_codes(Object, ObjectCodes),
    append_lists(["<div>", NameCodes, " likes ", ObjectCodes, "</div>", Tail], Text),
    combine_answers(T, Tail).

change_name :-
    _ >> [id -:> select, value +:> Name],
    (Name = everyone
      -> TextCodes = "See all likes",
        Disabled = false
    ;
    likes(Name, _)
      -> atom_codes(Name, NameCodes),
         append_lists(["What does ", NameCodes, " like?"], TextCodes),
         Disabled = false
    ;
    atom_codes(Name, NameCodes),
    append("Invalid name: ", NameCodes, TextCodes),
    Disabled = true
    ),
    atom_codes(Text, TextCodes),
    _ >> [id -:> button, value <:+ Text, disabled <:+ Disabled].

