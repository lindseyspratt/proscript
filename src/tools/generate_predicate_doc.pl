:- module(generate_predicate_doc,
    [generate_predicate_doc/1, generate_predicate_doc/0, generate_predicate_doc_stream/1,
     generate_pred_cat_sidenav/1, generate_pred_cat_sidenav/0, generate_pred_cat_sidenav_stream/1]).

:- use_module('../docs/predicate_doc').
:- use_module('../system/system_util').

%Pred(<inst><arg>:<type>, ...)
%    Desc
%    Tag:Value
%    Tag:Value

generate_predicate_doc(OutputFile) :-
    open(OutputFile, write, Stream),
    generate_predicate_doc_stream(Stream),
    close(Stream).

generate_predicate_doc :-
    current_output(Stream),
    generate_predicate_doc_stream(Stream).

generate_predicate_doc_stream(Stream) :-
    setof(Cat-Preds,
        setof(x(Pred, Sig, Desc, Tags),
          (doc(Pred, Sig, Desc, Tags), member(cat(Cat), Tags)), Preds),
          CatRefs),
    generate_predicate_doc(CatRefs, Stream).

generate_predicate_doc([], _Stream).
generate_predicate_doc([Cat-Preds|T], Stream) :-
    generate_predicate_doc_cat(Preds, Cat, Stream),
    generate_predicate_doc(T, Stream).

generate_predicate_doc_cat(Preds, Cat, Stream) :-
    predicate_cat_label(Cat, Label),
    write(Stream, '<div class="indexed" id="'),
    write(Stream, Label),
    writeln(Stream, '">'),
    write(Stream, '<h4>'),
    write(Stream, Cat),
    writeln(Stream, '</h4>'),
    writeln(Stream, '<ul>'),
    generate_predicate_doc_cat(Preds, Stream),
    writeln(Stream, '</ul>'),
    writeln(Stream, '</div>').

generate_predicate_doc_cat([], _Stream).
generate_predicate_doc_cat([H|T], Stream) :-
    generate_predicate_doc_cat1(Stream, H),
    generate_predicate_doc_cat(T, Stream).

% class="indexed" id="predicates"
generate_predicate_doc_cat1(Stream, x(Pred, Sig, DescCodes, Tags)) :-
    predicate_label(Pred, Label),
    format(Stream, '<li class="indexed" id="~w">~n', [Label]),
    write(Stream, '<h5>'),
    write(Stream, Pred),
    writeln(Stream, '</h5>'),
    sig_args(Sig, Stream),
    atom_codes(Desc, DescCodes),
    format(Stream, '<br><br>~w~n<br>~n', [Desc]),
    tags(Tags, Stream),
    writeln(Stream, '</li>\n').

list_to_sequence_atom(Args, ArgsCore) :-
    format(atom(ArgString), '~w', [Args]),
    atom_codes(ArgString, ArgStringCodes),
    append("[", ASCPrefix, ArgStringCodes),
    append(ASCCore, "]", ASCPrefix),
    atom_codes(ArgsCore, ASCCore).

sig_args([], _Stream) :-
    !.
sig_args([H|T], Stream) :-
    !,
    sig_args(H, Stream),
    sig_args(T, Stream).
sig_args(Sig is Determinism, Stream) :-
    !,
    sig_args(Sig, Functor, Args),
    list_to_sequence_atom(Args, ArgsCore),
    (ArgsCore = ''
     -> format(Stream, '<br><b>~w</b> is ~w~n', [Functor,Determinism])
    ;
     format(Stream, '<br><b>~w</b>(~w) is ~w~n', [Functor,ArgsCore,Determinism])
    ).
sig_args(Sig, Stream) :-
    sig_args(Sig, Functor, Args),
    list_to_sequence_atom(Args, ArgsCore),
    format(Stream, '<br><b>~w</b>(~w)~n', [Functor,ArgsCore]).

sig_args(Sig, Functor, Args) :-
    Sig =.. [Functor|Specs],
    spec_args(Specs, Args).

spec_args([], []).
spec_args([H|T], [HS|TS]) :-
    spec_arg(H, HS),
    spec_args(T, TS).

spec_arg(Inst < Name * Type, Arg) :-
    !,
    capitalize(Name, CapName),
    format(atom(Arg), '~w<i>~w</i>:~w', [Inst, CapName, Type]).
spec_arg(Inst < Name, Arg) :-
    !,
    capitalize(Name, CapName),
    format(atom(Arg), '~w<i>~w</i>', [Inst, CapName]).
spec_arg(Name * Type, Arg) :-
    !,
    capitalize(Name, CapName),
    format(atom(Arg), '<i>~w</i>:~w', [CapName, Type]).
spec_arg(Name, Arg) :-
    !,
    capitalize(Name, CapName),
    format(atom(Arg), '<i>~w</i>', [CapName]).

%tags(Tags, Stream)
tags([], _).
tags([H|T], Stream) :-
    tag(H, Stream),
    tags(T, Stream).

tag(arg(Name, DescCodes), Stream) :-
    capitalize(Name, CapName),
    atom_codes(Desc, DescCodes),
    format(Stream, '<br><b>~w</b>: <i>~w</i>. ~w.~n', [arg, CapName, Desc]).
tag(Tag, Stream) :-
    Tag =.. [Functor|Args],
    write(Stream, '<br><b>'),
    write(Stream, Functor),
    write(Stream, '</b>: '),
    write_list(Args, ', ', Stream),
    writeln(Stream, '.').

/*
<li>{{1 @ Type & 'anchor.sidenav.template' # [Type, doc, pred_cat_dom, 'index_doc.html', 'dom']}}
<ul>
<li>{{1 @ Type & 'anchor.sidenav.template' # [Type, doc, pred_foo_3, 'index_doc.html', 'foo/3']}}</li>
</ul>
</li>
*/

generate_pred_cat_sidenav(OutputFile) :-
    open(OutputFile, write, Stream),
    generate_pred_cat_sidenav_stream(Stream),
    close(Stream).

generate_pred_cat_sidenav :-
    current_output(Stream),
    generate_pred_cat_sidenav_stream(Stream).

generate_pred_cat_sidenav_stream(Stream) :-
    setof(Cat-Preds,
        setof(Pred,
          Sig^Desc^Tags^
            (doc(Pred, Sig, Desc, Tags), member(cat(Cat), Tags)), Preds),
          CatPreds),
    generate_pred_cat_sidenav(CatPreds, Stream).

generate_pred_cat_sidenav([],_).
generate_pred_cat_sidenav([Cat-H|T], Stream) :-
    generate_pred_cat_sidenav_entry(Cat, H, Stream),
    generate_pred_cat_sidenav(T, Stream).

generate_pred_cat_sidenav_entry(Cat, Preds, Stream) :-
    predicate_cat_label(Cat, Label),
    write(Stream, '<li><span class="caret"></span>{{1 @ Type & \'anchor.sidenav.template\' # [Type, doc, '),
    writeq(Stream, Label),
    write(Stream, ', \'index_doc.html\', \''),
    write(Stream, Cat),
    writeln(Stream, '\']}}'),
    writeln(Stream, '<ul class=\'nested\'>'),
    generate_pred_sidenav(Preds, Stream),
    write(Stream, '</ul></li>\n').

generate_pred_sidenav([],_).
generate_pred_sidenav([H|T], Stream) :-
    generate_pred_sidenav_entry(H, Stream),
    generate_pred_sidenav(T, Stream).

generate_pred_sidenav_entry(Pred, Stream) :-
    predicate_label(Pred, Label),
    write(Stream, '<li>{{1 @ Type & \'anchor.sidenav.template\' # [Type, doc, '),
    writeq(Stream, Label),
    write(Stream, ', \'index_doc.html\', '),
    format(atom(X), '~w', [Pred]),
    writeq(Stream, X),
    write(Stream, ']}}</li>\n').

predicate_cat_label(Cat, Label) :-
    format(atom(Label), 'pred_cat_~w', [Cat]).

predicate_label(Functor/Arity, Label) :-
    format(atom(Label), 'pred_~w_~w', [Functor,Arity]).
