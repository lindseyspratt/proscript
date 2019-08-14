generate_type_references(OutputFile) :-
    open(OutputFile, write, Stream),
    generate_type_references_stream(Stream),
    close(Stream).

generate_type_references :-
    current_output(Stream),
    generate_type_references_stream(Stream).

generate_type_references_stream(Stream) :-
    setof(x(Type, Name, Standard, MDN),
          dom_type_reference(Type, Name, Standard, MDN), Refs),
    generate_type_references1(Refs, Stream).

generate_type_references1([],_).
generate_type_references1([x(Type, Name, Standard, MDN)|T], Stream) :-
    write(Stream, '<li class="indexed" id="api_'),
    write(Stream, Type),
    write(Stream, '">'),
    generate_type_reference(Stream, Type, Name, Standard, MDN),
    generate_type_parents(Stream, Type),
    generate_type_children(Stream, Type),
    generate_type_properties(Stream, Type),
    generate_type_methods(Stream, Type),
    write(Stream, '</li>\n'),
    generate_type_references1(T, Stream).

generate_type_reference(Stream, Type, Name, Standard, MDN) :-
    (MDN = none
      -> MDNPart = ''
     ;
     atom_codes(MDN, MDNCodes),
     append(", <a href='", MDNCodes, A),
     append(A, "'>MDN</a>", MDNPartCodes),
     atom_codes(MDNPart, MDNPartCodes)
    ),
    write_list([
        '<h5>',
        Name,
        ' (<a href="',
        Standard,
        '">Standard</a>',
        MDNPart,
        '): ',
        Type,
        '</h5>',
        '\n'
    ],
    '',
    Stream
    ).

generate_type_parents(Stream, Type) :-
    setof(ParentLink, Parent^(dom_type_parent(Type, Parent), api_type_link(Parent, ParentLink)), Parents),
    !,
    list_to_html_unordered_list(Parents, ParentsHTMLList),
    write(Stream, '<h6>Parents</h6>'),
    write(Stream, ParentsHTMLList),
    write(Stream, '\n').
generate_type_parents(_Stream, _Type).

% [X1,X2...] -> <ul> <li>X1</li><li>X2</li>...<ul>
list_to_html_unordered_list(List, UL) :-
    append("<ul>", ContentCodes, ULCodes),
    list_to_html_unordered_list1(List, ContentCodes, "</ul>"),
    atom_codes(UL, ULCodes).

list_to_html_unordered_list1([], Content, Content).
list_to_html_unordered_list1([H|T], ContentCodes, ContentTail) :-
    atom_to_list_item_codes(H, LICodes),
    append(LICodes, ContentNext, ContentCodes),
    list_to_html_unordered_list1(T, ContentNext, ContentTail).

atom_to_list_item_codes(Atom, LICodes) :-
    atom_codes(Atom, AtomCodes),
    append("<li>", AtomCodes, Prefix),
    append(Prefix, "</li>", LICodes).

% <a href='#api_Parent'>Parent</a>

api_type_link(Type, TypeLink) :-
    atom_codes(Type, TypeCodes),
    append("<a href='#api_", TypeCodes, Prefix),
    append(Prefix, "'>", Prefix2),
    append(Prefix2, TypeCodes, Prefix3),
    append(Prefix3, "</a>", LinkCodes),
    atom_codes(TypeLink, LinkCodes).


generate_type_children(Stream, Type) :-
    setof(ChildLink, Child^(dom_type_parent(Child, Type), api_type_link(Child, ChildLink)), Children),
    !,
    list_to_html_unordered_list(Children, ChildrenHTMLList),
    write(Stream, '<h6>Children</h6>'),
    write(Stream, ChildrenHTMLList),
    write(Stream, '\n').
generate_type_children(_Stream, _Type).

generate_type_properties(Stream, Type) :-
    setof(p(Property, ImplementationName, DataType), dom_type_property(Type, Property, ImplementationName, DataType), Ps),
    !,
    write(Stream, '<h6>Properties</h6>\n'),
    write(Stream, '\t<ul>\n'),
    generate_type_properties1(Ps, Stream),
    write(Stream, '\t</ul>\n').
generate_type_properties(Stream, _Type) :-
    write(Stream, '\t(no properties)\n').

generate_type_properties1([], _Stream).
generate_type_properties1([H|T], Stream) :-
    generate_type_property(Stream, H),
    generate_type_properties1(T, Stream).

generate_type_property(Stream, p(Property, ImplementationName, DataType)) :-
    implementation_name_sublist(Property, ImplementationName, Sublist),
    append(['\t\t<li><b>', Property], Sublist, Prefix),
    append(Prefix, ['</b>: <i>', DataType, '</i></li>\n'], Full),
    write_list(Full, '', Stream ).

implementation_name_sublist(Property, Property, Sublist) :- !.
implementation_name_sublist(Property, ImplementationName, [' (',  ImplementationName, ')']).


generate_type_methods(Stream, Type) :-
    setof(p(Method, ImplementationName, Args, Result), dom_type_method(Type, Method, ImplementationName, Args, Result), Ps),
    !,
    write(Stream, '<h6>Methods</h6>\n'),
    write(Stream, '\t<ul>\n'),
    generate_type_methods1(Ps, Stream),
    write(Stream, '\t</ul>\n').
generate_type_methods(Stream, _Type) :-
    write(Stream, '\t(no methods)\n').

generate_type_methods1([], _Stream).
generate_type_methods1([H|T], Stream) :-
    generate_type_method(Stream, H),
    generate_type_methods1(T, Stream).

generate_type_method(Stream, p(Method, ImplementationName, Args, Result)) :-
    implementation_name_sublist(Method, ImplementationName, Sublist),
    (Method = ImplementationName
      -> ImplementationNote = []
     ;
     ImplementationNote = [' Implemented by ', ImplementationName, '.']
    ),
    (Result = void
      -> FinalArgs = Args,
         IsResult = ''
     ;
     append(Args, [Result], FinalArgs), % last type in FinalArgs (Result) is the value returned by ImplementationName
     IsResult = ' [last arg is result]'
    ),
    method_goal_list(Method, FinalArgs, GoalList),
    append(['\t\t<li>'|GoalList], [IsResult], Prefix),
    append(Prefix, ImplementationNote, Middle),
    append(Middle, ['</li>\n'], Full),
    write_list(Full, '', Stream ).

% construct '<b>Method</b>(<i>arg1</i>, ...)' for 1 or more args
% or '<b>Method</b>' for no args.

method_goal_list(Method, Args, GoalList) :-
    (Args = [_|_]
      -> decorate_args(Args, DecoratedArgs),
         append(['('|DecoratedArgs], [')'], FullArgs)
     ;
     FullArgs = []
    ),
    append(['<b>', Method, '</b>'], FullArgs, GoalList).

decorate_args([], []).
decorate_args([Arg|OtherArgs], DecoratedArgs) :-
    decorate_arg(Arg, DecoratedArgs, DecoratedNext),
    decorate_args1(OtherArgs, DecoratedNext).

decorate_args1([], []).
decorate_args1([Arg|OtherArgs], [', '|DecoratedArgs]) :-
    decorate_arg(Arg, DecoratedArgs, DecoratedNext),
    decorate_args1(OtherArgs, DecoratedNext).

decorate_arg(Arg, ['<i>', Arg, '</i>'|Tail], Tail).

generate_type_sidenav(OutputFile) :-
    open(OutputFile, write, Stream),
    generate_type_sidenav_stream(Stream),
    close(Stream).

generate_type_sidenav :-
    current_output(Stream),
    generate_type_sidenav_stream(Stream).

/*
<li>{{1 @ Type & 'anchor.sidenav.template' # [Type, doc, api_blob, 'index_doc.html', 'Blob']}}</li>
*/

generate_type_sidenav_stream(Stream) :-
    setof(x(Type, Name), Standard ^ MDN ^ dom_type_reference(Type, Name, Standard, MDN), Refs),
    generate_type_sidenav1(Refs, Stream).

generate_type_sidenav1([],_).
generate_type_sidenav1([H|T], Stream) :-
    generate_type_sidenav_entry(H, Stream),
    generate_type_sidenav1(T, Stream).

generate_type_sidenav_entry(x(Type, Name), Stream) :-
    write(Stream, '<li>{{1 @ Type & \'anchor.sidenav.template\' # [Type, doc, api_'),
    write(Stream, Type),
    write(Stream, ', \'index_doc.html\', \''),
    write(Stream, Name),
    write(Stream, '\']}}</li>\n').
