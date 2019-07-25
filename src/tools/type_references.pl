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
    write(Stream, '<li>'),
    generate_type_reference(Stream, Type, Name, Standard, MDN),
    generate_type_properties(Stream, Type),
 %   generate_type_methods(Stream, Type),
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
        Name,
        ' (<a href="',
        Standard,
        '">Standard</a>',
        MDNPart,
        '): ',
        Type,
        '\n'
    ],
    '',
    Stream
    ).

generate_type_properties(Stream, Type) :-
    setof(p(Property, ImplementationName, DataType), dom_type_property(Type, Property, ImplementationName, DataType), Ps),
    !,
    write(Stream, '<h5>Properties</h5>\n'),
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
    append(['\t\t<li>', Property], Sublist, Prefix),
    append(Prefix, [': ', DataType, '</li>\n'], Full),
    write_list(Full, '', Stream ).

implementation_name_sublist(Property, Property, Sublist) :- !.
implementation_name_sublist(Property, ImplementationName, [' (',  ImplementationName, ')']).

/*
generate_type_methods(Stream, Type) :-
    setof(p(Property, ImplementationName, DataType), dom_type_method(Type, Method, Args, Result), Ps),
    !,
    write(Stream, '<h5>Methods</h5>\n'),
    write(Stream, '\t<ul>\n'),
    generate_type_methods1(Ps, Stream),
    write(Stream, '\t</ul>\n').
generate_type_methods(Stream, _Type) :-
    write(Stream, '\t(no properties)\n').

generate_type_methods1([], _Stream).
generate_type_methods1([H|T], Stream) :-
    generate_type_method(Stream, H),
    generate_type_methods1(T, Stream).

generate_type_method(Stream, p(Property, ImplementationName, DataType)) :-
    implementation_name_sublist(Property, ImplementationName, Sublist),
    append(['\t\t<li>', Property], Sublist, Prefix),
    append(Prefix, [': ', DataType, '</li>\n'], Full),
    write_list(Full, '', Stream ).
*/