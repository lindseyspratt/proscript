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
    generate_type_reference(Stream, Type, Name, Standard, MDN),
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
        '<li>',
        Name,
        ' (<a href="',
        Standard,
        '">Standard</a>',
        MDNPart,
        '): ',
        Type,
        '</li>\n'
    ],
    '',
    Stream
    ).
