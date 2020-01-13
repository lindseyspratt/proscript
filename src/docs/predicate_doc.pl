% doc(Functor/Arity, Signatures, Discussion, Tags).
% Signatures = [S1, ...] or just S1
% Sk = (Functor(P1, ...) is Determinisim))
% Determinism = det/semidet/failure/nondet/multi
% Pj = ? < stuff < int = <(?, <(stuff, int))
%   = Instantiation < ArgName < Type, or Instantiation < ArgName, or ArgName < Type, or ArgName.
% Instantiation = '++' | '+' | '-' | '--' | '?' | ':' | '@' | '!'
% Discussion = "Description of the predicate semantics and common uses."
% Tags = [Tag(X1, ...), ...]
% Tag:
% arg(Name, Description)
%   Defines the predicate arguments. Each argument has its own arg tag. Argument declarations normally appear in order used by the predicate.
% throws(Term, Description)
%   Error condition.
% error(Error, Description)
%   As throws(Term, Description), but the exception is embedded in error(Error, Context).
% author(Name)
%   Author of the module or predicate. Multiple entries are used if there are multiple authors.
% version(Version)
%   Version of the module. There is no formal versioning system.
% see(Text)
%   Point to related material. Often contains links to predicates or files.
% deprecated(Alternative)
%   The predicate or module is deprecated. The description specifies what to use in new code.
% compat(Standards_and_systems)
%   When implementing libraries or externally defined interfaces this tag describes to which standard the interface is compatible.
% copyright(Copyright_holder)
%   Copyright notice.
% license(License_conditions)
%   License conditions that apply to the source.
% bug(Bug_description)
%   Known problems with the interface or implementation.
% tbd(Work_to_be_done)
%   Not yet realised behaviour that is anticipated in future versions.
% cat(PredicateCategory)

:- module(predicate_doc, [doc/4]).

% Terms
doc(acyclic_term/1,
    acyclic_term(? < term * term) is det,
    "Term is a acyclic.",
    [arg(term, "any Prolog term"), compat(iso), cat(type_testing)] ).
doc(subsumes_term/2, subsumes_term(? < term1 * term, ? < term2 * term) is det, "Term1 subsumes Term2.",
    [arg(term1, "any Prolog term"), arg(term2, "any Prolog term"), compat(iso), cat(term_comparison)]).
doc(var/1, var(? < term * term) is det, "Term is a variable.", [arg(term, "any Prolog term"), compat(iso), cat(type_testing)]).
doc(atom/1, atom(? < term * term) is det, "Term is an atom.", [arg(term, "any Prolog term"), compat(iso), cat(type_testing)]).
doc(integer/1, integer(? < term * term) is det, "Term is an integer.", [arg(term, "any Prolog term"), compat(iso), cat(type_testing)]).
doc(float/1, float(? < term * term) is det, "Term is a float (i.e. a number represented with a decimal point).", [arg(term, "any Prolog term"), compat(iso), cat(type_testing)]).
doc(compound/1, compound(? < term * term) is det, "Term is compound - either a structure or a list.", [arg(term, "any Prolog term"), compat(iso), cat(type_testing)]).
doc(ground/1, ground(? < term * term) is det, "Term is ground - is not a variable nor is it a compound containing a variable.", [arg(term, "any Prolog term"), compat(iso), cat(type_testing)]).
doc(= /2, =(? < term1 * term, ? < term2 * term) is det, "Term1 unifies with Term2.",
    [arg(term1, "any Prolog term"), arg(term2, "any Prolog term"), compat(iso), cat(term_unification)]).
doc(\= /2, \=(? < term1 * term, ? < term2 * term) is det, "Term1 does not unify with Term2.",
    [arg(term1, "any Prolog term"), arg(term2, "any Prolog term"), compat(iso), cat(term_unification)]).
doc(== /2, ==(? < term1 * term, ? < term2 * term) is det, "Term1 matches Term2. Two terms match if they are equal without binding any variables.",
    [arg(term1, "any Prolog term"), arg(term2, "any Prolog term"), compat(iso), cat(term_comparison)]).
doc(functor/3,
    [functor(+ < term * integer, ? < name * term, ? < arity * term) is det,
     functor(- < term * integer, + < name * term, + < arity * term) is det
    ],
    "Term has functor Name and Arity arguments. ",
    [arg(term, "any Prolog term"), arg(name, "an atom"), arg(arity, "an integer"), compat(iso), cat(term_creation_and_decomposition)]).
doc(arg/3, arg(+ < position * integer, + < term * term, ? < arg * term) is det,
    "Argument at Position of Term is Arg. ",
    [arg(position, "an integer"), arg(term, "a term"), arg(arg, "a term"), compat(iso), cat(term_creation_and_decomposition)]).
doc(=.. /2, =..(? < term * term, ? < list * list) is det,
    "Term univ List. List is a list [Functor|Args] where functor(Term, Functor, Arity)
     and Args are the arguments of Term (if any):
     forall(between(1, Arity, N), (arg(N, Term, Arg), nth1(N, Args, Arg))).
     If Args is null then Term is atomic (either an atom or a number) and is equal to Functor.",
    [arg(term, "any Prolog term"), arg(list, "a list"), compat(iso), cat(term_creation_and_decomposition)]).
doc(copy_term/2, copy_term(? < term1 * term, ? < term2 * term) is det,
    "Term2 is the same as Term1 but with distinct variables such that Term1 and Term2 subsume each other and the
    variables of Term2 have not appeared in the proof tree before evaluating this predicate.",
    [arg(term1, "a Prolog term"), arg(term2, "a Prolog term"), compat(iso), cat(term_creation_and_decomposition)]).
doc(term_variables/2, term_variables(? < term * term, ? < variables * list) is det,
    "Variables are the variables in Term.",
    [arg(term, "a term"), arg(variables, "a list of one term (variable) for each variable in Term"), compat(proscriptls), cat(term_creation_and_decomposition)]).
doc(atom_to_term/3, atom_to_term(+ < atom * atom, ? < term * term, ? < bindings * list) is det,
    "Term is the result of reading the name of Atom with the ProscriptLS parser. Bindings is a list of terms of the form 'name = variable'
    where 'name' is the name of each variable found in Atom and 'variable' is the ProscriptLS variable to which that named variable
    is converted in Term.",
    [arg(atom, "an atom"), arg(term, "a term"), arg(bindings, "a list of (Name = Variable) terms"), compat(proscriptls), cat(term_creation_and_decomposition)]).
doc(compare/3, compare(? < comparison * atom, ? < term1 * term, ? < term2 * term) is det,
    "Comparison is the term-ordering between Term1 and Term2: < if Term1 @< Term2, = if Term1 == Term2, and > if
    Term1 @> Term2.",
    [arg(comparison, "<, =, or >"), arg(term1, "a Prolog term"), arg(term2, "a Prolog term"), compat(swi), cat(term_comparison)]).
doc(@< /2, '@<'(? < term1 * term, ? < term2 * term) is det,
    "Term1 is less than Term2. This predicate can be used as an infix operator.",
    [arg(term1, "a Prolog term"), arg(term2, "a Prolog term"), compat(iso), cat(term_comparison)]).
doc(@=< /2, '@=<'(? < term1 * term, ? < term2 * term) is det,
    "Term1 is less than or equal to Term2. This predicate can be used as an infix operator.",
    [arg(term1, "a Prolog term"), arg(term2, "a Prolog term"), compat(iso), cat(term_comparison)]).
doc(@>= /2, @>=(? < term1 * term, ? < term2 * term) is det,
    "Term1 is greater than or equal to Term2. This predicate can be used as an infix operator.",
    [arg(term1, "a Prolog term"), arg(term2, "a Prolog term"), compat(iso), cat(term_comparison)]).
doc(@> /2, @>(? < term1 * term, ? < term2 * term) is det,
    "Term1 is greater than Term2. This predicate can be used as an infix operator.",
    [arg(term1, "a Prolog term"), arg(term2, "a Prolog term"), compat(iso), cat(term_comparison)]).
doc(atom_length/2, atom_length(+ < atom * atom, ? < length * integer) is det,
    "Length is the number of characters in the name of Atom.",
    [arg(atom, "an atom"), arg(length, "an integer"), compat(iso), cat(atomic_term_processing)]).
doc(atom_concat/3, atom_concat(+ < atom1 * atom, + < atom2 * atom, ? < concatenation * atom) is det,
    "The names of Atom1 and Atom2 are concatenated to create Concatenation.",
    [arg(atom1, "an atom"), arg(atom2, "an atom"), arg(concatenation, "an atom"), compat(iso), cat(atomic_term_processing)]).
doc(sub_atom/5,
    [sub_atom(+ < source * atom, + < start * integer, - < length * integer, + < remaining * integer, ? < subatom * atom) is det,
     sub_atom(+ < source * atom, - < start * integer, + < length * integer, + < remaining * integer, ? < subatom * atom) is det,
     sub_atom(+ < source * atom, + < start * integer, + < length * integer, ? < remaining * integer, ? < subatom * atom) is det,
     sub_atom(+ < source * atom, + < start * integer, - < length * integer, - < remaining * integer, - < subatom * atom) is nondet,
     sub_atom(+ < source * atom, - < start * integer, + < length * integer, - < remaining * integer, ? < subatom * atom) is nondet,
     sub_atom(+ < source * atom, - < start * integer, - < length * integer, + < remaining * integer, ? < subatom * atom) is nondet
    ],
    "The name of Subatom is a part of the name of Source that begins at position Start and has Length characters and there are Remaining
    characters in Source after Subatom.",
    [arg(source, "an atom"), arg(start, "an integer"), arg(length, "an integer"), arg(remaining, "an integer"), arg(subatom, "an atom"),
    compat(proscriptls), cat(atomic_term_processing)]).
doc(char_code/2, char_code(? < atom * atom, ? < code * integer) is det,
    "The name of Atom is a single character with integer character Code.",
    [arg(atom, "an atom"), arg(code, "an integer"), compat(iso), cat(atomic_term_processing)]).
doc(atom_chars/2, atom_chars(? < atom * atom, ? < chars * list(atom)) is det,
    "The name of Atom is the list of character atoms in Chars.",
    [arg(atom, "an atom"), arg(chars, "a list of atoms"), compat(iso), cat(atomic_term_processing)]).
doc(atom_codes/2, atom_codes(? < atom * atom, ? < codes * list(integer)) is det,
    "The name of Atom is the list of character integers in Codes.",
    [arg(atom, "an atom"), arg(codes, "a list of integers"), compat(iso), cat(atomic_term_processing)]).
doc(number_chars/2, number_chars(? < number * number, ? < chars * list(atom)) is det,
    "The name of Number is the list of character atoms in Chars.",
    [arg(number, "a number"), arg(chars, "a list of atoms"), compat(iso), cat(atomic_term_processing)]).
doc(number_codes/2, number_codes(? < number * number, ? < codes * list(integer)) is det,
    "The name of Number is the list of character integers in Codes.",
    [arg(number, "a number"), arg(codes, "a list of integers"), compat(iso), cat(atomic_term_processing)]).
doc(gensym/2, gensym(+ < root * atom, ? < newAtom * atom),
    "NewAtom is the concatenation of Root and increment of per-Root counter, starting at 0. Each use of gensym/2 with the same Root increments this counter.",
    [arg(root, "an atom"), arg(newAtom, "an atom"), compat(proscriptls), cat(atomic_term_processing)]).
doc(char_conversion/2, char_conversion(+ < inChar * atom, + < outChar *atom) is det,
    "Add conversion of InChar to OutChar to the character conversion table. (This should be
    a directive.)",
    [arg(inChar, "a single-character atom"), arg(outChar, "a single-character atom"), compat(iso), cat(term_io)]).
doc(current_char_conversion/2, 
    [current_char_conversion(- < inChar * atom, - < outChar *atom) is nondet,
     current_char_conversion(+ < inChar * atom, - < outChar *atom) is det,
     current_char_conversion(- < inChar * atom, + < outChar *atom) is det,
     current_char_conversion(+ < inChar * atom, + < outChar *atom) is det
    ],
    "Character conversion table specifies that InChar is converted to OutChar when reading streams.",
    [arg(inChar, "a single-character atom"), arg(outChar, "a single-character atom"), compat(iso), cat(term_io)]).
doc(write_term/3, write_term(+ < stream * term, + < term * term, + < options * list(write_option_term)),
    "Write Term to Stream in a form determined by Options.",
    [arg(stream, "a stream term"), arg(term, "a term"), arg(options, "a list of write_term option terms.")]).
doc(current_op/3, current_op(? < priority * integer, ? < specifier * atom, ? < operator * atom),
    "Operator is currently defined with Priority and Specifier for reading and writing terms.",
    [arg(priority, "an integer"), arg(specifier, "an atom: fx, fy, xfx, xfy, yfx, yf, xf"), arg(operator, "an atom"), compat(iso), cat(term_io)]).
doc(op/3, op(? < priority * integer, ? < specifier * atom, ? < operator * atom),
    "Update the operator table with Operator as defined with Priority and Specifier for reading and writing terms.",
    [arg(priority, "an integer"), arg(specifier, "an atom: fx, fy, xfx, xfy, yfx, yf, xf"), arg(operator, "an atom"), compat(iso), cat(term_io)]).
doc(writeln/1, writeln(+ < term * term),
    "Write Term followed by a newline to the current output stream.",
    [arg(term, "a term"), compat(proscriptls), cat(term_io)]).
doc(read_term/2, read_term(? < term * term, + < options * list),
    "Reads from the current input stream a single Term and instantiates
    accordingly the read Options list. (as described by ISO standard)",
    [arg(term, "a term"), arg(options, "a read options list"), compat(iso), cat(term_io)]).
doc(read_term/3, read_term(+ < stream * stream_term, ? < term * term, + < options * list),
    "Reads from Stream a single Term and instantiates
    accordingly the read Options list.",
    [arg(stream, "a stream term"), arg(term, "a term"), arg(options, "a read options list"), compat(iso), cat(term_io)]).

% Arithmetic
doc('=:='/2, =:=( + < left * term, + < right * term) is det,
    "The value of the arithmetic expression Left is equal  to the value of the arithmetic expression Right.",
    [arg(left, "an arithmetic expression term"), arg(right, "an arithmetic expression term"), compat(iso), cat(arithmetic)]).
doc('=\\=' /2, =\=( + < left * term, + < right * term) is det,
    "The value of the arithmetic expression Left is not equal  to the value of the arithmetic expression Right.",
    [arg(left, "an arithmetic expression term"), arg(right, "an arithmetic expression term"), compat(iso), cat(arithmetic)]).
doc('>'/2, >( + < left * term, + < right * term) is det,
    "The value of the arithmetic expression Left is greater than the value of the arithmetic expression Right.",
    [arg(left, "an arithmetic expression term"), arg(right, "an arithmetic expression term"), compat(iso), cat(arithmetic)]).
doc('>='/2, >=( + < left * term, + < right * term) is det,
    "The value of the arithmetic expression Left is greater than or equal to the value of the arithmetic expression Right.",
    [arg(left, "an arithmetic expression term"), arg(right, "an arithmetic expression term"), compat(iso), cat(arithmetic)]).
doc('<'/2, <( + < left * term, + < right * term) is det,
    "The value of the arithmetic expression Left is less than the value of the arithmetic expression Right.",
    [arg(left, "an arithmetic expression term"), arg(right, "an arithmetic expression term"), compat(iso), cat(arithmetic)]).
doc('=<'/2, =<( + < left * term, + < right * term) is det,
    "The value of the arithmetic expression Left is equal to or less than the value of the arithmetic expression Right.",
    [arg(left, "an arithmetic expression term"), arg(right, "an arithmetic expression term"), compat(iso), cat(arithmetic)]).
doc(is/2, is( ? < result * number, + < expression * term) is det,
    "Result is the value of evaluating the arithmetic Expression.",
    [arg(result, "a number"), arg(expression, "an arithmetic expression term"), compat(iso), cat(arithmetic)]).

% Runtime, Logic and Control
doc(halt/0, halt is det, "Halt the WAM engine. The engine may be restarted - typically after evaluating the backtrack Javascript function.",
    [compat(iso), cat(logic_and_control)]).
doc(yield/0, yield is det,
    "Yield (suspend) the WAM engine to allow any waiting HTML events to be processed.
     The yield/0 predicate completes after these waiting events have been handled.
     This can be used to make changes to the current web page and have them rendered
     immediately instead of when the current top level query completes.",
    [compat(proscriptls), cat(logic_and_control)]).
doc(fail/0, fail is det, "This goal always fails.",
    [compat(iso), cat(logic_and_control)]).
doc(true/0, true is det, "This goal always succeeds.",
    [compat(iso), cat(logic_and_control)]).
doc(current_prolog_flag/2,
    [current_prolog_flag(+ < flag * term, ? < value * term) is det,
     current_prolog_flag(- < flag * term, ? < value * term) is nondet],
    "Prolog Flag has Value. The defined flags are: bounded,
    max_integer, min_integer, integer_rounding_function,
    char_conversion, debug, max_arity, unknown, double_quotes, and dialect.",
    [arg(flag, "a defined flag atom"), arg(value, "a Prolog term"), compat(iso), cat(flag_updates)]).
doc(set_prolog_flag/2, set_prolog_flag(+ < flag * term, ? < value * term) is det,
    "Set Prolog Flag to Value. Only certain flags are settable: char_conversion, debug, unknown, double_quotes, wam_log, and wam_log_size.",
    [arg(flag, "a defined flag atom"), arg(value, "a Prolog term"), compat(iso), cat(flag_updates)]).
doc(repeat/0, repeat is nondet, "Always succeeds on initial call and on all redo calls (it backtracks without bound).",
    [compat(iso), cat(logic_and_control)]).
doc(statistics/0, statistics is det,
    "Display runtime statistics. These include WAM duration, current heap size in words,
    maximum heap size encountered, and maximum stack size encountered.",
    [compat(iso), cat(runtime)]).
doc(wam_duration/1, wam_duration is det,
    "Milliseconds spent in the WAM since start of the WAM",
    [arg(value, "an integer"), compat(proscriptls), cat(runtime)]).
doc(statistics_max_heap/1, statistics_max_heap(? < value * integer) is det,
    "Size in words of the maximum storage allocated in the WAM heap since the WAM was initialized",
    [arg(value, "an integer"), compat(proscriptls), cat(runtime)]).
doc(statistics_max_stack/1, statistics_max_stack(? < value * integer) is det,
    "Size in words of the maximum storage allocated in the WAM stack since the WAM was initialized",
    [arg(value, "an integer"), compat(proscriptls), cat(runtime)]).

% Stream selection and control
doc(set_input/1, set_input(+ < stream * term),
    "Sets current input stream to Stream.",
    [arg(stream, "a stream term"), compat(iso), cat(stream_selection_and_control)]).
doc(set_output/1, set_output(+ < stream * term),
    "Sets current output stream to Stream.",
    [arg(stream, "a stream term"), compat(iso), cat(stream_selection_and_control)]).
doc(current_input/1, set_input(+ < stream * term),
    "Current input stream is Stream.",
    [arg(stream, "a stream term"), compat(iso), cat(stream_selection_and_control)]).
doc(current_output/1, set_output(+ < stream * term),
    "Current output stream is Stream.",
    [arg(stream, "a stream term"), compat(iso), cat(stream_selection_and_control)]).
doc(flush_output/1, flush_output(+ < stream * term),
    "Flush output Stream.",
    [arg(stream, "a stream term"), compat(iso), cat(stream_selection_and_control)]).
doc(at_end_of_stream/1, at_end_of_stream(+ < stream * term),
    "Stream position is at its end.",
    [arg(stream, "a stream term"), compat(iso), cat(stream_selection_and_control)]).
doc(set_stream_position/2, set_stream_position(+ < stream * term, + < position * integer),
    "Set Stream position to Position.",
    [arg(stream, "a stream term"), arg(position, "an integer"), compat(iso), cat(stream_selection_and_control)]).
doc(stream_property/2, stream_property(? < stream * term, ? < property * term),
    "Enumerates all the pairs of open streams together with their properties.",
    [arg(stream, "a stream term"), arg(property, "a property term"), compat(iso), cat(stream_selection_and_control)]).
doc(open/4, open(+ < sourceSink * term, + < mode * atom, - < stream * term, + < options * list) is det,
    "Open a source or sink, creating a Stream term by which to access that source or sink.",
    [arg(sourceSink, "a term"), arg(mode, "a stream mode atom: 'read', 'write', 'append'"), arg(options, "a list of stream options"),
     compat(iso), cat(stream_selection_and_control)]).
doc(open/3, open(+ < sourceSink * term, + < mode * atom, - < stream * term) is det,
     "Open a text file source or sink with default options, creating a Stream term by which to access that source or sink.",
     [arg(sourceSink, "a term"), arg(mode, "a stream mode atom: 'read', 'write', 'append'"),
      compat(iso), cat(stream_selection_and_control)]).
doc(close/1, close(+ < stream * term),
    "Close Stream.",
    [arg(stream, "a stream term"), compat(iso), cat(stream_selection_and_control)]).

% Character IO
doc(get_char/1, get_char(? < char * atom),
    "Get next character from current input stream as Char.",
    [arg(char, "an atom"), compat(iso), cat(character_io)]).
doc(get_char/2, get_char(+ < stream * term, ? < char * atom),
    "Get next character from Stream as Char.",
    [arg(stream, "a stream term"), arg(char, "an atom"), compat(iso), cat(character_io)]).
doc(get_code/1, get_code(? < code * integer),
    "Get next character code from current input stream as Code.",
    [arg(code, "an integer"), compat(iso), cat(character_io)]).
doc(get_code/2, get_code(+ < stream * term, ? < code * integer),
    "Get next character code from Stream as Code.",
    [arg(stream, "a stream term"), arg(code, "an integer"), compat(iso), cat(character_io)]).
doc(peek_char/1, peek_char(? < char * atom),
    "Peek at next character in current inpu stream as Char. Do not advance stream position.",
    [arg(char, "an atom"), compat(iso), cat(character_io)]).
doc(peek_char/2, peek_char(+ < stream * term, ? < char * atom),
    "Peek at next character in Stream as Char. Do not advance stream position.",
    [arg(stream, "a stream term"), arg(char, "an atom"), compat(iso), cat(character_io)]).
doc(peek_code/1, peek_code(? < code * atom),
    "Peek at next character code in current input stream as Code. Do not advance stream position.",
    [arg(char, "an integer"), compat(iso), cat(character_io)]).
doc(peek_code/2, peek_code(+ < stream * term, ? < code * atom),
    "Peek at next character code in Stream as Code. Do not advance stream position.",
    [arg(stream, "a stream term"), arg(char, "an integer"), compat(iso), cat(character_io)]).
doc(put_char/1, put_char(? < char * atom),
    "Put Char in current output stream.",
    [arg(char, "an atom"), compat(iso), cat(character_io)]).
doc(put_char/2, put_char(+ < stream * term, ? < char * atom),
    "Put Char in Stream.",
    [arg(stream, "a stream term"), arg(char, "an atom"), compat(iso), cat(character_io)]).
doc(put_code/1, put_code(? < code * integer),
    "Put character Code in current output stream.",
    [arg(code, "an integer"), compat(iso), cat(character_io)]).
doc(put_code/2, put_code(+ < stream * term, ? < code * integer),
    "Put character Code in Stream.",
    [arg(stream, "a stream term"), arg(code, "an integer"), compat(iso), cat(character_io)]).

% Byte IO
doc(get_byte/2, get_byte(+ < stream * term, ? < byte * integer),
    "Get next byte from Stream as Byte.",
    [arg(stream, "a stream term"), arg(byte, "an integer"), compat(iso), cat(byte_io)]).
doc(peek_byte/2, peek_byte(+ < stream * term, ? < byte * atom),
    "Peek at next byte in Stream as Byte. Do not advance stream position.",
    [arg(stream, "a stream term"), arg(byte, "an integer"), compat(iso), cat(byte_io)]).
doc(put_byte/2, put_byte(+ < stream * term, ? < byte * integer),
    "Put Byte in Stream.",
    [arg(stream, "a stream term"), arg(byte, "an integer"), compat(iso), cat(byte_io)]).

% Clause retrieval and information
doc(clause/2, clause(+ < head * term, ? < body * term),
    "A public user-defined procedure in the database has Head and Body.",
    [arg(head, "a callable term"), arg(body, "a callable term"), compat(iso), cat(clause_retrieval_and_information)]).

% Clause creation and destruction
doc(abolish/1, abolish(+ < indicator * term) is det,
    "Removes the (dynamic) user defined procedure identified by its predicate indicator,
    leaving the database in the same state as if this procedure had never existed.
    (as defined in the ISO standard)",
    [arg(indicator, "predicate indicator term"), compat(iso), cat(clause_creation_and_destruction)]).
doc(retract/1, retract(+ < clause * term) is nondet,
    "Retract from the database the clauses which are unifiable with Clause.",
    [arg(clause, "a clause term"), compat(iso), cat(clause_creation_and_destruction)]).
doc(retract_clause/2, retract(+ < head * term, ? < body * term) is nondet,
    "Retract from the database the clauses which are unifiable with Head and Body.",
    [arg(head, "a callable term"), arg(body, "a term"), compat(iso), cat(clause_creation_and_destruction)]).

% Memory file
doc(new_memory_file/1, new_memory_file(- < memoryFile * memory_file_specifier) is det,
    "Create a memory file. This allocates internal storage that should be freed using free_memory_file/1.",
    [compat(proscriptls), cat(memory_file)]).
doc(open_memory_file/3,
    open_memory_file(+ < memoryFile * memory_file_specifier, + < mode * stream_mode_atom, - < stream * stream_specifier) is det,
    "Open a stream to read from , write into, or append to the given memory file.",
    [compat(proscriptls), cat(memory_file)]).
doc(free_memory_file/1, free_memory_file(+ < memoryFile * memory_file_specifier) is det,
    "Free the internal storage for the memory file.",
    [compat(proscriptls), cat(memory_file)]).
doc(copy_memory_file_to_local_storage/2,
    copy_memory_file_to_local_storage(+ < memoryFile * memory_file_specifier, + < key * atom) is det,
    "Copy from a memory file to Window.localStorage using the specified key.",
    [compat(proscriptls), cat(memory_file)]).
doc(copy_local_storage_to_memory_file/2,
    copy_local_storage_to_memory_file(+ < key * atom, + < memoryFile * memory_file_specifier) is det,
    "Copy from Window.localStorage at the specified key to the memory_file.",
    [compat(proscriptls), cat(memory_file)]).

% DOM
doc(remove_dom_element_class/2,
    remove_dom_element_class(element, class),
    "Remove Class from classes defined for Element.",
    [cat(dom)]).
doc(replace_dom_element_class/2,
    replace_dom_element_class(element, class),
    "Replace Class in classes defined for Element.",
    [cat(dom)]).
doc(toggle_dom_element_class/3,
    toggle_dom_element_class(element, class, mode),
    "Toggle Class in classes defined for Element with Mode of either 'add' or 'remove'.",
    [cat(dom)]).
doc(set_dom_element_attribute_value/3, set_dom_element_attribute_value(+ <element*object_id, + <attribute*atom, + <value*atom),
    "Set DOM element attribute to value.",
    [arg(element, " DOM Element object identifier"),
     arg(attribute, "an atom naming an attribute"),
     arg(value, "an atom specifying the value to be assigned to the attribute"),
     cat(dom)]).
doc(dom_element_attribute_value/3,
    [dom_element_attribute_value(+ <element*object_id, + <attribute*atom, + <value*atom) is det,
     dom_element_attribute_value(+ <element*object_id, + <attribute*atom, - <value*atom) is nondet,
     dom_element_attribute_value(+ <element*object_id, - <attribute*atom, + <value*atom) is nondet,
     dom_element_attribute_value(- <element*object_id, + <attribute*atom, + <value*atom) is nondet,
     dom_element_attribute_value(+ <element*object_id, - <attribute*atom, - <value*atom) is nondet,
     dom_element_attribute_value(- <element*object_id, + <attribute*atom, - <value*atom) is nondet,
     dom_element_attribute_value(- <element*object_id, - <attribute*atom, + <value*atom) is nondet,
     dom_element_attribute_value(- <element*object_id, - <attribute*atom, - <value*atom) is nondet],
    "There may be any combination of bindings of Element, Attribute, Value, including none bound.
     If Element is unbound and Attribute and Value are bound then dom_element_attribute_value/3 has two
     strategies for finding Element: if Attribute and Value are bound and Attribute is 'id', 'name', or 'class'
     then use specific Javascript document getElementById(Value), getElementsByName(Value), or getElementsByClass(Value) method
     where Attribute is 'id', 'name', or 'class', respectively; otherwise, get all
     elements using document.querySelectorAll('*') and check each one using element.getAttribute(Attribute)=Value.

     If Attribute is unbound then dom_element_attribute_value/3 checks each possible Element
     using Javascript element.getAttributeNames() to generate all values for Attribute for each Element. For each Attribute and Element value
     the value is checked/retrieved. As above, 'id', 'name',
     and 'class' attributes are handled specially; all other attributes are handled
     using element.getAttribute(Attribute)=Value.",
    [arg(element, " DOM Element object identifier"),
     arg(attribute, "an atom naming an attribute"),
     arg(value, "an atom specifying the value to be assigned to the attribute"),
     cat(dom)]).
doc(create_dom_element/2, create_dom_element(+ < tag * atom, - < element * object_id),
    "Create a DOM element object with tag Tag.",
    [arg(tag, "an HTML tag name (any case)"),
     arg(element, "an object ID structure for an HTML element object."),
     cat(dom)]).
doc(create_dom_text_node/2, create_dom_text_node(+ < text * character_code_list, - < element * object_id),
    "Create a DOM text node object.",
    [arg(text, "a string (represented as a character code list)"),
     arg(element, "an object ID structure for an HTML text node object"),
     cat(dom)]).
doc(append_dom_node_child/2, append_dom_node_child(+ < element * object_id, + < child * object_id),
    "Append a DOM node as a child node.",
    [arg(element, "an object ID structure for an HTML node object"),
     arg(child, "an object ID structure for an HTML node object"),
     cat(dom)]).
doc(insert_before_dom_node/3, insert_before_dom_node(+ < parent * object_id, + < element * object_id, + < before * object_id),
    "Insert child Element before Before element with parent Parent element.",
    [arg(parent, "an object ID structure for an HTML node object"),
     arg(element, "an object ID structure for an HTML node object"),
     arg(before, "an object ID structure for an HTML node object"),
     cat(dom)]).
doc(dom_select_element/2, dom_select_element(+ < query * character_code_list, ? < element * object_id),
    "Select an HTML Element that satisfies the specified Query.",
    [arg(query, "a string (represented as a character code list)"),
     arg(element, "an object ID structure for an HTML element object"),
     cat(dom)]).
doc(dom_select_all_elements/2, dom_select_all_elements(query, element) is nondet,
    "Select each HTML Element that satisfies the specified Query.",
    [arg(query, "a string (represented as a character code list)"),
     arg(element, "an object ID structure for an HTML element object"),
     cat(dom)]).
doc(dom_object_property/4,
    [dom_object_property(+ < type * atom, + < object * object_id, + < property * atom, - < value * term) is nondet,
     dom_object_property(+ < type * atom, - < object * object_id, + < property * atom, + < value * term) is nondet,
     dom_object_property(- < type * atom, + < object * object_id, + < property * atom, + < value * term) is det,
     dom_object_property(+ < type * atom, + < object * object_id, + < property * atom, + < value * term) is det
    ],
    "Determine each Value of Property of Object of Type.
     Property must be a ground value.
     At least two of Type, Object, and Value must be ground.",
    [arg(type, "an atom specifying the object type"),
     arg(object, "an object ID structure for a Web API (or other Javascript) object"),
     arg(property, "an atom naming a defined property for the related object"),
     arg(value, "a term specifying the value of the named property of the related object"),
     cat(dom)]).
doc(dom_object_method/3, dom_object_method(+ < object * object_id, : < methodStructure * callable_term, + < specTerm * list),
    "Evaluate a Javascript method applied to a Javascript Web API object.
     The result of the method (if any) is unified with the last argument
     of the Method structure.
     The Method argument may be qualified with a module name or
     an argument of the Method structure may be qualified. For example:
     dom_object_method(Element, add_event_listener(click, 'foo:bar'(thing)))
     or
     dom_object_method(Element, foo : add_event_listener(click, bar(thing))).
     In the second case, the 'foo' module name is used in conjunction with
     the meta-argument type definition for 'add_event_listener' second argument
     (of '0') to determine
     that 'bar(thing)' should be qualified as 'foo:bar'(thing).
     If the foo module had imported bar/1 from the quux module, then this
     qualification would be 'quux:bar'(thing).
     The SpecTerm is used to define the method call when it is not already defined.
     The SpecTerm has the form [MethodName, ArgTypes, ReturnType] or [MethodName, ArgTypes] if no return.
     ",
    [arg(object, "an object ID structure for a Web API (or other Javascript) object"),
     arg(methodStructure, "a term of the form 'methodName(arg1, arg2, ..)'"),
     arg(specTerm, "either [] or the form [MethodName, ArgTypes, ReturnType] or [MethodName, ArgTypes] if no return"),
     cat(dom)]).
doc(dom_object_method/2, dom_object_method(+ < object * object_id, : < methodStructure * callable_term),
    "This predicate is the same as dom_object_method/3 with a SpecTerm = [].", [cat(dom)]).
doc(dom_object_type/2, dom_object_type(+ < object * object_id, ? < type * atom),
    "Relate a Javascript object to its ProscriptLS type.
    A Web API object that is an instance of HTMLElement has the type 'htmlelement'.",
    [arg(object, "an object ID structure for a Web API (or other Javascript) object"),
     arg(type, "an atom naming the type of the associated object"),
     cat(dom)]).
doc(dom_create_object/3, dom_create_object(+ < type * atom, - < object * object_id, + < spec * list),
    "Create a Javascript object specified by the Type.
    The Type term is the constructor name and arguments (if any).
    The type argument may be of the form ModuleName : Type.
    An argument may be a goal_function type, in which case the ModuleName (inferred or explicit)
    is needed to determine what module holds the predicate(s) of the goal_functor.
    Argument types include: object, string, string_codes, integer, number, boolean, position, goal_function,
    event, and options. Also a type may specify a list or array of items of the same type as 'array_type(Type)', e.g.
    'array_type(integer)' for an array/list of integers.",
    [arg(type, "an atom or structure that specifies the kind of object to create"),
     arg(object, "an object ID structure for a Web API (or other Javascript) object"),
     arg(spec, "a list specifying the types of the arguments of the Type structure (if any)."),
     cat(dom)]).
doc(dom_create_object/2, dom_create_object(+ < type * atom, - < object * object_id),
    "Same as dom_create_object/3 with Spec = [].",
     [arg(type, "an atom that specifies the kind of object to create"),
      arg(object, "an object ID structure for a Web API (or other Javascript) object"),
      cat(dom)]).
doc(dom_release_object/1, dom_release_object(+ < object * object_id),
    "Release internal registration of Javascript structure for Object",
    [arg(object, "an object ID structure for a Web API (or other Javascript) object"), cat(dom)]).
doc(dom_type_reference/4, dom_type_reference(? < type * atom, ? < name * atom, ? < standard * atom, ? < mdn * atom),
    "Relates DOM Web API type and name with reference URLs in W3C standards and Mozilla MDN. This predicate is used in generating ProscriptLS documentation.",
    [arg(type, "an atom that is the ProscriptLS name (all lowercase) of the Web API interface"),
     arg(name, "an atom that is the Web API interface name (proper case)"),
     arg(standard, "an atom that is the URL to a W3C reference for the associated Web API name"),
     arg(mdn, "an atom that is the URL to a Mozilla MDN reference for the associated Web API name"),
    cat(dom)]).
doc(set_dom_object_property/3, set_dom_object_property(object, property, value),
    "Set Property of Object to Value. Property must be in a defined Web API interface for
    ProscriptLS for the interface type for Object or one of its parent types.",
    [arg(object, "object_id of a Javascript object that is an instance of a Web API interface"),
     arg(property, "an atom naming a defined property for the associated object Web API interface"),
     arg(value, "a term of a type appropriate to the associated property"),
     cat(dom)]).
doc(alert/1, alert(+ < term * term), "Display Term in a browser alert dialog.", [arg(term, "any term"), cat(dom)]).
doc(dom_window/1, dom_window(? < window * object_id) is det, "Window is the object ID for the Web API interface HTML Window object.", [arg(window, "the object ID for the HTML window object"), cat(dom)]).
doc(dom_type_property/4, dom_type_property(? < objectType * atom, ? < propertyName * atom, ? < jsName * atom, ? < valueType * atom),
    "Find the related values for a WebInterface API ObjectType, a PropertyName implemented for that type, the Javascript Web API function JsName used
     to implement that property, and the ValueType for that property.
    ",
    [arg(objectType, "an atom naming a Web API interface"),
     arg(propertyName, "an atom naming a property of a Web API interface"),
     arg(jsName, "an atom naming the Javascript function that implements getting the value of a property of a Web API interface"),
     arg(valueType, "an atom specifying the data type returned by a Javascript method"),
     cat(dom)]).
doc(dom_type_method/5, dom_type_method(? < objectType * atom, ? < methodName * atom, ? < implementationName * atom, ? < argumentTypes * list, ? < resultType * atom),
    "Find the related values for a WebInterface API object type, a method name implemented for that type, the Javascript Web API function name used
     to implement that method, the types of arguments for that method, and the result type (possibly undefined).",
    [arg(objectType, "an atom naming a Web API interface"),
     arg(methodName, "an atom naming a method of a Web API interface"),
     arg(implementationName, "an atom naming Javascript function that implements a method of a Web API interface"),
     arg(argumentTypes, "a list of data type specifications for the inputs of an API method."),
     arg(resultType, "an atom specifying the result data type (if any) of an API method"),
     cat(dom)]).
doc(dom_type_parent/2, dom_type_parent(? < objectType * atom, ? < parentType * atom),
    "Web API interface ObjectType has parent interface ParentType.",
    [arg(objectType, "an atom naming a Web API interface"),
     arg(parentType, "an atom naming a Web API interface"),
     cat(dom)]).
