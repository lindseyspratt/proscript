:- module(decode_instructions, [decode_instructions/2]).

:- use_module('../tools/wam_bootstrap_util').

decode_instructions(PredicateName, Codes) :-
    length(Codes, Length),
    decode_instructions(PredicateName, Codes, 0, Length).

decode_instructions(_PredicateName, _Codes, BeyondEnd, BeyondEnd) :- !.
decode_instructions(PredicateName, Codes, Current, BeyondEnd) :-
    Current < BeyondEnd, % sanity check
    decode_instruction_general(PredicateName, Current, Codes, Atom, Op, OpName, Size, GoalPredicate),
    writeln(inst(Atom, Op, OpName, Size, GoalPredicate)),
    Next is Current + Size,
    decode_instructions(PredicateName, Codes, Next, BeyondEnd).

op_name(0, 'zero', []).
op_name(1, 'allocate', []).
op_name(2, 'deallocate', []).
op_name(3, 'call', [pred, n]).
op_name(4, 'execute', [pred]).
op_name(5, 'proceed', []).
op_name(6, 'put_variable', [y,x]). % variable to Y and X registers.
op_name(60, 'put_variable', [y]). % Y register variable only
op_name(7, 'put_variable', [x,x]). % variable to X and X registers.
op_name(8, 'put_value', [v1,v2,x]).
op_name(9, 'put_unsafe_value', [y,x]).
op_name(10, 'put_constant', [ca,x]).
op_name(11, 'put_nil', [x]).
op_name(12, 'put_structure', [pred,x]).
op_name(13, 'put_list', [x]).
op_name(14, 'put_integer', [ci,x]).
op_name(51, 'put_float', [cf,x]).
op_name(15, 'get_variable', [v1,v2,x]).
op_name(16, 'get_value', [v1,v2,x]).
op_name(17, 'get_constant', [ca,x]).
op_name(18, 'get_nil', [x]).
op_name(19, 'get_structure', [pred,x]).
op_name(20, 'get_list', [x]).
op_name(21, 'get_integer', [ci,x]).
op_name(50, 'get_float', [cf,x]).
op_name(22, 'unify_void', [n]).
op_name(23, 'unify_variable', [v1,v2]).
op_name(24, 'unify_value', [v1,v2]).
op_name(25, 'unify_local_value', [v1,v2]).
op_name(26, 'unify_constant', [ca]).
op_name(27, 'unify_integer', [ci]).
op_name(52, 'unify_float', [cf]).
op_name(28, 'try_me_else', [n]).
op_name(29, 'retry_me_else', [n]).
op_name(30, 'trust_me', [n]). % n = 0
op_name(31, 'neck_cut', []).
op_name(32, 'cut', [y]).
op_name(33, 'get_level', [y]).
op_name(40, 'call_aux', [n,n,n]).
op_name(41, 'execute_aux', [n,n]).
op_name(42, 'retry_foreign', []).
op_name(43, 'get_choicepoint', [n,y]).
op_name(44, 'switch_on_term', [n,a,a,a,a,a]).
op_name(45, 'switch_on_constant', t(c)).
op_name(46, 'switch_on_structure', t(s)).
op_name(71, 'try', [n]).
op_name(72, 'retry', [n]).
op_name(73, 'trust', [n]).
op_name(74, 'goto_clause', [n]).
op_name(254, 'nop2', [n]).

decode_instruction_general(PredicateID, CodePosition, Code, String, Op, OpName, InstructionSize, GoalPredicate) :-
    (PredicateID = none
      -> PredicateName = 'no predicate'
    ;
     number(PredicateID)
      -> ftable(AtomID/Arity, PredicateID),
         atable(FunctorName, AtomID),
         format(atom(PredicateName), '~w/~w', [FunctorName, Arity])
    ;
     atom(PredicateID)
      -> PredicateName = PredicateID
    ;
     format(atom(PredicateName), '~w', [PredicateID])
    ),
    nth0(CodePosition, Code, Op),
    dig_op(Op, OpName, Instruction, InstructionSize, GoalPredicate, CodePosition, Code),
    format(atom(String), '~w:(~w,~w)', [PredicateName, Instruction, CodePosition]).

code(CodePosition, PositionOffset, Code, Value) :-
    Offset is CodePosition + PositionOffset,
    nth0(Offset, Code, Value).

dig_op(Op, Name, Inst, Next, GP, CP, C) :-
    op_name(Op, Name, Pattern),
    (Pattern = t(_)
      -> dig_op_table(Pattern, 1, FormatCodes, Args, GP, Next, CP, C),
         atom_codes(Format, FormatCodes),
         format(atom(Inst), Format, Args)
    ;
     dig_op_pattern(Pattern, 1, FormatCodes, Args, GP1, CP, C),
     (is_list(GP1) -> GP = [type-Name|GP1];GP=GP1),
     (FormatCodes = []
      -> Next = 1,
         Inst = Name
     ;
      append("~w(", FormatCodes, Prefix),
      append(Prefix, ")", InstFormatCodes),
      atom_codes(InstFormat, InstFormatCodes),
      format(atom(Inst), InstFormat, [Name|Args]),
      length(Pattern, ArgCount),
      Next is ArgCount + 1
     )
    ).

dig_op_table(t(TermType), ArgID, "~w(~w)", [Type,String], _GP, Size, CodePosition, Code) :-
    code(CodePosition, ArgID, Code, T),
    NextArg is ArgID + 1,
    (TermType = c -> Label = constant;Label = structure),
    (T = 0
      -> Type = seq,
         decode_switch_table_sequence(Label, NextArg, Size, TableCodes, CodePosition, Code),
         atom_codes(String, TableCodes)
    ;
     Type = hash,
     decode_switch_table_hash(Label, NextArg, String, Size)
    ).


dig_op_pattern([], _, [], [], GP, _, _) :-
    var(GP) -> GP = none
    ; true.
dig_op_pattern([H|T], ArgID, FormatCodes, Args, GP, CodePosition, Code) :-
    dig_op_pattern1(H, ArgID, FormatCodes1, Args1, GP, CodePosition, Code),
    (ArgID > 1, H \= v2 -> append(", ", Other, FormatCodes);Other=FormatCodes),
    append(FormatCodes1, FormatCodesTail, Other),
    append(Args1, ArgsTail, Args),
    ArgIDNext is ArgID + 1,
    dig_op_pattern(T, ArgIDNext, FormatCodesTail, ArgsTail, GP, CodePosition, Code).

dig_op_pattern1(pred, ArgID, "~w/~w", [Functor, Arity], [functor-Functor, arity-Arity, predicate-ArgVal], CodePosition, Code)  :-
    code(CodePosition, ArgID, Code, Arg),
    val(Arg, ArgVal),
    ftable(NameID / Arity, ArgVal),
    atable(Functor, NameID).
dig_op_pattern1(x, ArgID, "x(~w)", [Arg], _, CodePosition, Code) :-
    code(CodePosition, ArgID, Code, Arg).
dig_op_pattern1(y, ArgID, "y(~w)", [Arg], _, CodePosition, Code) :-
    code(CodePosition, ArgID, Code, Arg).
dig_op_pattern1(n, ArgID, "~w", [Arg], _, CodePosition, Code) :-
    code(CodePosition, ArgID, Code, Arg).
dig_op_pattern1(v1, ArgID, "~w", [Arg], _, CodePosition, Code) :-
    code(CodePosition, ArgID, Code, Arg1),
    (Arg1 = 0 -> Arg = x;Arg = y).
dig_op_pattern1(v2, ArgID, "(~w)", [Arg], _, CodePosition, Code) :-
    code(CodePosition, ArgID, Code, Arg).
dig_op_pattern1(ca, ArgID, "~w", [Arg], _, CodePosition, Code) :-
    code(CodePosition, ArgID, Code, Arg1),
    val(Arg1, Value),
    atable(Arg, Value).
dig_op_pattern1(ci, ArgID, "~w", [Arg], _, CodePosition, Code) :-
    code(CodePosition, ArgID, Code, Arg).
dig_op_pattern1(cf, ArgID, "~w", [Arg], _, CodePosition, Code) :-
    code(CodePosition, ArgID, Code, Arg1),
    val(Arg1, Value),
    fltable(Arg, Value).
dig_op_pattern1(a, ArgID, "~w", [Arg], _, CodePosition, Code) :-
    code(CodePosition, ArgID, Code, Arg1),
    decode_address(Arg1, Arg).


decode_address(Address, Decoded) :-
    fail_address(Address) -> Decoded = fail
    ; Address /\ 0x80000000 =:= 0 -> Decoded = clause_offset(Address)
    ; Decoded is Address xor 0x80000000 .

fail_address(1000000).

val(Term, Value) :-
    Value is Term /\ ((1 << 27)-1).

decode_switch_table_sequence(DataType, ArgID, Size, TableCodes, CodePosition, Code) :-
    code(CodePosition, ArgID, Code, N),
    Limit is 2*N,
    Size is 1+2*N,
    NextCP is CodePosition + ArgID + 1,
    decode_switch_table_sequence1(0, Limit, DataType, NextCP, Code, TableCodes).

decode_switch_table_sequence1(Limit, Limit, _DataType, _CodePosition, _Code, []) :- !.
decode_switch_table_sequence1(Offset, Limit, DataType, CodePosition, Code, TableCodes) :-
    Offset < Limit,
    decode_switch_table_sequence2(Offset, DataType, CodePosition, Code, EntryCodes),
    append(EntryCodes, Tail, TableCodes),
    Next is Offset + 2,
    decode_switch_table_sequence1(Next, Limit, DataType, CodePosition, Tail).

decode_switch_table_sequence2(Offset, DataType, CodePosition, Code, EntryCodes) :-
    code(CodePosition, Offset + 1, Code, K),
    code(CodePosition, Offset + 2, Code, VAddr),
    decode_address(VAddr, V),
    decode_switch_table_sequence3(DataType, K, C),
    (Offset > 0 -> append(", ", Tail, EntryCodes);Tail = EntryCodes),
    format(atom(Entry), "~w - ~w", [C, V]),
    atom_codes(Entry, Tail).

decode_switch_table_sequence3(constant, K, K).
decode_switch_table_sequence3(structure, K, Functor / Arity) :-
    val(K, KVal),
    ftable(NameID / Arity, KVal),
    atable(Functor, NameID).


decode_switch_table_hash(DataType, ArgID, Size, TableCodes, CodePosition, Code) :-
    code(CodePosition, ArgID, Code, N),
    Limit is N,
    SizeInit is 1+N,
    NextCP is CodePosition + ArgID + 1,
    decode_switch_table_hash1(0, Limit, DataType, NextCP, Code, TableCodes, SizeInit, Size).

decode_switch_table_hash1(Limit, Limit, _DataType, _CodePosition, _Code, [], Size, Size) :- !.
decode_switch_table_hash1(Offset, Limit, DataType, CodePosition, Code, TableCodes, SizeIn, SizeOut) :-
    Offset < Limit,
    decode_switch_table_hash2(Offset, DataType, CodePosition, Code, EntryCodes, BucketSize),
    append(EntryCodes, Tail, TableCodes),
    Next is Offset + 1,
    SizeNext is SizeIn + BucketSize,
    decode_switch_table_hash1(Next, Limit, DataType, CodePosition, Tail, SizeNext, SizeOut).

decode_switch_table_hash2(Offset, DataType, CodePosition, Code, EntryCodes, BucketSize) :-
    code(CodePosition, Offset + 1, Code, BA),
    (fail_address(BA)
      -> EntryCodes = "fail"
    ;
     BAX is BA xor 0x80000000,
     decode_switch_table_sequence(DataType, 0, BucketSize, SeqCodes, BAX, Code),
     append("[", SeqCodes, Prefix),
     append(Prefix, "]", EntryCodes)
    ).

