
function decode_instruction(predicateID, codePosition) {
    return decode_instruction_general(predicateID, codePosition, code);
}

// Binds 4th parameter to structure of the form: instruction(String, Op, OpName, Size, GoalPredicate),
// GoalPredicate = goal_predicate(Functor, Arity, PredicateID, Type)
// Base on info found at codePosition'th element of codeP list (0 based).
// The instruction String incorporates the predicateNameP as part of its label.
// The String is a list of character codes, e.g. "foo: bar" (using Prolog shorthand notation for string).

function predicate_decode_instruction(predicateNameP, codeP, codePositionP, instructionP) {
    let predicateNameJS;
    if(TAG(predicateNameP) === TAG_REF) {
        return instantiation_error(predicateNameP);
    } else if(TAG(predicateNameP) === TAG_ATM) {
        predicateNameJS = PL_get_atom_chars(predicateNameP);
    } else if(TAG(predicateNameP) === TAG_INT) {
        predicateNameJS = PL_get_integer(predicateNameP);
    } else {
        return type_error('predicate ID or name', predicateNameP);
    }

    if(TAG(codeP) === TAG_REF) {
        return instantiation_error(codeP);
    } else if(TAG(codeP) !== TAG_LST) {
        return type_error('list', codeP);
    }

    if(TAG(codePositionP) === TAG_REF) {
        return instantiation_error(codePositionP);
    } else if(TAG(codePositionP) !== TAG_INT) {
        return type_error('integer', codePositionP);
    }

    if(TAG(instructionP) !== TAG_REF && TAG(instructionP) !== TAG_STR) {
        return type_error('variable or instruction/5 structure', instructionP);
    }

    let codePositionJS = PL_get_integer(codePositionP);
    let codeJS = integer_list_to_term_array(codeP);
    let instruction = decode_instruction_general(predicateNameJS, codePositionJS, codeJS);
    let internalInstructionP = make_instruction_structure(instruction);
    return unify(instructionP, internalInstructionP);
}

// instruction = {string: instString, op: op, opName:opName, size:instructionSize, goalPredicate:goalPredicate};
// goalPredicate = {functor: functor, arity: arity, predicate: i, type: 'call'};
function make_instruction_structure(instruction) {
    let goalPredicate = instruction.goalPredicate;

    let goalPredicateP;
    if(typeof goalPredicate === 'string') {
        goalPredicateP = PL_put_atom_chars(goalPredicate);
    } else {
        let ftorGP = lookup_functor('goal_predicate', 4);
        goalPredicateP = alloc_structure(ftorGP);
        memory[state.H++] = PL_put_atom_chars(goalPredicate.functor);
        memory[state.H++] = PL_put_integer(goalPredicate.arity);
        memory[state.H++] = PL_put_integer(goalPredicate.predicate);
        memory[state.H++] = PL_put_atom_chars(goalPredicate.type);
    }

    let instructionStringP = string_to_codes(instruction.string);

    let ftor = lookup_functor('instruction', 5);
    let instructionP = alloc_structure(ftor);
    memory[state.H++] = instructionStringP;
    memory[state.H++] = PL_put_integer(instruction.op);
    memory[state.H++] = PL_put_atom_chars(instruction.opName);
    memory[state.H++] = PL_put_integer(instruction.size);
    memory[state.H++] = goalPredicateP;

    return instructionP;
}

function initOpNames() {
    let opNames = [];
    opNames[0] = 'zero';
    opNames[1] = 'allocate';
    opNames[2] = 'deallocate';
    opNames[3] = 'call';
    opNames[4] = 'execute';
    opNames[5] = 'proceed';
    opNames[6] = 'put_variable'; // variable to Y and X registers.
    opNames[60] = 'put_variable'; // Y register variable only
    opNames[7] = 'put_variable'; // variable to X and X registers.
    opNames[8] = 'put_value';
    opNames[9] = 'put_unsafe_value';
    opNames[10] = 'put_constant';
    opNames[11] = 'put_nil';
    opNames[12] = 'put_structure';
    opNames[13] = 'put_list';
    opNames[14] = 'put_integer';
    opNames[51] = 'put_float';
    opNames[15] = 'get_variable';
    opNames[16] = 'get_value';
    opNames[17] = 'get_constant';
    opNames[18] = 'get_nil';
    opNames[19] = 'get_structure';
    opNames[20] = 'get_list';
    opNames[21] = 'get_integer';
    opNames[50] = 'get_float';
    opNames[22] = 'unify_void';
    opNames[23] = 'unify_variable';
    opNames[24] = 'unify_value';
    opNames[25] = 'unify_local_value';
    opNames[26] = 'unify_constant';
    opNames[27] = 'unify_integer';
    opNames[52] = 'unify_float';
    opNames[28] = 'try_me_else';
    opNames[29] = 'retry_me_else';
    opNames[30] = 'trust_me';
    opNames[31] = 'neck_cut';
    opNames[32] = 'cut';
    opNames[33] = 'get_level';
    opNames[40] = 'call_aux';
    opNames[41] = 'execute_aux';
    opNames[42] = 'retry_foreign';
    opNames[43] = 'get_choicepoint';
    opNames[44] = 'switch_on_term';
    opNames[45] = 'switch_on_constant';
    opNames[46] = 'switch_on_structure';
    opNames[71] = 'try';
    opNames[72] = 'retry';
    opNames[73] = 'trust';
    opNames[74] = 'goto_clause';
    opNames[254] = 'nop2';

    return opNames;
}

let opNames = initOpNames();

function decode_instruction_general(predicateID, codePosition, code) {
    let predicateName; // = (predicateID == null) ? ("no predicate") : (atable[ftable[parseInt(predicateID.key)][0]] + "/" + ftable[parseInt(predicateID.key)][1]);
    if(typeof predicateID === 'undefined' || predicateID == null) {
        predicateName = 'no predicate';
    } else if(predicateID.key) {
        predicateName = atable[ftable[parseInt(predicateID.key)][0]] + "/" + ftable[parseInt(predicateID.key)][1]
    } else if(typeof predicateID === 'number') {
        predicateName = atable[ftable[predicateID][0]] + "/" + ftable[predicateID][1]
    } else if(typeof predicateID === 'string') {
        predicateName = predicateID;
    } else {
        predicateName = JSON.stringify(predicateID);
    }

    let op = code[codePosition];
    let instruction = '';
    let instructionSize = -1;
    let goalPredicate = 'none';

    switch(op) {
        // Control instructions 1-5
        case 1: // allocate
            instruction = opNames[op];
            instructionSize = 1;
            break;
        case 2: // deallocate
            instruction = opNames[op];
            instructionSize = 1;
            break;
        case 3: // call: [3, I, N]
        {
            let I = code[codePosition + 1];
            let N = code[codePosition + 2];

            // I = i ^ (TAG_ATM << WORD_BITS)
            let i = VAL(I);
            let nameID = ftable[i][0];
            let functor = atable[nameID];
            let arity = ftable[i][1];

            instruction = opNames[op] + '(' + functor + '/' + arity + ',' + N + ')';
            instructionSize = 3;
            goalPredicate = {functor: functor, arity: arity, predicate: i, type: 'call'};
            break;
        }
        case 4: // execute: [4, I]
        {
            let I = code[codePosition + 1];

            // I = i ^ (TAG_ATM << WORD_BITS)
            let i = VAL(I);
            let nameID = ftable[i][0];
            let functor = atable[nameID];
            let arity = ftable[i][1];

            instruction = opNames[op] + '(' + functor + '/' + arity + ')';
            instructionSize = 2;
            goalPredicate = {functor: functor, arity: arity, predicate: i, type: 'execute'};
            break;
        }
        case 5: // proceed
            instruction = opNames[op];
            instructionSize = 1;
            break;

        // Put instructions 6-15, 51, and 60
        case 6: // put_variable: [6, N, I]
        {
            let N = code[codePosition + 1];
            let I = code[codePosition + 2];
            instruction = opNames[op] + '(y(' + N + '), x(' + I + '))';
            instructionSize = 3;
           break;
        }
        case 60: // put_variable: [60, N]
        {
            let N = code[codePosition + 1];
            instruction = opNames[op] + '(y(' + N + '))';
            instructionSize = 2;
            break;
        }
        case 7: // put_variable: [7, N, I]
        {
            let N = code[codePosition + 1];
            let I = code[codePosition + 2];
            instruction = opNames[op] + '(x(' + N + '), x(' + I + '))';
            instructionSize = 3;
            break;
        }
        case 8: // put_value: [8, 0, N, I] or [8, 1, N, I]
        {
            let A = code[codePosition + 1];
            let N = code[codePosition + 2];
            let I = code[codePosition + 3];

            let V = (A === 0) ? 'y' : 'x';

            instruction = opNames[op] + '(' + V  + '(' + N + '), x(' + I + '))';
            instructionSize = 4;
            break;
        }
        case 9: // put_unsafe_value: [9, N, I]
        {
            let N = code[codePosition + 1];
            let I = code[codePosition + 2];
            instruction = opNames[op] + '(y(' + N + '), x(' + I + '))';
            instructionSize = 3;
            break;
        }
        case 10: // put_constant: [10, K, I]
        {
            let K = code[codePosition + 1];
            let I = code[codePosition + 2];

            let C = atable[VAL(K)];
            instruction = opNames[op] + '(' + C + ', x(' + I + '))';
            instructionSize = 3;
            break;
        }
        case 11: // put_nil: [I]
        {
            let I = code[codePosition + 1];
            instruction = opNames[op] + '(x(' + I + '))';
            instructionSize = 1;
            break;
        }
        case 12: // put_structure: [12, F, I]
        {
            let F = code[codePosition + 1];
            let I = code[codePosition + 2];

            let f = VAL(F);
            let nameID = ftable[f][0];
            let functor = atable[nameID];
            let arity = ftable[f][1];
            instruction = opNames[op] + '('  + functor + '/' + arity +  ', x(' + I + '))';
            instructionSize = 3;
            break;
        }
        case 13: // put_list: [13, I]
        {
            let I = code[codePosition + 1];

            instruction = opNames[op] + '(x(' + I + '))';
            instructionSize = 2;
            break;
        }
        case 14: // put_integer: [14, C, I]
        {
            let C = code[codePosition + 1];
            let I = code[codePosition + 2];

            instruction = opNames[op] + '(' + C + ', x(' + I + '))';
            instructionSize = 3;
            break;
        }
        case 51: // put_float: [51, N, I]
        {
            let N = code[codePosition + 1];
            let I = code[codePosition + 2];

            let C = floats[VAL(N)];
            instruction = opNames[op] + '(' + C + ', x(' + I + '))';
            instructionSize = 3;
            break;
        }

        // Get instructions 15-21 and 50.
        case 15: // get_variable: [15, 0, N, I] or [15, 1, N, I]
        {
            let A = code[codePosition + 1];
            let N = code[codePosition + 2];
            let I = code[codePosition + 3];

            let V = (A === 0) ? 'y' : 'x';

            instruction = opNames[op] + '(' + V  + '(' + N + '), x(' + I + '))';
            instructionSize = 4;
            break;
        }
        case 16: // get_value: [16, 0, N, I] or [16, 1, N, I]
        {
            let A = code[codePosition + 1];
            let N = code[codePosition + 2];
            let I = code[codePosition + 3];

            let V = (A === 0) ? 'y' : 'x';

            instruction = opNames[op] + '(' + V  + '(' + N + '), x(' + I + '))';
            instructionSize = 4;
            break;
        }
        case 17: // get_constant: [17, K, I]
        {
            let K = code[codePosition + 1];
            let I = code[codePosition + 2];

            let C = atable[VAL(K)];
            instruction = opNames[op] + '(' + C + ', x(' + I + '))';
            instructionSize = 3;
            break;
        }
        case 18: // get_nil: [18, I]
        {
            let I = code[codePosition + 1];
            instruction = opNames[op] + '(x(' + I + '))';
            instructionSize = 2;
            break;
        }
        case 19: // get_structure: [19, F, I]
        {
            let F = code[codePosition + 1];
            let I = code[codePosition + 2];

            let f = VAL(F);
            let nameID = ftable[f][0];
            let functor = atable[nameID];
            let arity = ftable[f][1];
            instruction = opNames[op] + '('  + functor + '/' + arity +  ', x(' + I + '))';
            instructionSize = 3;
            break;
        }
        case 20: // get_list: [20, I]
        {
            let I = code[codePosition + 1];

            instruction = opNames[op] + '(x(' + I + '))';
            instructionSize = 2;
            break;
        }
        case 21: // get_integer: [21, C, I]
        {
            let C = code[codePosition + 1];
            let I = code[codePosition + 2];

            instruction = opNames[op] + '(' + C + ', x(' + I + '))';
            instructionSize = 3;
            break;
        }
        case 50: // get_float: [50, N, I]
        {
            let N = code[codePosition + 1];
            let I = code[codePosition + 2];

            let C = floats[VAL(N)];
            instruction = opNames[op] + '(' + C + ', x(' + I + '))';
            instructionSize = 3;
            break;
        }

        // Unify instructions 22-27 and 52
        case 22: // unify_void: [22, N]
        {
            let N = code[codePosition + 1];

            instruction = opNames[op] + '(' + N + ')';
            instructionSize = 2;
            break;
        }
        case 23: // unify_variable: [23, 0, N] or [23, 1, N]
        {
            let A = code[codePosition + 1];
            let N = code[codePosition + 2];

            let V = (A === 0) ? 'y' : 'x';

            instruction = opNames[op] + '(' + V  + '(' + N + ')';
            instructionSize = 3;
            break;
        }
        case 24: // unify_value: [24, 0, N] or [24, 1, N]
        {
            let A = code[codePosition + 1];
            let N = code[codePosition + 2];

            let V = (A === 0) ? 'y' : 'x';

            instruction = opNames[op] + '(' + V  + '(' + N + ')';
            instructionSize = 3;
            break;
        }
        case 25: // unify_local_value: [25, 0, N] or [25, 1, N]
        {
            let A = code[codePosition + 1];
            let N = code[codePosition + 2];

            let V = (A === 0) ? 'y' : 'x';

            instruction = opNames[op] + '(' + V  + '(' + N + ')';
            instructionSize = 3;
            break;
        }
        case 26: // unify_constant: [26, K]
        {
            let K = code[codePosition + 1];

            let C = atable[VAL(K)];
            instruction = opNames[op] + '(' + C + ')';
            instructionSize = 2;
            break;
        }
        case 27: // unify_integer: [27, C]
        {
            let C = code[codePosition + 1];

            instruction = opNames[op] + '(' + C + ')';
            instructionSize = 2;
            break;
        }
        case 52: // unify_float: [52, N]
        {
            let N = code[codePosition + 1];

            let C = floats[VAL(N)];
            instruction = opNames[op] + '(' + C + ')';
            instructionSize = 2;
            break;
        }
            // Indexing instructions 28-30
        case 28: // try_me_else: [28, L]
        {
            let L = code[codePosition + 1];

            instruction = opNames[op] + '(' + L + ')';
            instructionSize = 2;
            break;
        }
        case 29: // retry_me_else: [29, L]
        {
            let L = code[codePosition + 1];

            instruction = opNames[op] + '(' + L + ')';
            instructionSize = 2;
            break;
        }
        case 30: // trust_me: [30, 0]
        {
            instruction = opNames[op] + '(0)';
            instructionSize = 2;
            break;
        }

        // Cut instructions
        case 31: // neck_cut: [31]
        {
            instruction = opNames[op];
            instructionSize = 1;
            break;
        }
        case 32: // cut: [32, I]
        {
            let I = code[codePosition + 1];

            instruction = opNames[op] + '(y(' + I + '))';
            instructionSize = 2;
            break;
        }
        case 33: // get_level: [33, I]
        {
            let I = code[codePosition + 1];

            instruction = opNames[op] + '(y(' + I + '))';
            instructionSize = 2;
            break;
        }

        // Aux instructions. Used for ; and ->. Basically just call with an offset rather than a functor to look up
        case 40: // call_aux: [40, P, A, N]
        {
            let P = code[codePosition + 1];
            let A = code[codePosition + 2];
            let N = code[codePosition + 3];

            instruction = opNames[op] + '(' + P + ',' + A + ',' + N +'))';
            instructionSize = 4;
            break;
        }
        case 41: // execute_aux: [41, P, A]
        {
            let P = code[codePosition + 1];
            let A = code[codePosition + 2];

            instruction = opNames[op] + '(' + P + ',' + A +'))';
            instructionSize = 3;
            break;
        }
        // retry_foreign is for foreign predicates with non-deterministic behaviour
        case 42: // retry_foreign: [42]
        {
            instruction = opNames[op];
            instructionSize = 1;
            break;
        }
            // get_choicepoint is used for setup_call_cleanup
            //encode_opcode(get_choicepoint(N, y(I)), 3, [43, N, I]).
        case 43: // get_choicepoint: [43, N, I]
        {
            let N = code[codePosition + 1];
            let I = code[codePosition + 2];
            instruction = opNames[op] + '(' + N + ', y(' + I + '))';
            instructionSize = 3;
            break;
        }
        case 44: // switch_on_term: [44, V, CA, CI, CF, L, S]
        {
            let V = code[codePosition + 1];
            let CA = decode_address(code[codePosition + 2]);
            let CI = decode_address(code[codePosition + 3]);
            let CF = decode_address(code[codePosition + 4]);
            let L = decode_address(code[codePosition + 5]);
            let S = decode_address(code[codePosition + 6]);

            instruction = opNames[op] + '(' + V + ', ' + CA + ', ' + CI + ', ' + CF + ', ' + L + ', ' + S + ')';
            instructionSize = 7;
            break;
        }
        case 45: // switch_on_constant: [45, T...]
        {
            let T = code[codePosition + 1];
            let table;
            let size;
            if(T === 0) {
                let decoding = decode_switch_table_sequence('constant',codePosition + 2);
                size = decoding.size;
                table = 'seq(' + decoding.string + ')';
            } else if(T === 1) {
                let decoding = decode_switch_table_hash('constant',codePosition + 2);
                size = decoding.size;
                table = 'hash(' + decoding.string + ')';
            }
            instruction = opNames[op] + '(' + table + ')';
            instructionSize = 2 + size;
            break;
        }
        case 46: // switch_on_structure: [46, T...]
        {
            let T = code[codePosition + 1];
            let table = '';
            let size;
            if(T === 0) {
                let decoding = decode_switch_table_sequence('structure',codePosition + 2);
                size = decoding.size;
                table = 'seq(' + decoding.string + ')';
            } else if(T === 1) {
                let decoding = decode_switch_table_hash('structure',codePosition + 2);
                size = decoding.size;
                table = 'hash(' + decoding.string + ')';
            }
            instruction = opNames[op] + '(' + T + ', ' + table + ')';
            instructionSize = 2 + size;
            break;
        }
        case 71: // try: [71, L]
        {
            let L = code[codePosition + 1];
            instruction = opNames[op] + '(' + L + ')';
            instructionSize = 2;
            break;
        }
        case 72: // retry: [72, L]
        {
            let L = code[codePosition + 1];
            instruction = opNames[op] + '(' + L + ')';
            instructionSize = 2;
            break;
        }
        case 73: // trust: [73, L]
        {
            let L = code[codePosition + 1];
            instruction = opNames[op] + '(' + L + ')';
            instructionSize = 2;
            break;
        }
        case 74: // goto_clause: [74, L]
        {
            let L = code[codePosition + 1];
            instruction = opNames[op] + '(' + L + ')';
            instructionSize = 2;
            break;
        }
        case 254: // nop2: [254, 0]
        {
            instruction = opNames[op] + '(0)';
            instructionSize = 2;
            break;
        }

        default:
            instruction = 'unknown(' + op + ')';
            instructionSize = 1;
            break;
    }

    return {string: (predicateName + ':' + '(' + instruction + ',' + codePosition + ')'), op: op, opName: opNames[op], size:instructionSize, goalPredicate:goalPredicate};

}

function decode_address(address) {
    if( address === FAIL_ADDRESS) {
        return 'fail';
    }
    if((address & 0x80000000) === 0) {
        return 'clause_offset(' + address + ')';
    } else {
        return address ^ 0x80000000;
    }
}

function decode_switch_table_sequence(dataType, codePosition) {
    let N = code[codePosition];
    let table = '';
    for(let ofst = 0;ofst < 2*N;ofst+=2) {
        let K = code[codePosition + 1 + ofst];
        let V = decode_address(code[codePosition + ofst + 2]);
        let C;
        if(dataType === 'constant'){
            C = K;
        } else if(dataType === 'structure'){
            // K = k ^ (TAG_ATM << WORD_BITS)
            let k = VAL(K);
            let nameID = ftable[k][0];
            let functor = atable[nameID];
            let arity = ftable[k][1];
            C = functor + '/' + arity;
        } else {
            throw 'invalid data type: ' + dataType;
        }

        table += ((table !== '') ? ', ' : '') + C + ' - ' + V;
    }
    let size = 1+2*N;
    return {size: size, string: table};
}

function decode_switch_table_hash(dataType, codePosition) {
    // hash: Size, BA1, ..., BASize, B1N, B1K1, B1V1, ..., B1KN, B1VN, B2N, ...
    // hash([a,b...],[c,d,...],...)

    let N = code[codePosition];
    let table = '';
    let size = N + 1;
    for(let ofst = 0;ofst < N;ofst++) {
        let BA = code[codePosition + 1 + ofst];
        let seq;
        if(BA === FAIL_ADDRESS) {
            seq = 'fail';
        } else {
            let BAX = BA ^ 0x80000000;
            let decoding = decode_switch_table_sequence(dataType, BAX);
            size += decoding.size;
            seq = '[' + decoding.string + ']';
        }
        table += ((table !== '') ? ', ' : '') + seq ;
    }
    return {size: size, string: table};
}

/*
codePosition = 31, the call of bootstrap_js:retract/1.
Argument 1 is in register[0] (= x(0)).
Argument 1 = wam_compiler : delayed_initialization(Y0).
x(0) -> structure :/2, arg 1 = wam_compiler, arg 2 = x(2)
x(2) -> structure delayed_initialization/1, arg 1 = y(0).

wam_compiler:process_delayed_initializations/0:(put_structure(delayed_initialization/1, x(2)),17)
wam_compiler:process_delayed_initializations/0:(unify_local_value(y(0),20)
wam_compiler:process_delayed_initializations/0:(put_structure(:/2, x(0)),23)
wam_compiler:process_delayed_initializations/0:(unify_constant(wam_compiler),26)
wam_compiler:process_delayed_initializations/0:(unify_value(x(2),28)
wam_compiler:process_delayed_initializations/0:(call(bootstrap_js:retract/1,1),31)

 */
const PUT_STRUCTURE_OP = 12;
const UNIFY_CONSTANT_OP = 26;
const UNIFY_VALUE_OP = 24;

function decode_retract_argument(callCodePosition, opCodePositions) {
    let op = code[callCodePosition];
    // op is either call or execute.
    // In either case, argument 1 is held in x(0).
    // Search back for codeb
    let arg = register[0];
    let ftor = VAL(memory[VAL(arg)]);
    let nameID = ftable[ftor][0];
    let functor = atable[nameID];
    let arity = ftable[ftor][1];

    let colonFunctorID = VAL(lookup_functor(':', 2));

    // find prev put_structure for :/2
    let callOfst = opCodePositions.indexOf(callCodePosition);
    for(let ofst = callOfst-1;ofst >= 0; ofst--) {
       let prevOpCP = opCodePositions[ofst];
       let prevOp = code[prevOpCP];
       if(prevOp === PUT_STRUCTURE_OP) { // [PUT_STRUCTURE_OP, F, I]
           let F = code[prevOpCP + 1];
           let I = code[prevOpCP + 2];

           let f = VAL(F);
           let nameID = ftable[f][0];
           let functor = atable[nameID];
           let arity = ftable[f][1];
           //instruction = 'put_structure('  + functor + '/' + arity +  ', x(' + I + '))';
           if(I === 0) {
               if(f === colonFunctorID) {
                   let moduleOpOfst = ofst + 1;
                   let functorOpOfst = ofst + 2;

                   let moduleOpCP = opCodePositions[moduleOpOfst];
                   let moduleOp = code[moduleOpCP];
                   if (moduleOp === UNIFY_CONSTANT_OP) { // unify_constant: [26, K]
                       let moduleNameID = code[moduleOpCP + 1];
                       let moduleName = atable[moduleNameID];

                       let functorOpCP = opCodePositions[functorOpOfst];
                       let functorOp = code[functorOpCP];
                       if (functorOp === UNIFY_VALUE_OP) { // unify_value: [24, 0, N] or [24, 1, N]
                           if (code[functorOpCP + 1] === 1) { // 1 -> X register, 0 -> Y register.
                               let targetRegister = code[functorOpCP + 2];
                               for (let subofst = functorOpOfst; subofst >= 0; subofst--) {
                                   let subOpCP = opCodePositions[subofst];
                                   let subOp = code[subOpCP];
                                   if (subOp === PUT_STRUCTURE_OP && code[subOpCP + 2] === targetRegister) {
                                       let targetF = code[subOpCP + 1];
                                       let targetFtor = VAL(targetF);
                                       return {module: moduleNameID, ftor: targetFtor};
                                   }
                               }
                           }
                       }
                   }
               }
               return {};
           } else if(prevOp === CALL_OP || prevOp === EXECUTE_OP) {
               return {};
           }
       }
    }
    return {};
}

/*
            let F = code[codePosition + 1];
            let I = code[codePosition + 2];

            let f = VAL(F);
            let nameID = ftable[f][0];
            let functor = atable[nameID];
            let arity = ftable[f][1];
            instruction = 'put_structure('  + functor + '/' + arity +  ', x(' + I + '))';

 */