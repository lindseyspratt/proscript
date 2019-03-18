
function decode_instruction(predicateID, codePosition) {
    let predicate = (predicateID == null)?("no predicate"):(atable[ftable[predicateID.key][0]] + "/" + ftable[predicateID.key][1]);
    let op = code[codePosition];
    let instruction = '';

    switch(op) {
        // Control instructions 1-5
        case 1: // allocate
            instruction = 'allocate';
            break;
        case 2: // deallocate
            instruction = 'deallocate';
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

            instruction = 'call(' + functor + '/' + arity + ',' + N + ')';
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

            instruction = 'execute(' + functor + '/' + arity + ')';
            break;
        }
        case 5: // proceed
            instruction = 'proceed';
            break;

        // Put instructions 6-15, 51, and 60
        case 6: // put_variable: [6, N, I]
        {
            let N = code[codePosition + 1];
            let I = code[codePosition + 2];
            instruction = 'put_variable(y(' + N + '), x(' + I + '))';
            break;
        }
        case 60: // put_variable: [60, N]
        {
            let N = code[codePosition + 1];
            instruction = 'put_variable(y(' + N + '))';
            break;
        }
        case 7: // put_variable: [7, N, I]
        {
            let N = code[codePosition + 1];
            let I = code[codePosition + 2];
            instruction = 'put_variable(x(' + N + '), x(' + I + '))';
            break;
        }
        case 8: // put_value: [8, 0, N, I] or [8, 1, N, I]
        {
            let A = code[codePosition + 1];
            let N = code[codePosition + 2];
            let I = code[codePosition + 3];

            let V = (A === 0) ? 'y' : 'x';

            instruction = 'put_variable(' + V  + '(' + N + '), x(' + I + '))';
            break;
        }
        case 9: // put_unsafe_value: [9, N, I]
        {
            let N = code[codePosition + 1];
            let I = code[codePosition + 2];
            instruction = 'put_unsafe_value(y(' + N + '), x(' + I + '))';
            break;
        }
        case 10: // put_constant: [10, K, I]
        {
            let K = code[codePosition + 1];
            let I = code[codePosition + 2];

            let C = atable[VAL(K)];
            instruction = 'put_constant(' + C + ', x(' + I + '))';
            break;
        }
        case 11: // put_nil: [I]
        {
            let I = code[codePosition + 1];
            instruction = 'put_nil(x(' + I + '))';
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
            instruction = 'put_structure('  + functor + '/' + arity +  ', x(' + I + '))';
            break;
        }
        case 13: // put_list: [13, I]
        {
            let I = code[codePosition + 1];

            instruction = 'put_list(x(' + I + '))';
            break;
        }
        case 14: // put_integer: [14, C, I]
        {
            let C = code[codePosition + 1];
            let I = code[codePosition + 2];

            instruction = 'put_integer(' + C + ', x(' + I + '))';
            break;
        }
        case 51: // put_float: [51, N, I]
        {
            let N = code[codePosition + 1];
            let I = code[codePosition + 2];

            let C = floats[VAL(N)];
            instruction = 'put_float(' + C + ', x(' + I + '))';
            break;
        }

        // Get instructions 15-21 and 50.
        case 15: // get_variable: [15, 0, N, I] or [15, 1, N, I]
        {
            let A = code[codePosition + 1];
            let N = code[codePosition + 2];
            let I = code[codePosition + 3];

            let V = (A === 0) ? 'y' : 'x';

            instruction = 'get_variable(' + V  + '(' + N + '), x(' + I + '))';
            break;
        }
        case 16: // get_value: [16, 0, N, I] or [16, 1, N, I]
        {
            let A = code[codePosition + 1];
            let N = code[codePosition + 2];
            let I = code[codePosition + 3];

            let V = (A === 0) ? 'y' : 'x';

            instruction = 'get_value(' + V  + '(' + N + '), x(' + I + '))';
            break;
        }
        case 17: // get_constant: [17, K, I]
        {
            let K = code[codePosition + 1];
            let I = code[codePosition + 2];

            let C = atable[VAL(K)];
            instruction = 'get_constant(' + C + ', x(' + I + '))';
            break;
        }
        case 18: // get_nil: [18, I]
        {
            let I = code[codePosition + 1];
            instruction = 'get_nil(x(' + I + '))';
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
            instruction = 'get_structure('  + functor + '/' + arity +  ', x(' + I + '))';
            break;
        }
        case 20: // get_list: [20, I]
        {
            let I = code[codePosition + 1];

            instruction = 'get_list(x(' + I + '))';
            break;
        }
        case 21: // get_integer: [21, C, I]
        {
            let C = code[codePosition + 1];
            let I = code[codePosition + 2];

            instruction = 'get_integer(' + C + ', x(' + I + '))';
            break;
        }
        case 50: // get_float: [50, N, I]
        {
            let N = code[codePosition + 1];
            let I = code[codePosition + 2];

            let C = floats[VAL(N)];
            instruction = 'get_float(' + C + ', x(' + I + '))';
            break;
        }

        // Unify instructions 22-27 and 52
        case 22: // unify_void: [22, N]
        {
            let N = code[codePosition + 1];

            instruction = 'unify_void(' + N + ')';
            break;
        }
        case 23: // unify_variable: [23, 0, N] or [23, 1, N]
        {
            let A = code[codePosition + 1];
            let N = code[codePosition + 2];

            let V = (A === 0) ? 'y' : 'x';

            instruction = 'unify_variable(' + V  + '(' + N + ')';
            break;
        }
        case 24: // unify_value: [24, 0, N] or [24, 1, N]
        {
            let A = code[codePosition + 1];
            let N = code[codePosition + 2];

            let V = (A === 0) ? 'y' : 'x';

            instruction = 'unify_value(' + V  + '(' + N + ')';
            break;
        }
        case 25: // unify_local_value: [25, 0, N] or [25, 1, N]
        {
            let A = code[codePosition + 1];
            let N = code[codePosition + 2];

            let V = (A === 0) ? 'y' : 'x';

            instruction = 'unify_local_value(' + V  + '(' + N + ')';
            break;
        }
        case 26: // unify_constant: [26, K]
        {
            let K = code[codePosition + 1];

            let C = atable[VAL(K)];
            instruction = 'unify_constant(' + C + ')';
            break;
        }
        case 27: // unify_integer: [27, C]
        {
            let C = code[codePosition + 1];

            instruction = 'unify_integer(' + C + ')';
            break;
        }
        case 52: // unify_float: [52, N]
        {
            let N = code[codePosition + 1];

            let C = floats[VAL(N)];
            instruction = 'unify_float(' + C + ')';
            break;
        }
            // Indexing instructions 28-30
        case 28: // try_me_else: [28, L]
        {
            let L = code[codePosition + 1];

            instruction = 'try_me_else(' + L + ')';
            break;
        }
        case 29: // retry_me_else: [29, L]
        {
            let L = code[codePosition + 1];

            instruction = 'retry_me_else(' + L + ')';
            break;
        }
        case 30: // trust_me: [30, 0]
        {
            instruction = 'trust_me(0)';
            break;
        }

        // Cut instructions
        case 31: // neck_cut: [31]
        {
            instruction = 'neck_cut';
            break;
        }
        case 32: // cut: [32, I]
        {
            let I = code[codePosition + 1];

            instruction = 'cut(y(' + I + '))';
            break;
        }
        case 33: // get_level: [33, I]
        {
            let I = code[codePosition + 1];

            instruction = 'get_level(y(' + I + '))';
            break;
        }

        // Aux instructions. Used for ; and ->. Basically just call with an offset rather than a functor to look up
        case 40: // call_aux: [40, P, A, N]
        {
            let P = code[codePosition + 1];
            let A = code[codePosition + 2];
            let N = code[codePosition + 3];

            instruction = 'call_aux(' + P + ',' + A + ',' + N +'))';
            break;
        }
        case 41: // execute_aux: [41, P, A]
        {
            let P = code[codePosition + 1];
            let A = code[codePosition + 2];

            instruction = 'execute_aux(' + P + ',' + A +'))';
            break;
        }
        // retry_foreign is for foreign predicates with nondeterministic behaviour
        case 42: // retry_foreign: [42]
        {
            instruction = 'retry_foreign';
            break;
        }
            // get_choicepoint is used for setup_call_cleanup
            //encode_opcode(get_choicepoint(N, y(I)), 3, [43, N, I]).
        case 43: // get_choicepoint: [43, N, I]
        {
            let N = code[codePosition + 1];
            let I = code[codePosition + 2];
            instruction = 'get_choicepoint(' + N + ', y(' + I + '))';
            break;
        }
        case 254: // nop2: [254, 0]
        {
            instruction = 'nop2(0)';
            break;
        }

        default:
            instruction = 'unknown(' + op + ')';
            break;
    }

    return (predicate + ':' + '(' + instruction + ',' + codePosition + ')');

}
