"use strict";

let compilation_environment = {
    buffer: [],
    indexing_mode: 'basic'
};

// Round toward zero by default
function round(a)
{
    if (a < 0)
        return Math.ceil(a);
    else
        return Math.floor(a);
}


function evaluate_expression(expression, evaluated)
{
    expression = deref(expression);
    if (TAG(expression) === TAG_INT)
    {
        if ((VAL(expression) & (1 << (WORD_BITS-1))) === (1 << (WORD_BITS-1)))
            evaluated.value = VAL(expression) - (1 << WORD_BITS);
        else
            evaluated.value = VAL(expression);
        return true;
    }
    if (TAG(expression) === TAG_FLT)
    {
        evaluated.value = floats[VAL(expression)];
        return true;
    }
    if (TAG(expression) === TAG_REF)
    {
        return instantiation_error(expression);
    }
    else if (TAG(expression) === TAG_ATM && expression === lookup_atom("pi"))
    {
        evaluated.value = Math.PI;
        return true;
    }
    else if (TAG(expression) === TAG_ATM && expression === lookup_atom("e"))
    {
        evaluated.value = Math.E;
        return true;
    }
    else if (TAG(expression) === TAG_STR)
    {
        var indicator;
        var v = [];
        var arity = ftable[VAL(memory[VAL(expression)])][1];
        var name = atable[ftable[VAL(memory[VAL(expression)])][0]];
        for (var i = 0; i < arity; i++)
        {
            var t = {};
            if (!evaluate_expression(memory[VAL(expression)+i+1], t))
                return false;
            else
                v[i] = t.value;
        }
        if (name === "+" && arity === 2)
            evaluated.value = v[0] + v[1];
        else if (name === "-" && arity === 2)
            evaluated.value = v[0] - v[1];
        else if (name === "*" && arity === 2)
            evaluated.value = v[0] * v[1];
        else if (name === "//" && arity === 2)
            evaluated.value = ~~(v[0] / v[1]);
        else if (name === "/" && arity === 2)
            evaluated.value = v[0] / v[1];
        else if (name === "rem" && arity === 2)
        {
            if (v[1] === 0)
                return evaluation_error("zero_divisor");
            evaluated.value = v[0] - (round(v[0]/v[1]) * v[1]);            
        }
        else if (name === "mod" && arity === 2)
        {
            if (v[1] === 0)
                return evaluation_error("zero_divisor");            
            evaluated.value = v[0] - (Math.floor(v[0]/v[1]) * v[1]);
        }
        else if (name === "-" && arity === 1)
            evaluated.value = -v[0];
        else if (name === "abs" && arity === 1)
            evaluated.value = Math.abs(v[0]);
        else if (name === "sign" && arity === 1)
        {
            if (v[0] === 0)
                evaluated.value = 0;
            else if (v[0] > 0)
                evaluated.value = 1;
            else
                evaluated.value = -1;
        }
        else if (name === "float_integer_part" && arity === 1)
            evaluated.value = ~~v[0];
        else if (name === "float_fractional_part" && arity === 1)
            evaluated.value = v[0] % 1;
        else if (name === "float" && arity === 1)
            evaluated.value = v[0];
        else if (name === "floor" && arity === 1)
            evaluated.value = Math.floor(v[0]);
        else if (name === "truncate" && arity === 1)
            evaluated.value = ~~v[0];
        else if (name === "round" && arity === 1)
            evaluated.value = Math.round(v[0]);
        else if (name === "ceiling" && arity === 1)
            evaluated.value = Math.ceil(v[0]);
        else if (name === "**" && arity === 2)
            evaluated.value = Math.pow(v[0], v[1]);
        else if (name === "sin" && arity === 1)
            evaluated.value = Math.sin(v[0]);
        else if (name === "cos" && arity === 1)
            evaluated.value = Math.cos(v[0]);
        else if (name === "atan" && arity === 1)
            evaluated.value = Math.atan(v[0]);
        else if (name === "exp" && arity === 1)
            evaluated.value = Math.exp(v[0]);
        else if (name === "log" && arity === 1)
            evaluated.value = Math.log(v[0]);
        else if (name === "sqrt" && arity === 1)
            evaluated.value = Math.sqrt(v[0]);
        else if (name === ">>" && arity === 2)
            evaluated.value = v[0] >> v[1];
        else if (name === "<<" && arity === 2)
            evaluated.value = v[0] << v[1];
        else if (name === "/\\" && arity === 2)
            evaluated.value = v[0] & v[1];
        else if (name === "\\/" && arity === 2)
            evaluated.value = v[0] | v[1];
        else if (name === "\\" && arity === 1)
            evaluated.value = ~v[0];
        // Corrigendum
        else if (name === "+" && arity === 1)
            evaluated.value = v[0];
        else if (name === "max" && arity === 2)
            evaluated.value = Math.max(v[0], v[1]);
        else if (name === "min" && arity === 2)
            evaluated.value = Math.min(v[0], v[1]);
        else if (name === "acos" && arity === 1)
            evaluated.value = Math.acos(v[0]);
        else if (name === "asin" && arity === 1)
            evaluated.value = Math.asin(v[0]);
        else if (name === "tan" && arity === 1)
            evaluated.value = Math.tan(v[0]);
        else if (name === "xor" && arity === 2)
            evaluated.value = v[0] ^ v[1];
        else if (name === "atan2" && arity === 2)
            evaluated.value = Math.atan2(v[0], v[1]);
        else if (name === "^" && arity === 2)
            evaluated.value = Math.pow(v[0], v[1]);
        else if (name === "div" && arity === 2)
        {
            if (v[1] === 0)
                return evaluation_error("zero_divisor");
            evaluated.value = round(v[0] /v[1]);        
        }
        else
        {
            indicator = state.H ^ (TAG_STR << WORD_BITS);
            memory[state.H++] = lookup_functor("/", 2);
            memory[state.H++] = lookup_atom(name);
            memory[state.H++] = arity ^ (TAG_INT << WORD_BITS);
            return type_error("evaluable", indicator)
        }

        return true;
    }
    else if (TAG(expression) === TAG_ATM)
    {
        indicator = state.H ^ (TAG_STR << WORD_BITS);
        memory[state.H++] = lookup_functor("/", 2);
        memory[state.H++] = expression;
        memory[state.H++] = 0 ^ (TAG_INT << WORD_BITS);
        return type_error("evaluable", indicator)
    }
    else if (TAG(expression) === TAG_STR)
    {
        return type_error("evaluable", expression);
    }
    abort("Illegal type");
    return {status:0};
}

function term_from_list(list, tail)
{
    var tmp = state.H;
    for (var i = 0; i < list.length; i++)
    {
        alloc_list();
        memory[state.H++] = list[i];        
    }
    memory[state.H++] = tail;
    return memory[tmp];
}

function predicate_fail()
{
    return false;
}

function predicate_true()
{
    return true;
}


function predicate_is(value, expression)
{
    var e = {};
    if (!evaluate_expression(expression, e))
        return false;
//    if (e.status)
//        debug_msg("Evaluated " + term_to_string(expression) + " to " + e.value);
//    else
//        debug_msg("Failed to evaluate " + term_to_string(expression));
    // Note that e.value may be negative. Have to AND to get rid of any high bits
    if (e.value === ~~e.value)
    {
        // FIXME: Is this right?! This just truncates to WORD_BITS bits!
        e.value &= ((1 << WORD_BITS) -1);
        return (e.status !== 0 && unify(e.value ^ (TAG_INT << WORD_BITS), value));
    }
    else
    {
        return (e.status !== 0 && unify(lookup_float(e.value), value));
    }
}


function predicate_ne(a, b)
{
    var ae = {};
    var be = {};
    if (evaluate_expression(a, ae) && evaluate_expression(b, be))
        return ae.value !== be.value;
    return false;
}

function predicate_gt(a, b)
{
    var ae = {};
    var be = {};
    if (evaluate_expression(a, ae) && evaluate_expression(b, be))
        return ae.value > be.value;
    return false;
}

function predicate_lt(a, b)
{
    var ae = {};
    var be = {};
    if (evaluate_expression(a, ae) && evaluate_expression(b, be))
        return ae.value < be.value;
    return false;
}

function predicate_elt(a, b)
{
    var ae = {};
    var be = {};
    if (evaluate_expression(a, ae) && evaluate_expression(b, be))
        return ae.value <= be.value;
    return false;
}

function predicate_egt(a, b)
{
    var ae = {};
    var be = {};
    if (evaluate_expression(a, ae) && evaluate_expression(b, be))
    {
        return ae.value >= be.value;
    }
    return false;
}


function predicate_eq(a, b)
{
    var ae = {};
    var be = {};
    if (evaluate_expression(a, ae) && evaluate_expression(b, be))
        return ae.value === be.value;
    return false;
}

function predicate_term_variables(t, vt)
{    
    return unify(term_from_list(term_variables(t), NIL), vt);
}

function term_variables(z)
{
    var pdl = [z];
    var vars = [];
    while (pdl.length !== 0)
    {
        var t = deref(pdl.pop());
        if (TAG(t) === TAG_REF)
        {
            if (vars.indexOf(t) === -1)
            {
                vars.push(t);
            }
        }
        else if (TAG(t) === TAG_LST)
        {
            pdl.push(memory[VAL(t)+1]);
            pdl.push(memory[VAL(t)]);
        }
        else if (TAG(t) === TAG_STR)
        {
            var ftor = VAL(memory[VAL(t)]);
            for (var i = ftable_arity(ftor)-1; i >= 0 ; i--)
                pdl.push(memory[VAL(t) + 1 + i]);
        }
    }
    return vars;
}

function writeln(t)
{
    debug_msg("writeln(" + hex(t) +")");
    debug_msg("memory[" + VAL(t) +"] = " + hex(memory[VAL(t)]));
    stdout(format_term(t, {numbervars: true, ignore_ops: false, quoted:false}) + "\n");
    return true;
}

// noinspection JSUnusedLocalSymbols
function predicate_halt(n)
{
    state.running = false;
    return true;
}

function predicate_univ(term, list_term)
{
    var list;
    debug_msg("Univ : " + hex(term));
    debug_msg(" ...... = " + term_to_string(term));
    // noinspection JSBitwiseOperatorUsage
    if (term & M_BIT)
        abort("GC exception: M bit is still set");
    switch (TAG(term)) {
        case TAG_ATM:
        case TAG_INT:
        case TAG_FLT:
            list = term_from_list([term], NIL);
            return unify(list_term, list);
        case TAG_REF:
            // Save space on heap for ftor
            var tmp = state.H;
            state.H++;
            var arity = 0;
            if (TAG(list_term) !== TAG_LST)
                return type_error("list", list_term);

            let ftor_name_id = deref(memory[VAL(list_term)]);

            if(TAG(ftor_name_id)===TAG_ATM) {
                var ftor_name = atable[VAL(ftor_name_id)];
                list_term = memory[VAL(list_term) + 1];
                // Now write the args
                while (TAG(list_term) === TAG_LST) {
                    // Write head
                    memory[state.H++] = memory[VAL(list_term)];
                    // Update tail
                    list_term = memory[VAL(list_term) + 1];
                    arity++;
                }
                // Check tail
                if (list_term !== NIL) {
                    if (TAG(list_term) === TAG_REF)
                        return instantiation_error(list_term);
                    else
                        return type_error("list", list_term);
                }

                if (arity === 0) {
                    state.H--; // undo the setup for allocating a structure on the heap.
                    return unify(term, ftor_name_id);
                }
                memory[tmp] = lookup_functor(ftor_name, arity);
                return unify(term, tmp ^ (TAG_STR << WORD_BITS));
            } else if(TAG(ftor_name_id)===TAG_INT || TAG(ftor_name_id)===TAG_FLT) {
                return unify(term, ftor_name_id);
            } else {
                return type_error('atomic first item in univ list', ftor_name_id);
            }
        case TAG_STR:
            var ftor = VAL(memory[VAL(term)]);
            if (ftable[ftor] === undefined)
                abort("Garbage functor " + hex(ftor) + " pointed at by " + hex(term));
            list = [ftable[ftor][0] ^ (TAG_ATM << WORD_BITS)];
        for (var i = 0; i < ftable_arity(ftor); i++)
        {
            debug_msg("Arg " + i + " is at " + (VAL(term)+1+i) + " and has value " + hex(memory[VAL(term)+1+i]));
            list.push(memory[VAL(term)+1+i]);
        }
        return unify(list_term, term_from_list(list, NIL));
    case TAG_LST:
        list = [lookup_atom(".")];
        list.push(memory[VAL(term)]);
        list.push(memory[VAL(term)+1]);
        return unify(list_term, term_from_list(list, NIL));
    }
}

function predicate_functor(term, name, arity)
{
    var ftor;
    // noinspection FallThroughInSwitchStatementJS
    switch (TAG(term)) {
        case TAG_REF:
            if (TAG(name) === TAG_ATM && TAG(arity) === TAG_INT) {
                var name_string = atable[VAL(name)];
                ftor = lookup_functor(name_string, VAL(arity));
                var t = state.H ^ (TAG_STR << WORD_BITS);
                memory[state.H++] = ftor;
                for (var i = 0; i < VAL(arity); i++)
                    alloc_var();
                return unify(term, t);
            }
            else if (TAG(name) === TAG_REF)
                return instantiation_error(name);
            else if (TAG(arity) === TAG_REF)
                return instantiation_error(arity);
            else if (TAG(name) !== TAG_ATM)
                return type_error("atom", name);
            else if (TAG(arity) !== TAG_INT)
                return type_error("integer", arity);
        case TAG_ATM:
        case TAG_INT:
        case TAG_FLT:
            return unify(name, term) && unify(arity, 0 ^ (TAG_INT << WORD_BITS));
        case TAG_STR:
            ftor = VAL(memory[VAL(term)]);
            return unify(name, ftable[ftor][0] ^ (TAG_ATM << WORD_BITS)) && unify(arity, ftable_arity(ftor) ^ (TAG_INT << WORD_BITS));
        case TAG_LST:
            return unify(name, lookup_atom('.')) && unify(arity, 2 ^ (TAG_INT << WORD_BITS));
    }
}

function predicate_arg(n, t, a)
{
    if (TAG(n) === TAG_REF)
        return instantiation_error(n);
    if (TAG(t) === TAG_REF)
        return instantiation_error(t);
    if (TAG(n) !== TAG_INT)
        return type_error("integer", n);
    if (TAG(t) !== TAG_STR && TAG(t) !== TAG_LST)
        return type_error("compound", t);
    if (VAL(n) < 0)
        return domain_error("not_less_than_zero", n);

    if (TAG(t) === TAG_STR) {
        var ftor = VAL(memory[VAL(t)]);
        if (VAL(n) === 0 || VAL(n) > ftable_arity(ftor))
            return false;
        return unify(memory[VAL(t) + VAL(n)], a);
    } else if (TAG(t) === TAG_LST) {
        if (VAL(n) > 2)
            return false;
        return unify(memory[VAL(t) + VAL(n) - 1], a);
    }
}

function predicate_var(v)
{
    return TAG(v) === TAG_REF;
}

function predicate_atom(v)
{
    return TAG(v) === TAG_ATM;
}

function predicate_integer(v)
{
    return TAG(v) === TAG_INT;
}

function predicate_float(v)
{
    return TAG(v) === TAG_FLT;
}

function predicate_compound(v)
{
    return TAG(v) === TAG_STR || TAG(v) === TAG_LST;
}

function predicate_ground(x)
{
    var args = [x];
    while(args.length > 0)
    {
        var arg = args.pop();
        switch (TAG(arg))
        {
        case TAG_REF:
            return false;
        case TAG_INT:
        case TAG_FLT:
        case TAG_ATM:
            return true;
        case TAG_LST:
            args.push(memory[VAL(arg)]);
            args.push(memory[VAL(arg)+1]);
            continue;
        case TAG_STR:
            var ftor = VAL(memory[VAL(arg)]);
            for (var i = 0; i < ftable_arity(ftor); i++)
                args.push(memory[VAL(arg)+1+i]);

        }
    }
}

function predicate_unify(a, b)
{
    return unify(a,b);
}

function predicate_match(a, b)
{
    var match_pdl = [];
    match_pdl.push(a);
    match_pdl.push(b);
    while (match_pdl.length !== 0)
    {
        var d1 = deref(match_pdl.pop());
        var d2 = deref(match_pdl.pop());
        if (d1 !== d2)
        {
            var type1 = TAG(d1);
            var val1 = VAL(d1);
            var type2 = TAG(d2);
            var val2 = VAL(d2);
            // If either is a variable or atomic then they must be equal in order to match. They are not equal if we get to this line, so bail.
            if (type1 === TAG_REF || type2 === TAG_REF || type2 === TAG_ATM || type2 === TAG_INT || type2 === TAG_FLT)
                return false;

            if (type1 !== type2) // Types must be equal for matching
                return false;

            if (type1 === TAG_LST)
            {                        
                match_pdl.push(memory[val1]); // unify heads
                match_pdl.push(memory[val2]);
                match_pdl.push(memory[val1+1]); // unify tails
                match_pdl.push(memory[val2+1]);
            }
            else if (type1 === TAG_STR)
            {
                var f1 = VAL(memory[val1]);
                var f2 = VAL(memory[val2]);
                if (f1 === f2)
                {
                    for (var i = 0; i < ftable[f1][1]; i++)
                    {
                        match_pdl.push(val1 + 1 + i);
                        match_pdl.push(val2 + 1 + i);
                    }
                }
                else
                    return false;
            }
            else
                abort("Illegal tag");
        }
    }
    return true;
}

// gets the i-th arg of a term. First arg is index=1, not index=0.
function get_arg(term, index)
{
    return deref(memory[VAL(term) + index]);
}

function lookup_atom(name)
{
    if(typeof name !== 'string') {
        throw 'invalid lookup_atom. name must have type of string, but is ' + typeof name + '. name = ' + name;
    }

    var i;
    for (i = 0; i < atable.length; i++)
    {
        if (atable[i] === name)
            return i ^ (TAG_ATM << WORD_BITS);
    }
    i = atable.length;
    atable[i] = name;
    return i ^ (TAG_ATM << WORD_BITS);
}

function lookup_functor(name, arity) 
{
    var a = VAL(lookup_atom(name));
    var i;
    for (i = 0; i < ftable.length; i++)
        if (ftable[i][0] === a && ftable[i][1] === arity)
            return i ^ (TAG_ATM << WORD_BITS);
    i = ftable.length;
    ftable[i] = [a, arity];
    return i ^ (TAG_ATM << WORD_BITS);    
}

function lookup_dynamic_functor(name, arity) {
    let a = lookup_functor(name, arity);

    let already_dynamic = false;
    for (let i = 0; i < dtable.length; i++) {
        if (dtable[i] === a) {
            already_dynamic = true;
            break;
        }
    }

    if(! already_dynamic) {
        dtable.push(a);
    }
    return a;
}

function emit_code(address, c)
{
    let value;

    if(TAG(c) === TAG_INT) {
        value = VAL(c);
    } else if(TAG(c) === TAG_STR) {
        // c = extended_address(BaseC)
        // value = VAL(BaseC) ^ 0x80000000
        let functor = VAL(memory[VAL(c)]);
        let functorName = atable[ftable[functor][0]];
        if(functorName === 'extended_address' &&
        ftable[functor][1] === 1) {
            let arg = memory[VAL(c)+1];
            if(TAG(arg) === TAG_INT) {
                let argValue = VAL(arg);
                value = argValue ^ 0x80000000;
            } else {
                return type_error('integer', arg);
            }
        } else {
            return type_error('extended_address/1 structure', c);
        }
    }

    compilation_environment.buffer[VAL(address)] = value;
    return true;
}

function predicate_lookup_atom(atom, index)
{
    if(TAG(index) !== TAG_REF) {
        if(TAG(index) !== TAG_INT) {
            return type_error('integer', index);
        }
        let indexJS = PL_get_integer(index);
        if(indexJS < 0 || indexJS >= atable.length) {
            return domain_error("atom ID > 0 and < " + atable.length, index);
        }
        return unify(atom, PL_put_atom(indexJS));
    }

    if(TAG(atom) === TAG_REF) {
        return instantiation_error('atom or index ground', atom);
    }

    if(TAG(atom) !== TAG_ATM) {
        return type_error('atom', atom);
    }

    return unify(VAL(atom) ^ (TAG_INT << WORD_BITS), index);
}

function predicate_lookup_float(f, index)
{
    return unify(VAL(f) ^ (TAG_INT << WORD_BITS), index);
}

function predicate_lookup_functor(fname, arity, index)
{
    if(TAG(index) === TAG_REF) {
        if(TAG(fname) === TAG_REF) {
            return instantiation_error('atom', fname);
        }

        if(TAG(fname) !== TAG_ATM) {
            return type_error('atom', fname);
        }

        if(TAG(arity) === TAG_REF) {
            return instantiation_error('integer', arity);
        }

        if(TAG(arity) !== TAG_INT) {
            return type_error('integer', arity);
        }

        if (atable[VAL(fname)] === undefined)
            abort("Atom out of range: " + hex(deref(fname)));
        var i;
        for (i = 0; i < ftable.length; i++) {
            if (ftable[i][0] === VAL(fname) && ftable[i][1] === VAL(arity)) {
                return unify(index, i ^ (TAG_INT << WORD_BITS));
            }
        }
        i = ftable.length;
        ftable[i] = [VAL(fname), VAL(arity)];
        return unify(index, i ^ (TAG_INT << WORD_BITS));
    } else {
        if(TAG(index) !== TAG_INT) {
            return type_error('integer', index);
        }

        let indexJS = PL_get_integer(index);
        let pair = ftable[indexJS];
        if(typeof pair === 'undefined') {
            return domain_error('functor identifier', index);
        }

        let nameID = pair[0];
        let lookupFName = PL_put_atom(nameID);

        let arityJS = pair[1];
        let lookupArity = PL_put_integer(arityJS);

        return unify(fname, lookupFName) && unify(arity, lookupArity);
    }
}

function predicate_generate_system_goal(Module, Sys) {
    if (TAG(Module) !== TAG_ATM) {
        return type_error('atom', Module);
    }
    if (TAG(Sys) !== TAG_REF) {
        return instantiation_error(Sys);
    }
    let n = system.length;
    let moduleJS = PL_get_atom_chars(Module);
    let functor = lookup_functor(moduleJS + ':$sys_' + n, 0);
    system.push(VAL(functor));
    let nameID = ftable[VAL(functor)][0];
    let namePL = PL_put_atom(nameID);
    return unify(namePL, Sys);
}

function predicate_generate_initialization_goal(Module, Init) {
    if (TAG(Module) !== TAG_ATM) {
        return type_error('atom', Module);
    }
    if (TAG(Init) !== TAG_REF) {
        return instantiation_error(Init);
    }
    let n = initialization.length;
    let moduleJS = PL_get_atom_chars(Module);
    let functor = lookup_functor(moduleJS + ':$init_' + n, 0);
    initialization.push(VAL(functor));
    let nameID = ftable[VAL(functor)][0];
    let namePL = PL_put_atom(nameID);
    return unify(namePL, Init);
 }

// dynamic implies public. In proscript, public also implies dynamic.
// the is_public flag will be set to true in the saved state
// for predicate Name/Arity.


function predicate_define_dynamic_predicate(indicator) {
    let colon = lookup_functor(":", 2);
    var slash2 = lookup_functor("/", 2);
    if (TAG(indicator) === TAG_STR && (memory[VAL(indicator)] === colon || memory[VAL(indicator)] === slash2)) {
        let name;
        let arity;

        if (memory[VAL(indicator)] === colon) {
            // :(ModuleName, /(Functor, Arity))
            let moduleName = deref(memory[VAL(indicator) + 1]);
            let slashStructure = deref(memory[VAL(indicator) + 2]);
            if (TAG(slashStructure) !== TAG_STR) {
                return type_error('predicate_indicator', slashStructure);
            }
            name = deref(memory[VAL(slashStructure) + 1]);
            arity = deref(memory[VAL(slashStructure) + 2]);

            if (TAG(moduleName) === TAG_REF) {
                return instantiation_error(moduleName);
            }

            let moduleNameJS = PL_get_atom_chars(moduleName);
            if (TAG(name) === TAG_ATM) {
                let nameJS = PL_get_atom_chars(name);
                if (!nameJS.includes(":")) {
                    name = PL_put_atom_chars(moduleNameJS + ':' + nameJS);
                } else {
                    let prefix = nameJS.substring(0, nameJS,indexOf(":"));
                    if(!plausibleModuleName(prefix)) {
                        name = PL_put_atom_chars(moduleNameJS + ':' + nameJS);
                    }
                }
            }
        } else {
            name = deref(memory[VAL(indicator) + 1]);
            arity = deref(memory[VAL(indicator) + 2]);
        }
        if (TAG(name) === TAG_ATM && TAG(arity) === TAG_INT) {
            if (VAL(arity) < 0)
                return domain_error("not_less_than_zero", arity);
            var ftor = VAL(lookup_dynamic_functor(atable[VAL(name)], VAL(arity)));
            if (!predicates[ftor]) {
                predicates[ftor] = {
                    clauses: {},
                    key: ftor,
                    clause_keys: [],
                    is_public: true,
                    next_key: 0
                };
            } else if (!predicates[ftor].is_public)
                return permission_error("modify", "static_procedure", indicator);
            return true;
        } else if (TAG(name) === TAG_REF)
            return instantiation_error(name);
        else if (TAG(name) !== TAG_ATM)
            return type_error("atom", name);
        else if (TAG(arity) === TAG_REF)
            return instantiation_error(arity);
        else if (TAG(arity) !== TAG_INT)
            return type_error("integer", arity);
    }
    else if (TAG(indicator) === TAG_REF)
        return instantiation_error(indicator);
    else
        return type_error("predicate_indicator", indicator);
}

function plausibleModuleName(name) {
    let first = name.substring(0,1);
    if(first !== first.toUpperCase() && first === first.toLowerCase()) {
        return name.match(/\w+/);
    } else {
        return false;
    }
}
/**
 The predicateIndicator is a structure of Name/Arity or op(Precedence, Associativity, Operator).
 In the first case, Name is the unqualified name of a predicate (i.e. does not have a module
 name prefix).
 The second case is currently ignored - operators are global in ProcscriptLS.
 *
 * @param moduleName
 * @param predicateIndicator
 * @returns {boolean}
 */

function predicate_add_module_export(moduleName, predicateIndicator) {
  //     assertz('$module_export'(Name, F, A)).
    if (TAG(moduleName) === TAG_REF)
        return instantiation_error(moduleName);
    else if (TAG(moduleName) !== TAG_ATM)
        return type_error("atom", moduleName);

    var slash2 = lookup_functor("/", 2);
    var op3 = lookup_functor("op", 3);
    if (TAG(predicateIndicator) === TAG_STR && memory[VAL(predicateIndicator)] === slash2)
    {
        var name = deref(memory[VAL(predicateIndicator) + 1]);
        var arity = deref(memory[VAL(predicateIndicator) + 2]);
        if (TAG(name) === TAG_ATM && TAG(arity) === TAG_INT)
        {
            if (VAL(arity) < 0)
                return domain_error("not_less_than_zero", arity);

            let exportedPredicates = module_exports[VAL(moduleName)];
            if(exportedPredicates) {
                for (let entry of exportedPredicates) {
                    if (entry[0] === VAL(name) && entry[1] === VAL(arity)) {
                        return true; // do nothing, the predicate is already specified in the exports.
                    }
                }
            } else {
                exportedPredicates = [];
                module_exports[VAL(moduleName)] = exportedPredicates;
            }

            // add namePL/arity to exports.
            exportedPredicates.push([VAL(name), VAL(arity)]);
            return true;
        }
        else if (TAG(name) === TAG_REF)
            return instantiation_error(name);
        else if (TAG(name) !== TAG_ATM)
            return type_error("atom", name);
        else if (TAG(arity) === TAG_REF)
            return instantiation_error(arity);
        else if (TAG(arity) !== TAG_INT)
            return type_error("integer", arity);
    }
    else if (TAG(predicateIndicator) === TAG_STR && memory[VAL(predicateIndicator)] === op3)
    {
        return true;
    }
    else if (TAG(predicateIndicator) === TAG_REF)
        return instantiation_error(predicateIndicator);
    else
        return type_error("predicate_indicator or op", predicateIndicator);
}


function predicate_module_export(moduleName, predicateIndicator)
{
    if (TAG(moduleName) === TAG_REF)
    {
        let cursor;

        if (state.foreign_retry)
        {
            cursor = state.foreign_value;
            cursor.exportIndex++;
        }
        else
        {
            create_choicepoint();
            cursor = {module:0, exportIndex:0};
        }

        if (cursor.module >= module_exports.length)
        {
            destroy_choicepoint();
            return false;
        } else if(cursor.exportIndex >= module_exports[cursor.module].length) {
            cursor.module++;
            cursor.exportIndex = 0;
        }

        update_choicepoint_data(cursor);

        let moduleExports = module_exports[cursor.module];
        while(!moduleExports && cursor.module < module_exports.length) {
            cursor.module++;
            moduleExports = module_exports[cursor.module];
        }

        if(!moduleExports) {
            destroy_choicepoint();
            return false;
        } else if( unify(moduleName, PL_put_atom(cursor.module))) {
            let pair = moduleExports[cursor.exportIndex];
            if(!pair) {
                return false;
            } else {
                let indicatorPL = create_indicator_structure(PL_put_atom(pair[0]), PL_put_integer(pair[1]));
                return unify(predicateIndicator, indicatorPL);
            }
        } else {
            return false;
        }
    }
    else if (TAG(moduleName) === TAG_ATM)
    {

        let cursor;

        if (state.foreign_retry)
        {
            cursor = state.foreign_value;
            cursor.exportIndex++;
        }
        else if(module_exports[VAL(moduleName)] && module_exports[VAL(moduleName)].length > 0)
        {
            create_choicepoint();
            cursor = {module:VAL(moduleName), exportIndex:0};
        }
        else
        {
            return false;
        }

        if(cursor.exportIndex >= module_exports[cursor.module].length) {
            destroy_choicepoint();
            return false;
        }

        update_choicepoint_data(cursor);
        let moduleExports = module_exports[cursor.module];

        if(!moduleExports) {
            destroy_choicepoint();
            return false;
        } else if( unify(moduleName, PL_put_atom(cursor.module))) {
            let pair = moduleExports[cursor.exportIndex];
            if(!pair) {
                return false;
            } else {
                let indicatorPL = create_indicator_structure(PL_put_atom(pair[0]), PL_put_integer(pair[1]));
                return unify(predicateIndicator, indicatorPL);
            }
        } else {
            return false;
        }
    }
    else
        return type_error("atom", key);
}

function predicate_add_module_import(importingModule, importedModule) {
    //     assertz('$import'(importingModule, importedModule)).
    if (TAG(importingModule) === TAG_REF)
        return instantiation_error(importingModule);
    else if (TAG(importingModule) !== TAG_ATM)
        return type_error("atom", importingModule);

    if (TAG(importedModule) === TAG_REF)
        return instantiation_error(importedModule);
    else if (TAG(importedModule) !== TAG_ATM)
        return type_error("atom", importedModule);

    let selectedImportedModules = module_imports[VAL(importingModule)];
    if(selectedImportedModules) {
        selectedImportedModules.push(VAL(importedModule));
    } else {
        module_imports[VAL(importingModule)] = [VAL(importedModule)];
    }

    return true;
}

function predicate_module_import(importingModule, importedModule)
{
    if (TAG(importingModule) === TAG_REF)
    {
        let cursor;

        if (state.foreign_retry)
        {
            cursor = state.foreign_value;
            cursor.importIndex++;
        }
        else
        {
            create_choicepoint();
            cursor = {module:0, importIndex:0};
        }

        if (cursor.module >= module_imports.length)
        {
            destroy_choicepoint();
            return false;
        } else if(!module_imports[cursor.module] || cursor.importIndex >= module_imports[cursor.module].length) {
            cursor.module++;
            cursor.importIndex = 0;
        }

        update_choicepoint_data(cursor);

        let moduleImports = module_imports[cursor.module];
        while(cursor.module < module_imports.length
            && (!moduleImports || cursor.importIndex >= moduleImports.length)) {
            cursor.module++;
            cursor.importIndex = 0;
            moduleImports = module_imports[cursor.module];
        }

        if(!moduleImports || cursor.importIndex >= moduleImports.length) {
            destroy_choicepoint();
            return false;
        } else if( unify(importingModule, PL_put_atom(cursor.module))) {
            let importedModuleID = moduleImports[cursor.importIndex];
            if(!importedModuleID) {
                return false;
            } else {
                return unify(importedModule, PL_put_atom(importedModuleID));
            }
        } else {
            return false;
        }
    }
    else if (TAG(importingModule) === TAG_ATM)
    {

        let cursor;

        if (state.foreign_retry)
        {
            cursor = state.foreign_value;
            cursor.importIndex++;
        }
        else if(module_imports[VAL(importingModule)] && module_imports[VAL(importingModule)].length > 0)
        {
            create_choicepoint();
            cursor = {module:VAL(importingModule), importIndex:0};
        } else {
            return false;
        }

        if(cursor.importIndex >= module_imports[cursor.module].length) {
            destroy_choicepoint();
            return false;
        }

        update_choicepoint_data(cursor);
        let moduleImports = module_imports[cursor.module];

        if(!moduleImports) {
            destroy_choicepoint();
            return false;
        } else if( unify(importingModule, PL_put_atom(cursor.module))) {
            let importedModuleID = moduleImports[cursor.importIndex];
            if(!importedModuleID) {
                return false;
            } else {
                return unify(importedModule, PL_put_atom(importedModuleID));
            }
        } else {
            return false;
        }
    }
    else
        return type_error("atom", importingModule);
}

/**
 meta predicates: FunctorID : {Arity1: [ArgTypeID1, ArtTypeID2, ...], Arity2: [ArgTypes2], ...}
 ArgType is 0..9, :, ^, +, -, ?
 *
 * @param functor
 * @param arity
 * @param argTypes
 * @returns {boolean}
 */

function predicate_add_meta_predicate(functor, arity, argTypes) {
    if (TAG(functor) === TAG_REF)
        return instantiation_error(functor);
    else if (TAG(functor) !== TAG_ATM)
        return type_error("atom", functor);

    if (TAG(arity) === TAG_REF)
        return instantiation_error(arity);
    else if (TAG(arity) !== TAG_INT)
        return type_error("integer", arity);

    if (TAG(argTypes) === TAG_REF)
        return instantiation_error(argTypes);
    else if (TAG(argTypes) !== TAG_LST)
        return type_error("list", argTypes);

    let functorID = VAL(functor);
    let argTypeArray = type_list_to_id_array(argTypes);
    let arities = meta_predicate_signatures ? meta_predicate_signatures[functorID] : undefined;
    if(arities) {
        let aritySignature = arities[VAL(arity)];
        if(aritySignature) {
            return existence_error('unique arity for ' + PL_get_atom_chars(functor), arity);
        }
        arities[VAL(arity)] = argTypeArray;
    } else {
        arities = {};
        arities[VAL(arity)] = argTypeArray;
        if(!meta_predicate_signatures) {
            meta_predicate_signatures = {};
        }
        meta_predicate_signatures[functorID] = arities;
    }
    return true;
}

/**
 * predicate_pls_meta_predicate is the ProscriptLS implementation of meta_predicate.
 * This is named pls_meta_predicate to avoid a name collision with SWI-PL meta_predicate
 * during bootstrap compilation.
 *
 * @param functor
 * @param arity
 * @param argTypes
 * @returns {boolean}
 */
function predicate_pls_meta_predicate(functor, arity, argTypes) {
    if(TAG(functor) !== TAG_REF && TAG(functor) !== TAG_ATM) {
        return type_error('atom', functor);
    }

    if(TAG(arity) !== TAG_REF && TAG(arity) !== TAG_INT) {
        return type_error('integer', arity);
    }

    if(TAG(argTypes) !== TAG_REF && TAG(argTypes) !== TAG_LST) {
        return type_error('list', argTypes);
    }

    // case: all args are bound, argTypes may contain TAG_REF (variable) terms.
    if(TAG(functor) === TAG_ATM && TAG(arity) === TAG_INT) {
        let functorID = VAL(functor);
        let arityInt = VAL(arity);
        let arities = meta_predicate_signatures ? meta_predicate_signatures[functorID] : undefined;
        if (arities) {
            if (TAG(argTypes) === TAG_REF) {
                let aritySignature = arities[arityInt];
                if (aritySignature) {
                    let argTypesPL = terms_to_list(convert_arg_types(aritySignature));
                    return unify(argTypes, argTypesPL);
                } else {
                    return false;
                }
            } else if (TAG(argTypes) === TAG_LST) {
                let argTypeArray = atom_or_var_list_to_term_array(argTypes);
                let aritySignature = arities[arityInt];
                if (aritySignature) {
                    for (let ofst = 0; ofst < arityInt; ofst++) {
                        let inputType = argTypeArray[ofst];
                        let storedTypePL = convert_arg_type(aritySignature[ofst]);
                        if (!unify(inputType, storedTypePL)) {
                            return false;
                        }
                    }
                    return true;
                } else {
                    return false;
                }
            } else {
                return type_error('list', argTypes);
            }
         } else {
            return false;
        }
    }

    let cursor;

    if (state.foreign_retry)
    {
        cursor = state.foreign_value;
        if(cursor.arities && cursor.arities.length > 1) {
            cursor.arities = cursor.arities.slice(1);
        } else {
            if(cursor.functors && cursor.functors.length > 1) {
                cursor.functors = cursor.functors.slice(1);
                cursor.arities = meta_predicate_signatures(cursor.functors[0]);
                if(! cursor.arities) {
                    return false;
                }
            } else {
                destroy_choicepoint();
                return false;
            }
        }
    }
    else
    {
        create_choicepoint();

        let functorIDs;
        let arityIntArray;

        if(TAG(functor) === TAG_ATM) {
            functorIDs = [VAL(functor)];
            arityIntArray = meta_predicate_signatures[VAL(functor)];
        } else {
            let functorIDs = [];
            for(let ofst = 0;ofst < meta_predicate_signatures.length; ofst++) {
                let arities = meta_predicate_signatures[ofst];
                if(arities && (TAG(arity) === TAG_REF || arities[VAL(arity)])) {
                    functorIDs.push(functorID);
                }
            }

            if(TAG(arity) === TAG_REF) {
                arityIntArray = meta_predicate_signatures[functorIDs[0]];
            } else {
                arityIntArray = VAL(arity);
            }
        }

        cursor = {functors:functorIDs, arities:arityIntArray};
    }

    update_choicepoint_data(cursor);

    // cursor = {functors: functorIDArray, arities: arityIntArray}
    // current choice is functorIDArray[0] and arityIntArray[0].
    // arityIntArray is function of functorIDArray[0]
    // argTypesArray is function of functorIDArray[0] and arityIntArray[0].

    let currentFunctorID = cursor.functors[0];
    let currentArityInt = cursor.arities[0];
    let functorSignatures = meta_predicate_signatures[currentFunctorID];
    if(functorSignatures) {
        let currentSignature = functorSignatures[currentArityInt];
        if(currentSignature) {
            if (TAG(argTypes) === TAG_REF) {
                let argTypesPL = integers_to_list(convert_arg_types(currentSignature));
                return unify(argTypes, argTypesPL);
            } else if (TAG(argTypes) === TAG_LST) {
                let argTypeArray = atom_or_var_list_to_term_array(argTypes);
                for (let ofst = 0; ofst < arityInt; ofst++) {
                    let inputType = argTypeArray[ofst];
                    let storedType = currentSignature[ofst];
                    if (!unify(inputType, PL_put_atom(storedType))) {
                        return false;
                    }
                }
                return true;
            } else {
                return type_error('list', argTypes);
            }
        } else {
            return engine_error('Stored arity ' + currentArityInt + ' does not have a stored signature.');
        }
    } else {
        return engine_error('Stored functor identifier ' + currentFunctorID + ' does not have a stored signature.');
    }
}

function convert_arg_types(storedTypes) {
    let results = [];
    for(let ofst = 0;ofst < storedTypes.length;ofst++) {
        let storedType = storedTypes[ofst];
        results[ofst] = convert_arg_type(storedType);
    }
    return results;
}

function convert_arg_type(storedType) {
    let storedTypePL = PL_put_atom(storedType);
    let storedAtom = PL_get_atom_chars(storedTypePL);
    let resultType;
    if(storedAtom.startsWith('t')) {
        let storedNumberString = storedAtom.substring(1);
        let storedNumber = Number.parseInt(storedNumberString);
        resultType = PL_put_integer(storedNumber);
    } else {
        resultType = storedTypePL;
    }
    return resultType;
}

function type_list_to_id_array(listPL) {

    let result = [];
    var head = memory[VAL(listPL)];
    var tail = memory[VAL(listPL)+1];
    while (true)
    {
        if(TAG(head) !== TAG_ATM &&  TAG(head) !== TAG_LST) {
            throw('Invalid type list. Item is neither an atom nor an integer.');
        }

        let termID;
        if(TAG(head) === TAG_ATM) {
            termID = VAL(head);
        } else {
            let taggedNumber = 't' + PL_get_atom_chars(head);
            termID = VAL(lookup_atom(taggedNumber));
        }

        result.push(termID);

        if (tail === NIL)
            return result;
        else if (TAG(tail) === TAG_LST)
        {
            head = memory[VAL(tail)];
            tail = memory[VAL(tail)+1];
        }
        else
            throw('Invalid atom list. Last item was not NIL.');
    }
}

function atom_or_var_list_to_term_array(listPL) {

    let result = [];
    var head = memory[VAL(listPL)];
    var tail = memory[VAL(listPL)+1];
    while (true)
    {
        if(TAG(head) !== TAG_ATM && TAG(head) !== TAG_REF) {
            throw('Invalid atom-or-var list. Item is not an atom and not a variable.');
        }

        result.push(head);

        if (tail === NIL)
            return result;
        else if (TAG(tail) === TAG_LST)
        {
            head = memory[VAL(tail)];
            tail = memory[VAL(tail)+1];
        }
        else
            throw('Invalid atom-or-var list. Last item was not NIL.');
    }
}


function integer_list_to_term_array(listPL) {

    let result = [];
    var head = memory[VAL(listPL)];
    var tail = memory[VAL(listPL)+1];
    while (true)
    {
        if(TAG(head) !== TAG_INT) {
            throw('Invalid integer list. Item is not an integer.');
        }

        result.push(VAL(head));

        if (tail === NIL)
            return result;
        else if (TAG(tail) === TAG_LST)
        {
            head = memory[VAL(tail)];
            tail = memory[VAL(tail)+1];
        }
        else
            throw('Invalid integer list. Last item was not NIL.');
    }
}

function integer_list_list_to_term_array(listPL) {

    let result = [];
    var head = memory[VAL(listPL)];
    var tail = memory[VAL(listPL)+1];
    while (true)
    {
        if(TAG(head) !== TAG_LST) {
            throw('Invalid integer list list. Item is not a list.');
        }

        result.push(integer_list_to_term_array(head));

        if (tail === NIL)
            return result;
        else if (TAG(tail) === TAG_LST)
        {
            head = memory[VAL(tail)];
            tail = memory[VAL(tail)+1];
        }
        else
            throw('Invalid integer list list. Last item was not NIL.');
    }
}

function create_indicator_structure(name, arity) {
    var ftor = lookup_functor('/', 2);
    var structure = alloc_structure(ftor);
    memory[state.H++] = name;
    memory[state.H++] = arity;
    return structure;
}


function predicate_dump_tables(streamPL) {
    let streamContainer = {};
    if (!get_stream(streamPL, streamContainer))
        return false;
    let streamValue = streamContainer.value;
    write_to_stream(streamValue, 'atable =' + JSON.stringify(atable) + ';\n');
    write_to_stream(streamValue, 'floats =' + JSON.stringify(floats) + ';\n');
    write_to_stream(streamValue, 'ftable =' + JSON.stringify(ftable) + ';\n');
    write_to_stream(streamValue, 'dtable =' + JSON.stringify(dtable) + ';\n');
    write_to_stream(streamValue, 'predicates =' + JSON.stringify(predicates) + ';\n');
    write_to_stream(streamValue, 'indexed_predicates =' + JSON.stringify(indexed_predicates) + ';\n');
    write_to_stream(streamValue, 'foreign_predicates ={' );
    let isFirst = true;
    for(let predKey of Object.keys(foreign_predicates)) {
        let predFunc = foreign_predicates[predKey];
        if(! isFirst) {
            write_to_stream(streamValue, ', ')
        } else {
            isFirst = false;
        }
        //0: predicate_acyclic_term
        write_to_stream(streamValue, predKey + ': ' + predFunc.name);
    }
    write_to_stream(streamValue, '};\n' );

    write_to_stream(streamValue, 'system =' + JSON.stringify(system) + ';\n');
    write_to_stream(streamValue, 'initialization =' + JSON.stringify(initialization) + ';\n');
    write_to_stream(streamValue, 'module_exports =' + JSON.stringify(module_exports) + ';\n');
    write_to_stream(streamValue, 'module_imports =' + JSON.stringify(module_imports) + ';\n');
    write_to_stream(streamValue, 'meta_predicate_signatures =' + JSON.stringify(meta_predicate_signatures) + ';\n');

    return true;
}

function write_to_stream(streamValue, string) {
    let bytes = toByteArray(string);
    return streamValue.write(streamValue, 1, bytes.length, bytes);
}
function predicate_trace_unify(a, b)
{
    stdout("tracing unification of " + hex(a) + " and " + hex(b) + "\n");
    stdout("before, LHS = " + term_to_string(a) + "\n");
    stdout("before, RHS = " + term_to_string(b) + "\n");
    if (unify(a,b))
    {
        stdout("after, LHS = " + term_to_string(a) + "\n");
        stdout("after, RHS = " + term_to_string(b) + "\n");
        return true;
    }
    stdout("Failed to unify\n");
    return false;
}

function predicate_op(precedence, fixity, name)
{
    var op_name;
    var names;
    if (TAG(fixity) === TAG_REF)
        return instantiation_error(fixity);
    if (TAG(precedence) === TAG_REF)
        return instantiation_error(precedence);
    if (TAG(precedence) !== TAG_INT)
        return type_error("integer", precedence);

    var op_precedence = VAL(precedence);
    var fixityJS = atable[VAL(fixity)];
    if (op_precedence < 0 || op_precedence > 1200)
        return domain_error("operator_priority", precedence);

    if (TAG(name) === TAG_ATM) {
        op_name = atable[VAL(name)];
        if (op_name === ",")
            return permission_error("modify", "operator", name);
        else if (op_name === "|" && op_precedence < 1001)
            return permission_error("modify", "operator", name);
        names = [op_name];
    }
    else if (TAG(name) === TAG_LST)
    {
        names = [];
        var head = name;
        while (TAG(head) === TAG_LST)
        {
            if (TAG(deref(memory[VAL(head)])) === TAG_ATM)
            {
                op_name = atable[deref(memory[VAL(head)])];
                if (op_name === ",")
                    return permission_error("modify", "operator", name);
                else if (op_name === "|" && op_precedence < 1001)
                    return permission_error("modify", "operator", name);
                names.push(op_name);
            }
            else
                return type_error("atom", head);
        }
        if (head !== NIL)
        {
            if (TAG(head) === TAG_REF)
                return instantiation_error(head);
            else
                return type_error("atom", head);
        }
    }
    else
        return type_error("list", name);

    for (var i = 0; i < names.length; i++)
    {
        op_name = names[i];

        if (fixityJS === "fx" || fixityJS === "fy")
        {
            if (op_precedence === 0)
                prefix_operators[op_name] = undefined;
            else
                prefix_operators[op_name] = {precedence: op_precedence, fixity:fixityJS};
        }
        else if (fixityJS === "xf" || fixityJS === "yf")
        {
            if (op_precedence === 0)
                postfix_operators[op_name] = undefined;
            else
                postfix_operators[op_name] = {precedence: op_precedence, fixity:fixityJS};
        }
        else
        {
            if (op_precedence === 0)
                infix_operators[op_name] = undefined;
            else
                infix_operators[op_name] = {precedence: op_precedence, fixity:fixityJS};
        }
    }
    return true;
}

var gensyms = {};

function predicate_gensym(root, sym)
{
    if (gensyms[root] === undefined)
        gensyms[root] = 0;
    return unify(lookup_atom(atable[VAL(root)] + gensyms[root]), sym);
}

function prepend_clause_to_predicate(predicate, head, body)
{
    var predicateJS = VAL(lookup_functor(atable[VAL(deref(memory[VAL(predicate)+1]))], VAL(deref(memory[VAL(predicate)+2]))));
    if (predicates[predicateJS] === undefined || (predicates[predicateJS].is_public && predicates[predicateJS].clause_keys.length === 0))
    {
        // Easy case. New predicate or empty predicate. Add it to the table then set up the <NOP,0> header
        compilation_environment.buffer[0] = 254;
        compilation_environment.buffer[1] = 0;
        predicates[predicateJS] = {clauses: {0:{code:compilation_environment.buffer.slice(0),
                                              key:0, 
                                              head:record_term(head), 
                                              body:record_term(body)}},
                                 clause_keys: [0],
                                 key:predicateJS,
                                 is_public: true,
                                 next_key: 1};
    }
    else
    {
        var first_key = predicates[predicateJS].clause_keys[0];
        var first_clause = predicates[predicateJS].clauses[first_key];
        if (first_clause.code[0] === 254)
        {
            // First clause was NOP - ie only clause. Make it trust_me, and the new clause is try_me_else
            compilation_environment.buffer[0] = 28;
            compilation_environment.buffer[1] = first_key;
            first_clause.code[0] = 30;
            first_clause.code[1] = 0;
            predicates[predicateJS].clauses[predicates[predicateJS].next_key] = {code:compilation_environment.buffer.slice(0),
                                                                             key: predicates[predicateJS].next_key,
                                                                             head:record_term(head), 
                                                                             body:record_term(body)};
            predicates[predicateJS].clause_keys.unshift(predicates[predicateJS].next_key);
            predicates[predicateJS].next_key++;
            
        }
        else if (first_clause.code[0] === 28)
        {
            // first clause was try_me_else. It becomes retry_me_else
            // Our new clause is try_me_else
            compilation_environment.buffer[0] = 28;
            compilation_environment.buffer[1] = first_key;
            first_clause.code[0] = 29;
            predicates[predicateJS].clauses[predicates[predicateJS].next_key] = {code:compilation_environment.buffer.slice(0),
                                                                             key: predicates[predicateJS].next_key,
                                                                             head:record_term(head), 
                                                                             body:record_term(body)};
            predicates[predicateJS].clause_keys.unshift(predicates[predicateJS].next_key);
            predicates[predicateJS].next_key++;
        }
        else
            abort("Garbage clauses in prepend: " + first_clause.code[0]);
    }
    return true;
}

function check_compile_buffer(head, body)
{
    // Paranoia
    for (var z = 0; z < compilation_environment.buffer.length; z++)
    {
        if (compilation_environment.buffer[z] === null)
        {
            debug(term_to_string(head) + ":- " + term_to_string(body));
            debug(JSON.stringify(compilation_environment.buffer));
            abort("Illegal compile buffer: Address " + z + " is null!");
        }
    }
}

function predicate_indexable_compiled_predicates(result) {
    let indexables = state.H ^ (TAG_LST << WORD_BITS);
    let count = 0;
    for(let predicateID of Object.keys(predicates)) {
        let predicate = predicates[predicateID];
        if(! indexed_predicates[predicate.key] && ftable[predicate.key][1] > 0 && ! predicate.is_public
        && typeof predicate.clause_keys[0] !== "undefined" &&
            typeof predicate.clauses[predicate.clause_keys[0]] !== "undefined" &&
            typeof predicate.clauses[predicate.clause_keys[0]].head !== "undefined") {
            count++;
            // add to indexables
            memory[state.H] = PL_put_integer(predicate.key);
            // If there are no more items we will overwrite the last entry with [] when we exit the loop
            memory[state.H+1] = ((state.H+2) ^ (TAG_LST << WORD_BITS));
            state.H += 2;
        }
    }
    
    if(count > 0) {
        memory[state.H - 1] = NIL;
    } else {
        indexables = NIL;
    }

    return unify(result,indexables );
}

function predicate_register_indexed_predicate(predicateP) {
    indexed_predicates[VAL(predicateP)] = true;
    return true;
}

function predicate_compiled_clauses(predicateP, clauseInfos) {
    // create list of ClauseInfo = ClauseOffset - (Head/ClauseCodes)
    let predicate = predicates[VAL(predicateP)];
    if(! predicate) {
        return existence_error('predicate', predicateP);
    }

    let terms = [];

    for(let ofst = 0;ofst < predicate.clause_keys.length;ofst++) {
        let clause = predicate.clauses[predicate.clause_keys[ofst]];
        let head = recall_term(clause.head, {});
        let clauseCodes = integers_to_list(clause.code);
        let slashFtor = lookup_functor('/', 2);
        let headCodeInfo = alloc_structure(slashFtor);
        memory[state.H++] = head;
        memory[state.H++] = clauseCodes;

        let dashFtor = lookup_functor('-', 2);
        let clauseInfo = alloc_structure(dashFtor);
        memory[state.H++] = PL_put_integer(ofst);
        memory[state.H++] = headCodeInfo;

        terms.push(clauseInfo);
    }

    let clauseInfoList = terms_to_list(terms);
    return unify(clauseInfos, clauseInfoList);
}

function predicate_add_index_clause_to_predicate(predicateP) {
    let predicate = VAL(predicateP);
    if(! predicates[predicate]) {
        abort("predicate to be indexed is undefined: " + atable[VAL(deref(memory[VAL(predicateP)+1]))] + "/" + VAL(deref(memory[VAL(predicateP)+2])));
    }

    let indexIndicator = PL_put_atom_chars("index");

    predicates[predicate].clauses[predicates[predicate].next_key] =
        {
            code:   compilation_environment.buffer.slice(0),
            key:    predicates[predicate].next_key,
            head:   record_term(indexIndicator),
            body:   record_term(indexIndicator)
        };

    predicates[predicate].clause_keys.push(predicates[predicate].next_key);
    predicates[predicate].index = predicates[predicate].next_key;
    predicates[predicate].next_key++;

    return true;
}

// predicate_edit_clauses_for_index_sequences modifies sequences of clauses
// to use try_me_else, retry_me_else, trust_me, and nop2 appropriately for
// supporting indexing using switch_on_term instruction.
// The sequencesP parameter is a Prolog list of lists of integers: these
// integers specify clauseKey indices (0-based) such that an integer K
// identifies a clause as:
//      let clause = predicates.clauses[predicates.clauseKeys[K]];
// The code for clause K has its first instruction modified appropriately
// for a control instruction according to the position of clause K in its
// containing sequence:
//  if the sequence only has one clause (K), then the control instruction is 'nop2';
//  if K is the first clause in its sequence then its control instruction is
//      'try_me_else(N)' where N is the clauseKey index of the next clause in
//      the sequence, which is alwasy (K+1);
//  if K is neither the first nor the last clause in its sequence then its control
//      instruction is retry_me_else(N), where N=K+1 as before; finally,
//  if K is the last clause in its sequence (of more than one clause) then
//      its control instruction is trust_me.

function predicate_edit_clauses_for_index_sequences(sequencesP, predicateP) {
    let sequences = integer_list_list_to_term_array(sequencesP);
    for(let ofst = 0;ofst < sequences.length; ofst++) {
        edit_clauses_for_index_sequence(sequences[ofst], predicateP);
    }
    return true;
}

function edit_clauses_for_index_sequence(sequence, predicateP) {
    let predicate = VAL(predicateP);
    if(sequence.length === 0) {
        abort("invalid empty sequence. A sequence must not be empty.")
    } else if(sequence.length === 1) {
        let clauseCode = predicates[predicate].clauses[predicates[predicate].clause_keys[sequence[0]]].code;
        clauseCode[0] = 254;
        clauseCode[1] = 0;
    } else {
        // first clause of sequence has try_me_else(second_clause)
        // clauses after first and before last have retry_me_else(next_clause)
        // last clause of sequence has trust_me(0).

        let clauseCode = predicates[predicate].clauses[predicates[predicate].clause_keys[sequence[0]]].code;
        clauseCode[0] = 28;
        clauseCode[1] = sequence[1];

        for(let ofst = 1;ofst < sequence.length-1;ofst++) {
            clauseCode = predicates[predicate].clauses[predicates[predicate].clause_keys[sequence[ofst]]].code;
            clauseCode[0] = 29;
            clauseCode[1] = sequence[ofst+1];
        }

        clauseCode = predicates[predicate].clauses[predicates[predicate].clause_keys[sequence[sequence.length-1]]].code;
        clauseCode[0] = 30;
        clauseCode[1] = 0;
    }
}

function add_clause_to_predicate(predicateP, head, body)
{
    // if(atable[VAL(deref(memory[VAL(predicateP)+1]))] === 'select_test') {
    //     console.log(JSON.stringify(record_term(head)) + " : " + JSON.stringify(record_term(body)));
    // }

    var predicate = VAL(lookup_functor(atable[VAL(deref(memory[VAL(predicateP)+1]))], VAL(deref(memory[VAL(predicateP)+2]))));
    if (predicates[predicate] === undefined || (predicates[predicate].is_public && predicates[predicate].clause_keys.length === 0))
    {
        // Easy case. New or empty predicate. Add it to the table then set up the <NOP,0> header
        compilation_environment.buffer[0] = 254;
        compilation_environment.buffer[1] = 0;
        check_compile_buffer(head, body);
        // If this predicate was defined as a dynamic predicate then it will have is_public already set to true.
        let is_public = predicates[predicate] && predicates[predicate].is_public;
        predicates[predicate] = {clauses: {0:{code:compilation_environment.buffer.slice(0),
                                              key:0, 
                                              head:record_term(head), 
                                              body:record_term(body)}},
                                 key:predicate,
                                 clause_keys: [0],
                                 is_public: is_public,
                                 next_key: 1};
    }
    else
    {
        var last_key = predicates[predicate].clause_keys[predicates[predicate].clause_keys.length-1];
        var last_clause = predicates[predicate].clauses[last_key];
        if (last_clause.code[0] === 254)
        {
            // Last clause was NOP - ie only clause. Make it try_me_else, and the new clause is trust_me
            last_clause.code[0] = 28;
            last_clause.code[1] = predicates[predicate].next_key;
            compilation_environment.buffer[0] = 30;
            compilation_environment.buffer[1] = 0;
            check_compile_buffer(head, body);            
            predicates[predicate].clauses[predicates[predicate].next_key] = {code:compilation_environment.buffer.slice(0),
                                                                             key: predicates[predicate].next_key,
                                                                             head:record_term(head), 
                                                                             body:record_term(body)};
            predicates[predicate].clause_keys.push(predicates[predicate].next_key);
            predicates[predicate].next_key++;
            
        }
        else if (last_clause.code[0] === 30)
        {
            // last clause was trust_me, so there is already a try_me_else. Make it retry_me_else and add new clause as trust_me
            last_clause.code[0] = 29;
            last_clause.code[1] = predicates[predicate].next_key;
            compilation_environment.buffer[0] = 30;
            compilation_environment.buffer[1] = 0;
            //compilation_environment.buffer.unshift(predicates[predicate].next_key); WHAT?
            check_compile_buffer(head, body);            
            predicates[predicate].clauses[predicates[predicate].next_key] = {code:compilation_environment.buffer.slice(0),
                                                                             key: predicates[predicate].next_key,
                                                                             head:record_term(head), 
                                                                             body:record_term(body)};
            predicates[predicate].clause_keys.push(predicates[predicate].next_key);
            predicates[predicate].next_key++;            
        }
        else
            abort("Garbage clauses: " + last_clause.code[0]);
    }

    return true;
}

function add_clause_to_aux(label, n, l, lt)
{
    debug_msg("Adding clause to aux: " + term_to_string(label) + " at " + VAL(n));
    if (TAG(label) === TAG_STR && memory[VAL(label)] === lookup_functor("defined", 1))
    {
        debug_msg("Aux code is at " + VAL(n));
        debug_msg("This is " + (VAL(n) ^ 0x80000000) & 0x7fffffff);
        add_clause_to_existing(VAL(memory[VAL(label)+1]), VAL(n) ^ 0x80000000);
        unify(l, lt);
    }
    else
    {
        debug_msg("Adding first clause");
        compilation_environment.buffer[VAL(n)] = 254;
        compilation_environment.buffer[VAL(n)+1] = 0;
        var ptr = state.H;
        memory[state.H++] = lookup_functor("defined", 1);
        memory[state.H++] = n;
        unify(label, ptr ^ (TAG_STR << WORD_BITS));

        var ptr2 = state.H;
        // noinspection UnnecessaryLocalVariableJS
        var ftor = lookup_functor("label", 2);
        memory[state.H++] = ftor;
        memory[state.H++] = label;
        memory[state.H++] = n;

        var ptr3 = state.H; // should unify with l
        memory[state.H++] = (ptr2) ^ (TAG_STR << WORD_BITS);

        var ptr4 = state.H;
        alloc_var();
        unify(ptr4, lt);

        unify(l, ptr3 ^ (TAG_LST << WORD_BITS));
    }
    return true;
}

function add_clause_to_existing(address, offset)
{
    while(true)
    {
        debug_msg("Examining address " + address + " with value " + compilation_environment.buffer[address] + " and offset " + offset) ;
        switch(compilation_environment.buffer[address])
        {
        case 254:
            // Change <NOP,0> -> try_me_else offset
            compilation_environment.buffer[address] = 28;
            compilation_environment.buffer[address+1] = offset;
            // Add <trust_me,0> for new clause
            compilation_environment.buffer[offset ^ 0x80000000] = 30;
            compilation_environment.buffer[(offset ^ 0x80000000)+1] = 0;
            return;
        case 30:
            // Change <trust_me,0> -> <retry_me_else, N>
            compilation_environment.buffer[address] = 29;
            compilation_environment.buffer[address+1] = offset;
            // Add <trust_me,0> for new clause
            compilation_environment.buffer[offset ^ 0x80000000] = 30;
            compilation_environment.buffer[(offset ^ 0x80000000)+1] = 0;
            return;
        case 28:
        case 29:
            address = compilation_environment.buffer[address+1] ^ 0x80000000;
            break;
        default:
            abort("Garbage in code array: " + compilation_environment.buffer[address]);
        }        
    }
}


function create_choicepoint()
{
    let nextCP = {code: bootstrap_code,
        predicate:state.current_predicate,  // Suspect
        offset:retry_foreign_offset};

    wam_create_choicepoint(nextCP, [0, {code: code, offset: state.P}]);
    return true;
}

function update_choicepoint_data(value)
{
    debug_msg('Updating backtrack ' + state.B + ' foreign value (at ' + (state.B+FCP_V) + ' to ' + value + ' which is ' + term_to_string(value));
    memory[state.B+FCP_V] = value;
    return true;
}

// used in proscript_interpreter_terminal.js
// noinspection JSUnusedLocalSymbols
function destroy_all_choicepoints() {
    while(state.B !== 0) {
        destroy_choicepoint();
    }
}

function destroy_choicepoint()
{
    debug_msg("Destroying choicepoint at " + state.B);
    var n = memory[state.B];
    unwind_trail(memory[state.B + n + CP_TR], state.TR);
    state.B = memory[state.B + n + CP_B];
    state.HB = memory[state.B+ memory[state.B] + CP_H];
    debug_msg("B is now " + state.B);
}

// For testing only! Assumes -,+ mode
function predicate_member_test(element, list)
{
    if (state.foreign_retry)
    {
        list = state.foreign_value;
        debug_msg("Is retry! Setting ptr back to " + hex(list) + " = " + term_to_string(list));
    }
    else
    {
        debug_msg("Not a retry");
        create_choicepoint();
    }    
    while(TAG(list) === TAG_LST)
    {
        var head = memory[VAL(list)];
        if (unify(head, element))
        {
            debug_msg("Unification succeeded. Setting choicepoint value @" +(state.B+FCP_V) + " to " + hex(memory[VAL(list)+1]));
            update_choicepoint_data(memory[VAL(list)+1]);
            return true;
        } else {
            // undo any bindings created by failed unify(head, element) call.
            var n = memory[state.B];
            unwind_trail(memory[state.B + n + CP_TR], state.TR);
        }
        list = memory[VAL(list)+1]
    }
    destroy_choicepoint();
    return false;
}

function predicate_debug()
{
    debugging = true;
    return true;
}

function predicate_nodebug()
{
    debugging = false;
    return true;
}

function predicate_jmp(vars)
{
    if(state.trace_call === 'trace_next_jmp') {
        state.trace_call = 'trace_next';
    } else if (state.trace_call === 'leap_trace_next_jmp') {
        state.trace_call = 'leap_trace_next';
    }

    state.P = -1; // PC will be incremented by 3 after this if we succeed to 2. This is where queries are compiled from, since the first two bytes are for try/retry/trust
    code = compilation_environment.buffer.slice(0);
    register[0] = vars;
    return true;
}

// function predicate_trace_call(CP, args) {
//     if(state.trace_call === 'trace_next_jmp') {
//         state.trace_call = 'trace_next';
//     }
//
//     code = CP.code;
//     let offset = CP.offset;
//     let predicate = CP.predicate;
//     state.P = offset - 3; // P is incremented by 3 after this foreign call succeeds.
// }

function mark_top_choicepoint(vars_list, markpoint)
{
    var vars = [];
    while(TAG(vars_list) === TAG_LST)
    {        
        vars.push(memory[VAL(vars_list)]);        
        vars_list = memory[VAL(vars_list) + 1];
    }
    if (vars_list !== NIL)
        abort("Invalid list in mark_top_choicepoint");

    debug_msg("Marking choicepoint " + state.B+ " with cleanup at " + (state.P+3) + " and code = " + code);
    let mark = {B: state.B,
            V: vars,
            P: state.P+3,
            code: code};
    cleanups.unshift(mark);
    // Skip the cleanup code
    state.P += 4;
    return unify(markpoint, state.B ^ (TAG_INT << WORD_BITS));
}

// FIXME: Not implemented: [c, d, D, e, E, I, N, p, s, @, t, |, +]
function predicate_format(stream, fmt, args) {

    //look for atom(X)
    if (TAG(stream) === TAG_STR) {
        let ftor = VAL(memory[VAL(stream)]);
        if (atable[ftable[ftor][0]] === "atom" && ftable_arity(ftor) === 1) {
            let arg = memory[VAL(stream)+1];
            if(TAG(arg) === TAG_REF) {
                let result = format_to_string(fmt, args);
                return unify(arg, lookup_atom(result));
            }
        }
    }

    var s = {};
    if (!get_stream(stream, s))
        return false;
    stream = s.value;
    var result = format_to_string(fmt, args);
    var bytes = toByteArray(result);
    return (stream.write(stream, 1, bytes.length, bytes) >= 0)
}

function format_to_string(fmt, args) {
    var result = "";
    fmt = atable[VAL(fmt)];
    var arg = args;
    var numarg = undefined;
    for (var i = 0; i < fmt.length; i++)
    {
        var c = fmt.charAt(i);
        if (c === '~')
        {
            while (true)
            {
                switch (fmt.charAt(i+1))
                {
                case 'a':
                    if (TAG(memory[VAL(arg)]) !== TAG_ATM)
                        return type_error("atom", arg);
                    // fall-through
                case 'w':
                    result += format_term(memory[VAL(arg)], {ignore_ops:false, numbervars:true, quoted:false});
                    arg = memory[VAL(arg)+1];
                    break;
                case 'W':
                    var a = memory[VAL(arg)];
                    arg = memory[VAL(arg)+1];
                    var options = parse_term_options(memory[VAL(arg)]);
                    result += format_term(a, options);
                    arg = memory[VAL(arg)+1];
                    break;
                    
                case 'i':
                    arg = memory[VAL(arg)+1];
                    break;
                case 'q':
                    result += format_term(memory[VAL(arg)], {ignore_ops:false, numbervars:true, quoted:true});
                    arg = memory[VAL(arg)+1];
                    break;
                case 'k':
                    result += format_term(memory[VAL(arg)], {ignore_ops:true, numbervars:true, quoted:true});
                    arg = memory[VAL(arg)+1];
                    break;
                case 'n':
                    result += "\n";
                    break;
                case '~':
                    result += "~";
                    break;
                case 'r':
                case 'R':
                    if (numarg === undefined)
                        return format_error("r,R requires radix specifier");
                    var e = {};
                    if (!evaluate_expression(memory[VAL(arg)], e))
                        return false;
                    if (fmt.charAt(i+1) === 'R')
                        result += e.value.toString(numarg).toUpperCase();
                    else
                        result += e.value.toString(numarg);
                    break;
                case '0':
                case '1':
                case '2':
                case '3':
                case '4':
                case '5':
                case '6':
                case '7':
                case '8':
                case '9':
                    numarg = fmt.charAt(i+1) - '0';
                    i+=2;
                    while (fmt.charAt(i) >= '0' && fmt.charAt(i) <= '9')
                    {
                        numarg = (numarg * 10) + (fmt.charAt(i)-'0');
                        i++;
                    }
                    i--;
                    continue;                
                default:
                    return existence_error("format_character", lookup_atom(fmt.charAt(i+1)));
                }
                i++;
                break; // Really this is just a goto for the numarg reading
            }
        }
        else if (c === '\\')
        {
            switch(fmt.charAt(i+1))
            {
            case '\\':
                result += "\\";
                break;
            case "n":
                result += "\n";
                break;
            default:
                abort("Unimplemented or escape character: " + fmt.charAt(i+1));
            }
            i++;
        }
        else
            result += c;
    }
    return result;
}

function unmark_choicepoint(mark)
{
    debug_msg("Unmarking: " + term_to_string(mark));
    mark = VAL(mark);    
    for (var i = 0; i < cleanups.length; i++)
    {
        if (cleanups[i].B === mark)
        {
            cleanups.splice(i, 1);
            // Additionally, we have to actually cut this choicepoint as well. This deserves an explanation!
            // Suppose we nest setup_call_cleanup(true, setup_call_cleanup(true, true, true), true).
            // Once we complete the inner true, we will unmark the choicepoint that allows us to distinguish exit from _.
            // but unless we cut the parent choicepoint (which allows us to distinguish success from error/failure)
            // this will persist. The outer cleanup will then see a choicepoint, even though the inner one has succeeded 
            // deterministically, and will exit with unbound port.
            state.B = memory[mark + memory[mark]+3];
            if (state.B > 0)
                tidy_trail();
            return true;
        }
    }
    debug("Looking for " + mark);
    debug(JSON.stringify(cleanups));
    abort("nope");
}

// This is used in the failure port. Since we have failed into the failure branch of the cleanup, there cannot be any choicepoints around except for the 
// one that got us here. Therefore, we can just delete the first cleanup handler (I hope!)
function unmark_top_choicepoint()
{
    cleanups.shift();
    return true;
}

function predicate_copy_term(t1, t2)
{
    return unify(t2, copy_term(t1));
}

function copy_term(term) {
    return recall_term(record_term(term), {});
}

function predicate_repeat()
{
    // Create a choicepoint that points to itself
    var newB;
    if (state.E > state.B) {
        newB = state.E + state.CP.code[state.CP.offset - 1] + 2;
    } 
    else
    {
        newB = state.B + memory[state.B] + CP_SIZE;
    }
    debug_msg("Creating foreign choicepoint on the stack at " + newB);
    memory[newB] = state.num_of_args+2;
    var n = memory[newB];
    memory[newB + FCP_V] = 0;
    debug_msg("Reserved @" + (newB + 1) + " for value");
    memory[newB + FCP_C] = {code: code,
                        offset: state.P};
    debug_msg("Saving " + n + " args including the two specials");
    for (var i = 0; i < state.num_of_args; i++)
    {
        debug_msg("Saving register " + i + "(" + hex(register[i]) + ") to " + (newB + 3 + i));
        memory[newB + FCP_R + i] = register[i];
    }
    // Save the current context
    memory[newB+n+CP_E] = state.E;
    memory[newB+n+CP_CP] = state.CP;
    memory[newB+n+CP_B] = state.B;
    memory[newB+n+CP_Next] = {code: code,
                        predicate: state.current_predicate, // suspect!
                        offset: state.P}; // Retry will just create the choicepoint again!
    memory[newB+n+CP_TR] = state.TR;
    memory[newB+n+CP_H] = state.H;
    memory[newB+n+CP_B0] = state.B0;
    memory[newB+n+CP_TC] = lookup_atom(state.trace_call);
    memory[newB+n+CP_TI] = state.trace_info;
    state.B = newB;
    state.HB = state.H;
    return true;
}

var flags = [];

function predicate_flag(key, old_value, new_value)
{
    if (TAG(key) === TAG_REF)
        return instantiation_error(key);
    if (TAG(key) !== TAG_ATM)
        return type_error("atom", key);
    key = atable[VAL(key)];
    var o = (TAG_INT << WORD_BITS);
    if (flags[key] !== undefined)
        o = flags[key] ^ (TAG_INT << WORD_BITS);
    if (!unify(o, old_value))
        return false;
    var n = {};
    if (evaluate_expression(new_value, n))
        flags[key] = n.value;
    else 
        return false;
    return true;
}

function predicate_atom_length(atom, length)
{
    if (TAG(atom) === TAG_REF)
        return instantiation_error(atom);
    if (TAG(atom) !== TAG_ATM)
        return type_error("atom", atom);
    return unify(atable[VAL(atom)].length ^ (TAG_INT << WORD_BITS), length);    
}

function predicate_atom_concat(atom1, atom2, atom12)
{
    var index;
    if (!state.foreign_retry)
    {        
        // First call, or deterministic
        if (TAG(atom1) === TAG_REF && TAG(atom12) === TAG_REF)
            return instantiation_error(atom1);
        if (TAG(atom1) !== TAG_REF && TAG(atom1) !== TAG_ATM)
            return type_error("atom", atom1);
        if (TAG(atom2) !== TAG_REF && TAG(atom2) !== TAG_ATM)
            return type_error("atom", atom2);
        if (TAG(atom12) !== TAG_REF && TAG(atom12) !== TAG_ATM)
            return type_error("atom", atom12);
        if (TAG(atom1) === TAG_ATM && TAG(atom2) === TAG_ATM)
        {
            // Deterministic case
            return unify(atom12, lookup_atom(atable[VAL(atom1)] + atable[VAL(atom2)]));
        }
        else 
        {
            // Nondeterministic case. Need a choicepoint:
            create_choicepoint();
            index = 0;
        }
    }
    else
    {
        index = state.foreign_value+1;
    }
    update_choicepoint_data(index);
    // Drop through to general nondeterministic case
    if (index === atable[VAL(atom12)].length+1)
    {
        destroy_choicepoint();
        return false;
    }
    return !!(unify(atom1, lookup_atom(atable[VAL(atom12)].substring(0, index))) && unify(atom2, lookup_atom(atable[VAL(atom12)].substring(index))));

}

function predicate_char_code(atom, code)
{
    if (TAG(atom) === TAG_REF && TAG(code) === TAG_REF)
        return instantiation_error(atom);
    if (TAG(atom) === TAG_ATM)
    {
        var a = atable[VAL(atom)];
        if (a.length !== 1)
            return type_error("character", atom);
        return unify(code, a.charCodeAt(0) ^ (TAG_INT << WORD_BITS));
    }
    else if (TAG(code) === TAG_INT)
    {
        if (VAL(code) < 0)
            return representation_error("character_code", code);
        return unify(atom, lookup_atom(String.fromCharCode(VAL(code))));
    }
}

function predicate_atom_chars(atom, chars)
{
    var charsJS;

    if (TAG(chars) === TAG_REF)
    {
        // Atom -> chars
        if (TAG(atom) !== TAG_ATM)
            return type_error("atom", atom);
        charsJS = atable[VAL(atom)].split('');
        var tmp = state.H;
        for (var i = 0; i < charsJS.length; i++)
        {
            memory[state.H] = lookup_atom(charsJS[i]);
            memory[state.H+1] = ((state.H+2) ^ (TAG_LST << WORD_BITS));
            state.H += 2;
        }
        memory[state.H-1] = NIL;
        return unify(chars, tmp ^ (TAG_LST << WORD_BITS));
    }
    else
    {
        // Chars -> Atom
        charsJS = [];
        while (TAG(chars) === TAG_LST)
        {
            charsJS.push(atable[VAL(memory[VAL(chars)])]);
            chars = memory[VAL(chars)+1];
        }
        if (chars !== NIL)
            return type_error("list", chars);
        return unify(atom, lookup_atom(charsJS.join('')));
    }
}

function predicate_atom_codes(atom, codes)
{
    var codesJS;

    if (TAG(atom) === TAG_ATM)
    {
        // Atom -> Codes
        codesJS = atable[VAL(atom)];
        var tmp = state.H ^ (TAG_LST << WORD_BITS);
        for (var i = 0; i < codesJS.length; i++)
        {
            memory[state.H] = codesJS.charCodeAt(i) ^ (TAG_INT << WORD_BITS);
            // If there are no more items we will overwrite the last entry with [] when we exit the loop
            memory[state.H+1] = ((state.H+2) ^ (TAG_LST << WORD_BITS));
            state.H += 2;
        }
        memory[state.H-1] = NIL;
        return unify(codes, tmp);
    }
    else
    {
        // Codes -> Atom
        codesJS = [];
        while (TAG(codes) === TAG_LST)
        {
            codesJS.push(String.fromCharCode(memory[VAL(codes)]));
            codes = memory[VAL(codes)+1];
        }
        if (codes !== NIL)
            return type_error("list", codes);
        return unify(atom, lookup_atom(codesJS.join('')));
    }
}
// return -1 if a < b
// return 0 if a === b
// return 1 if a > b
//
function compare_terms(a, b)
{
    switch(TAG(a))
    {
    case TAG_REF:
        if (TAG(b) === TAG_REF)
        {
            if (a === b)
                return 0;
            else if (a > b)
                return 1;
        }
        return -1;
    case TAG_FLT:
        if (TAG(b) === TAG_REF)
            return 1;
        if (TAG(b) === TAG_FLT)
        {
            if (floats[VAL(a)] === floats[VAL(b)])
                return 0;
            else if (floats[VAL(a)] > floats[VAL(b)])
                return 1;
        }
        return -1;
    case TAG_INT:
        if (TAG(b) === TAG_REF || TAG(b) === TAG_FLT)
            return 1;
        if (TAG(b) === TAG_INT)
        {
            if (VAL(a) === VAL(b))
                return 0;
            else if (VAL(a) > VAL(b))
                return 1;
        }
        return -1;
    case TAG_ATM:
        if (TAG(b) === TAG_REF || TAG(b) === TAG_FLT || TAG(b) === TAG_INT)
            return 1;
        if (TAG(b) === TAG_ATM)
        {
            if (atable[VAL(a)] === atable[VAL(b)])
                return 0;
            else if (atable[VAL(a)] > atable[VAL(b)])
                return 1;
        }
        return -1;
    case TAG_STR:
    case TAG_LST:
        if (TAG(b) === TAG_REF || TAG(b) === TAG_FLT || TAG(b) === TAG_INT || TAG(b) === TAG_ATM)
            return 1;
        var aftor;
        var bftor;
        if (TAG(a) === TAG_LST)
            aftor = lookup_functor(".", 2);
        else
            aftor = memory[VAL(a)];
        if (TAG(b) === TAG_LST)
            bftor = lookup_functor(".", 2);
        else
            bftor = memory[VAL(b)];
        if (ftable[VAL(aftor)][1] > ftable[VAL(bftor)][1])
            return 1;
        else if (ftable[VAL(aftor)][1] < ftable[VAL(bftor)][1])
            return -1;
        // At this point the arity is equal and we must compare the functor names
        if (atable[ftable[VAL(aftor)][0]] > atable[ftable[VAL(bftor)][0]])
            return 1;
        else if(atable[ftable[VAL(aftor)][0]] < atable[ftable[VAL(bftor)][0]])
            return -1;
        // So the functors are the same and we must compare the arguments.
        for (var i = 0; i < ftable[VAL(aftor)][1]; i++)
        {
            var result = compare_terms(memory[VAL(a)+1+i], memory[VAL(b)+1+i]);
            if (result !== 0)
                return result;
        }
    }
    return 0;
}

function predicate_compare(x, a, b)
{
    var i = compare_terms(a,b);
    if (i > 0) 
        return unify(x, lookup_atom(">"));
    else if (i < 0) 
        return unify(x, lookup_atom("<"));
    else
        return unify(x, lookup_atom("="));
}

function predicate_term_lt(a, b)
{
    return compare_terms(a,b) === -1;
}

function predicate_term_elt(a, b)
{
    return compare_terms(a,b) !== 1;
}

function predicate_term_gt(a, b)
{
    return compare_terms(a,b) === 1;
}

function predicate_term_egt(a, b)
{
    return compare_terms(a,b) !== -1;
}


function predicate_acyclic_term(t)
{
    var visited_cells = [];
    var stack = [t];
    while (stack.length !== 0)
    {        
        var arg = stack.pop();
        switch (TAG(arg))
        {
        case TAG_INT:
        case TAG_FLT:
        case TAG_ATM:
            continue;
        case TAG_REF:
            var needle = deref(arg);
            for (var cellOfst = 0; cellOfst < visited_cells.length; cellOfst++)
            {
                if (visited_cells[cellOfst] === needle)
                {
                    return false;
                }
            }
            continue;
        case TAG_LST:
            visited_cells.push(arg);
            stack.push(memory[VAL(arg)]);
            stack.push(memory[VAL(arg)+1]);
            continue;
        case TAG_STR:
            visited_cells.push(arg);
            var arity = ftable[VAL(memory[VAL(arg)])][1];
            for (var argOfst = 0; argOfst < arity; argOfst++)
                stack.push(memory[VAL(arg)+1+argOfst]);

        }
    }
    return true;
}

function predicate_number_chars(n, chars)
{
    var charsJS;
    if (TAG(chars) === TAG_REF)
    {
        // Atom -> chars
        if (TAG(n) === TAG_INT)
            charsJS = (VAL(n) + "").split('');
        else if (TAG(n) === TAG_FLT)
            charsJS = (floats[VAL(n)] + "").split('');
        else
            return type_error("number", n);
        var tmp = state.H;
        for (var i = 0; i < charsJS.length; i++)
        {
            memory[state.H] = lookup_atom(charsJS[i]);
            memory[state.H+1] = ((state.H+2) ^ (TAG_LST << WORD_BITS));
            state.H += 2;
        }
        memory[state.H-1] = NIL;
        return unify(chars, tmp ^ (TAG_LST << WORD_BITS));
    }
    else
    {
        // Chars -> Atom
        charsJS = [];
        while (TAG(chars) === TAG_LST)
        {
            charsJS.push(atable[VAL(memory[VAL(chars)])]);
            chars = memory[VAL(chars)+1];
        }
        if (chars !== NIL)
            return type_error("list", chars);
        var f = parseFloat(charsJS.join(''));
        // FIXME: Overflows
        if (~~f === f)
            return unify(n, f ^ (TAG_INT << WORD_BITS));
        else
        {            
            return unify(n, lookup_float(f));
        }
    }
}

function lookup_float(f)
{
    for (var i = 0; i < floats.length+1; i++)
    {
        if (floats[i] === f)
        {
            return i ^ (TAG_FLT << WORD_BITS);
        }
        if (floats[i] === undefined)
        {
            floats[i] = f;
            return i ^ (TAG_FLT << WORD_BITS);
        }
    }
    abort("Should not get here");
}

function predicate_number_codes(n, codes)
{
    var codesJS;
    if (TAG(codes) === TAG_REF)
    {
        // Atom -> codes
        if (TAG(n) === TAG_INT)
        {
            let value;
            if ((VAL(n) & (1 << (WORD_BITS-1))) === (1 << (WORD_BITS-1)))
                value = VAL(n) - (1 << WORD_BITS);
            else
                value = VAL(n);

            codesJS = value + "";
        }
        else if (TAG(n) === TAG_FLT)
        {
            codesJS = floats[VAL(n)] + "";
        }
        else
            return type_error("number", n);

        var tmp = state.H;
        for (var i = 0; i < codesJS.length; i++)
        {
            memory[state.H] = codesJS.charCodeAt(i) ^ (TAG_INT << WORD_BITS);
            memory[state.H+1] = ((state.H+2) ^ (TAG_LST << WORD_BITS));
            state.H += 2;
        }
        memory[state.H-1] = NIL;
        return unify(codes, tmp ^ (TAG_LST << WORD_BITS));
    }
    else
    {
        // Codes -> Atom
        codesJS = [];
        while (TAG(codes) === TAG_LST)
        {
            codesJS.push(String.fromCharCode(memory[VAL(codes)]));
            codes = memory[VAL(codes)+1];
        }
        if (codes !== NIL)
            return type_error("list", codes);
        var f = parseFloat(codesJS.join(''));
        // FIXME: Overflows
        if (~~f === f)
            return unify(n, f ^ (TAG_INT << WORD_BITS));
        else
            return unify(n, lookup_float(f));
    }
}

function predicate_subsumes_term(a, b)
{
    var before = term_variables(b);
    create_choicepoint();
    if (!unify(a,b))
    {
        destroy_choicepoint();
        return false;
    }
    if (!predicate_acyclic_term(b))
    {
        destroy_choicepoint();
        return false;
    }
    var after = term_variables(b);
    // We need to save a bit of info for this backtrack to not cause us some serious problems
    var oldP = state.P;
    var oldcode = code;
    var oldPred = state.current_predicate;
    backtrack();
    state.P = oldP;
    code = oldcode;
    state.current_predicate = oldPred;

    destroy_choicepoint();
    return (after.length === before.length);
}

function predicate_numbervars(term, start, end) {
    if(TAG(start) !== TAG_INT) {
        return type_error('integer', start);
    }
    let startID = VAL(start);
    let vars = term_variables(term);
    for(let ofst = 0;ofst < vars.length;ofst++) {
        unify(vars[ofst], construct_var_term(startID + ofst));
    }
    return unify(end, PL_put_integer(startID + vars.length));
}

function construct_var_term(id) {
    let ftor = lookup_functor('$VAR', 1);
    let var_structure = alloc_structure(ftor);
    memory[state.H++] = PL_put_integer(id);
    return var_structure;
}

function predicate_current_op(precedence, fixity, name)
{
    var index;
    if (state.foreign_retry) {
        index = state.foreign_value + 1;
    }
    else
    {
        create_choicepoint();
        index = 0;
    }
    update_choicepoint_data(index);
    // This is horrific
    var infix_count = Object.keys(infix_operators).length;
    var prefix_count = Object.keys(prefix_operators).length;
    var try_name;
    var try_fixity;
    var try_precedence;
    if (index >= infix_count + prefix_count)
    {
        destroy_choicepoint();
        return false;
    }
    else if (index >= infix_count)
    {
        try_name = Object.keys(prefix_operators)[index - infix_count];
        try_fixity = prefix_operators[try_name].fixity;
        try_precedence = prefix_operators[try_name].precedence;
    }
    else
    {
        try_name = Object.keys(infix_operators)[index];
        try_fixity = infix_operators[try_name].fixity;
        try_precedence = infix_operators[try_name].precedence;
    }
    return unify(name, lookup_atom(try_name)) && unify(fixity, lookup_atom(try_fixity)) && unify(precedence, try_precedence ^ (TAG_INT<<WORD_BITS));
}

var prolog_flags = [{name:"bounded", fn:flag_bounded},
                    {name:"max_integer", fn:flag_max_integer},
                    {name:"min_integer", fn:flag_min_integer},
                    {name:"integer_rounding_function", fn:flag_integer_rounding_function},
                    {name:"char_conversion", fn:flag_char_conversion},
                    {name:"debug", fn:flag_debug},
                    {name:"max_arity", fn:flag_max_arity},
                    {name:"unknown", fn:flag_unknown},
                    {name:"double_quotes", fn:flag_double_quotes},
                    {name:"dialect", fn:flag_dialect}];

var prolog_flag_values = {char_conversion: false,
                          debug: false,
                          unknown: "error",
                          double_quotes: "codes"};

function flag_bounded(set, value)
{
    if (set) return permission_error("prolog_flag");
    return unify(value, lookup_atom("true"));
}

function flag_max_integer(set, value)
{
    if (set) return permission_error("prolog_flag");
    return unify(value, ((2**(WORD_BITS-1)-1) & ((1 << WORD_BITS)-1)) ^ (TAG_INT<<WORD_BITS));
}

function flag_min_integer(set, value)
{
    if (set) return permission_error("prolog_flag");
    let newTerm = ((-1*(2**(WORD_BITS-1))) & ((1 << WORD_BITS)-1)) ^ (TAG_INT << WORD_BITS);
    return unify(value, newTerm);
}

function flag_integer_rounding_function(set, value)
{
    if (set) return permission_error("prolog_flag");
    return unify(value, lookup_atom("toward_zero"));
}

function flag_char_conversion(set, value)
{
    if (set) 
    {
        if (TAG(value) === TAG_ATM && atable[VAL(value)] === "on")
            prolog_flag_values.char_conversion = true;
        else if (TAG(value) === TAG_ATM && atable[VAL(value)] === "off")
            prolog_flag_values.char_conversion = false;
        else
            return type_error("flag_value", value);
        return true;
    }
    return unify(value, prolog_flag_values.char_conversion?lookup_atom("on"):lookup_atom("off"));
}

function flag_debug(set, value)
{
    if (set) 
    {
        if (TAG(value) === TAG_ATM && atable[VAL(value)] === "on")
            prolog_flag_values.debug = true;
        else if (TAG(value) === TAG_ATM && atable[VAL(value)] === "off")
            prolog_flag_values.debug = false;
        else
        {
            return type_error("flag_value", value);
        }
        return true;
    }
    return unify(value, prolog_flag_values.debug?lookup_atom("on"):lookup_atom("off"));
}

function flag_max_arity(set, value)
{
    if (set) return permission_error("prolog_flag");
    return unify(value, lookup_atom("unbounded"));
}

function flag_unknown(set, value)
{
    if (set) 
    {
        if (TAG(value) === TAG_ATM && atable[VAL(value)] === "error")
            prolog_flag_values.unknown = "error";
        else if (TAG(value) === TAG_ATM && atable[VAL(value)] === "fail")
            prolog_flag_values.unknown = "fail";
        else if (TAG(value) === TAG_ATM && atable[VAL(value)] === "warning")
            prolog_flag_values.unknown = "warning";
        else
            return type_error("flag_value", value);
        return true;
    }
    return unify(value, lookup_atom(prolog_flag_values.unknown));
}

function flag_double_quotes(set, value)
{
    if (set) 
    {
        if (TAG(value) === TAG_ATM && atable[VAL(value)] === "chars")
            prolog_flag_values.double_quotes = "chars";
        else if (TAG(value) === TAG_ATM && atable[VAL(value)] === "codes")
            prolog_flag_values.double_quotes = "codes";
        else if (TAG(value) === TAG_ATM && atable[VAL(value)] === "atom")
            prolog_flag_values.double_quotes = "atom";
        else
            return type_error("flag_value", value);
        return true;
    }
    return unify(value, lookup_atom(prolog_flag_values.double_quotes));
}

function flag_dialect(set, value)
{
    if (set) return permission_error("prolog_flag");
    return unify(value, lookup_atom("proscriptls"));
}


function predicate_set_prolog_flag(key, value)
{
    if (TAG(key) !== TAG_ATM)
        return type_error("atom", key);
    var keyname = atable[VAL(key)];
    
    for (var i = 0; i < prolog_flags.length; i++)
    {
        if (prolog_flags[i].name === keyname)
        {
            return prolog_flags[i].fn(true, value);
        }
    }
    debug("No such flag");
    return false;
}

function predicate_current_prolog_flag(key, value)
{
    if (TAG(key) === TAG_REF)
    {
        let index;

        if (state.foreign_retry)
        {
            index = state.foreign_value + 1;         
        }
        else
        {
            create_choicepoint();
            index = 0;
        }
        update_choicepoint_data(index);        
        if (index >= prolog_flags.length)
        {
            destroy_choicepoint();
            return false;
        }
        unify(key, lookup_atom(prolog_flags[index].name));
        return prolog_flags[index].fn(false, value);        
    }
    else if (TAG(key) === TAG_ATM)
    {
        let keyname = atable[VAL(key)];
        for (let i = 0; i < prolog_flags.length; i++)
        {
            if (prolog_flags[i].name === keyname)
                return prolog_flags[i].fn(false, value);
        }
        return false;
    }
    else
        return type_error("atom", key);
}

function predicate_clause(head, body)
{
    var ftor;
    var index;
    if (TAG(head) === TAG_REF)
        return instantiation_error(head);
    else if (TAG(head) === TAG_ATM)
    {
        ftor = VAL(lookup_functor(atable[VAL(head)], 0));
    }
    else if (TAG(head) === TAG_STR)
    {
        ftor = VAL(memory[VAL(head)]);
    }
    else
        return type_error("callable", head);
    if (predicates[ftor].is_public !== true)
        return permission_error("access", "private_procedure", head);
    if (!state.foreign_retry)
    {
        create_choicepoint();
        index = 0;
    }
    else
    {
        index = state.foreign_value + 1;
    }
    update_choicepoint_data(index);
    if (index >= predicates[ftor].clause_keys.length)
    {
        destroy_choicepoint();
        return false;
    }
    var key = predicates[ftor].clause_keys[index];
    var varmap = {};
    var head_ref = recall_term(predicates[ftor].clauses[key].head, varmap);
    if (unify(head_ref, head))
    {
        return !!unify(recall_term(predicates[ftor].clauses[key].body, varmap), body);
    }
    else
    {
        return false;    
    }
}

// indicator may have module prefix - Module:Name/Arity - or not - Name/Arity.
// Module:Name/Arity = :(Module, /(Name, Arity)).

function predicate_current_predicate(indicator)
{
    let colon = lookup_functor(":", 2);
    var slash2 = lookup_functor("/", 2);
    let indicatorHasModule = false;
    var index;
    if (!state.foreign_retry)
    {
        if (TAG(indicator) === TAG_STR)
        {
            if (memory[VAL(indicator)] === colon || memory[VAL(indicator)] === slash2) {
                let slashStructure;
                let name;
                let arity;
                if (memory[VAL(indicator)] === colon) {
                    // :(ModuleName, /(Functor, Arity))
                    indicatorHasModule = true;
                    let moduleName = memory[VAL(indicator) + 1];
                    slashStructure = memory[VAL(indicator) + 2];
                    if (TAG(slashStructure) !== TAG_STR) {
                        return type_error('predicate_indicator', slashStructure);
                    }
                    name = memory[VAL(slashStructure) + 1];
                    arity = memory[VAL(slashStructure) + 2];

                    if (TAG(moduleName) === TAG_REF) {
                        return instantiation_error(moduleName);
                    }

                    let moduleNameJS = PL_get_atom_chars(moduleName);
                    if (TAG(name) === TAG_ATM) {
                        let nameJS = PL_get_atom_chars(name);
                        if (!nameJS.includes(":")) {
                            name = PL_put_atom_chars(moduleNameJS + ':' + nameJS);
                        }
                    }
                } else {
                    // VAL(indicator)] === slash2
                    name = memory[VAL(indicator) + 1];
                    arity = memory[VAL(indicator) + 2];

                    if (TAG(name) === TAG_ATM) {
                        let nameJS = PL_get_atom_chars(name);
                        if (!nameJS.includes(":")) {
                            return domain_error('qualified_functor', indicator);
                        }
                    }
                }
                if (TAG(arity) !== TAG_INT && TAG(arity) !== TAG_REF)
                    return type_error("integer", arity);
                if (TAG(name) !== TAG_ATM && TAG(name) !== TAG_REF)
                    return type_error("atom", name);

                if (TAG(name) === TAG_ATM && TAG(arity) === TAG_INT) {
                    // Deterministic
                    var ftor = VAL(lookup_functor(atable[VAL(name)], VAL(arity)));
                    if (predicates[ftor] !== undefined)
                        return true;
                    else if (foreign_predicates[ftor] !== undefined)
                        return true;
                    return false;
                }
            }
            else
                return type_error("predicate_indicator", indicator);
        }
        // We are going to have to enumerate all predicates
        create_choicepoint();
        index = 0;
    }
    else {
        index = state.foreign_value + 1;
    }

    if (index >= Object.keys(predicates).length)
    {
        destroy_choicepoint();
        return false;
    }
    update_choicepoint_data(index);
    var key = Object.keys(predicates)[index];
    let qualifiedFunctorPL = PL_put_atom(ftable[key][0]);
    let arityPL = PL_put_integer(ftable[key][1]);
    var result;
    if(indicatorHasModule) {
        // slashStructure = /(UnqualifiedFunctor, Arity)
        let qualifiedFunctorJS = PL_get_atom_chars(qualifiedFunctorPL);
        let position = qualifiedFunctorJS.indexOf(":");
        let unqualifiedFunctorJS = qualifiedFunctorJS.substring(position+1);
        let unqualifiedFunctorPL = PL_put_atom_chars(unqualifiedFunctorJS);
        let slashStructure = state.H ^ (TAG_STR << WORD_BITS);
        memory[state.H++] = slash2;
        memory[state.H++] = unqualifiedFunctorPL;
        memory[state.H++] = arityPL;

        // result = :( ModuleName, /(UnqualifiedFunctor, Arity))
        result = state.H ^ (TAG_STR << WORD_BITS);
        memory[state.H++] = colon;
        memory[state.H++] = moduleNamePL;
        memory[state.H++] = slashStructure;
    } else {
        // result = /(QualifiedFunctor, Arity)
        result = state.H ^ (TAG_STR << WORD_BITS);
        memory[state.H++] = slash2;
        memory[state.H++] = qualifiedFunctorPL;
        memory[state.H++] = arityPL;
    }
    return unify(result, indicator);
}

function predicate_abolish(indicator)
{
    var slash2 = lookup_functor("/", 2);
    if (TAG(indicator) === TAG_STR && memory[VAL(indicator)] === slash2)
    {
        var name = deref(memory[VAL(indicator) + 1]);
        var arity = deref(memory[VAL(indicator) + 2]);
        if (TAG(name) === TAG_ATM && TAG(arity) === TAG_INT)
        {
            if (VAL(arity) < 0)
                return domain_error("not_less_than_zero", arity);
            var ftor = VAL(lookup_functor(atable[VAL(name)], VAL(arity)));
            if (predicates[ftor].is_public !== true)
                return permission_error("modify", "static_procedure", indicator);
            predicates[ftor] = undefined;
            return true;
        }
        else if (TAG(name) === TAG_REF)
            return instantiation_error(name);
        else if (TAG(name) !== TAG_ATM)
            return type_error("atom", name);
        else if (TAG(arity) === TAG_REF)
            return instantiation_error(arity);
        else if (TAG(arity) !== TAG_INT)
            return type_error("integer", arity);
    }
    else if (TAG(indicator) === TAG_REF)
        return instantiation_error(indicator);
    else
        return type_error("predicate_indicator", indicator);
}

function predicate_retract_clause(head, body)
{
    var ftor;
    var index;
    let sub_head = head;
    if (TAG(head) === TAG_REF)
        return instantiation_error(head);
    else if (TAG(head) === TAG_ATM)
    {
        ftor = VAL(lookup_functor(atable[VAL(head)], 0));
    }
    else if (TAG(head) === TAG_STR)
    {
        ftor = VAL(memory[VAL(head)]);
    }
    else
        return type_error("callable", head);

    if( ftor === VAL(lookup_functor(":", 2))) {
        // foo : bar(a)
        // -> 'foo:bar'(a)
        let moduleName = deref(memory[VAL(head) + 1]);
        sub_head = deref(memory[VAL(head) + 2]);

        let moduleNameJS = PL_get_atom_chars(moduleName);

        if (TAG(sub_head) === TAG_REF)
            return instantiation_error(sub_head);
        else if (TAG(sub_head) === TAG_ATM)
        {
            let functorJS = atable[VAL(sub_head)];
            if(! functorJS.includes(':')) {
                functorJS = moduleNameJS + ":" + functorJS;
            } else {
                let prefix = functorJS.substring(0, functorJS,indexOf(":"));
                if(!plausibleModuleName(prefix)) {
                    functorJS = PL_put_atom_chars(moduleNameJS + ':' + nameJS);
                }
            }
            ftor = VAL(lookup_functor(functorJS, 0));
        }
        else if (TAG(sub_head) === TAG_STR)
        {
            ftor = VAL(memory[VAL(sub_head)]);
            let arity = ftable[ftor][1];
            let functorJS = atable[ftable[ftor][0]];
            if(! functorJS.includes(':')) {
                functorJS = moduleNameJS + ":" + functorJS;
            } else {
                let prefix = functorJS.substring(0, functorJS.indexOf(":"));
                if(!plausibleModuleName(prefix)) {
                    functorJS = PL_put_atom_chars(moduleNameJS + ':' + functorJS);
                }
            }
            ftor = VAL(lookup_functor(functorJS, arity));
        }
        else
            return type_error("callable", sub_head);
    }

    if (predicates[ftor].is_public !== true)
        return permission_error("access", "static_procedure", head);
    if (!state.foreign_retry)
    {
        create_choicepoint();
        index = 0;
    }
    else
    {
        index = state.foreign_value + 1;
    }
    update_choicepoint_data(index);
    if (index >= predicates[ftor].clause_keys.length)
    {
        destroy_choicepoint();
        return false;
    }
    var key = predicates[ftor].clause_keys[index];
    var varmap = {};
    // let functor = ftable[ftor][0];
    // print('retract: ' + atable[functor] + ', key ' + key + ' at index ' + index);
    // print('  predicate: ' + JSON.stringify(predicates[ftor]));

    // With the use of modules, the head_ref and head structures may differ
    // where head_ref = 'bar:foo'(a,b) while head = bar : foo(a,b).
    // In this example, the functor for 'head' is ':' (the module operator).

    var head_ref = recall_term(predicates[ftor].clauses[key].head, varmap);
    if (unify_head_args(head_ref, sub_head, ftable[ftor][1]))
    {
        var body_ref = recall_term(predicates[ftor].clauses[key].body, varmap);
        if (unify(body_ref, body))
        {
            // Delete this clause. This is not a trivial operation!
            var p = predicates[ftor];
            // // First case: This is the only predicate
            if (p.clause_keys.length === 1)
            {
                //if(!predicates[ftor].is_dynamic) {
                //    predicates[ftor] = undefined;
                //}
                // remove the key
                p.clauses[key] = undefined;
                p.clause_keys.shift();
                //destroy_choicepoint();// reserve bindings
                return true;
            }
            else
                if (index === 0)
            {
                // Delete the first clause. Update the second clause from either:
                // 1) trust_me -> NOP
                // 2) retry_me_else -> try_me_else
                if (p.clauses[p.clause_keys[1]].code[0] === 30)
                    p.clauses[p.clause_keys[1]].code[0] = 254;
                else if (p.clauses[p.clause_keys[1]].code[0] === 29)
                    p.clauses[p.clause_keys[1]].code[0] = 28;
                else
                    abort("Garbage clauses in retract: " + p.clauses[p.clause_keys[1]].code[0]);
                // and remove the key
                p.clauses[key] = undefined;
                p.clause_keys.shift();
                update_choicepoint_data(index-1);
                return true;
            }
            else if (index === p.clause_keys.length-1)
            {
                // Remove the last clause. Update the second-to-last clause from either:
                // 1) try_me_else -> NOP
                // 2) retry_me_else -> trust_me
                if (p.clauses[p.clause_keys[p.clause_keys.length-2]].code[0] === 28)
                    p.clauses[p.clause_keys[p.clause_keys.length-2]].code[0] = 254;
                else if (p.clauses[p.clause_keys[p.clause_keys.length-2]].code[0] === 29)
                    p.clauses[p.clause_keys[p.clause_keys.length-2]].code[0] = 30;
                else
                    abort("Garbage clauses in retract: " + p.clauses[p.clause_keys[p.clause_keys.length-2]].code[0]);            
                // and remove the key
                p.clauses[key] = undefined;
                p.clause_keys.pop();
                //destroy_choicepoint(); // preserve bindings
                return true;
            }
            else
            {
                // Delete a clause from the middle. Update the previous clause from either:
                // try_me_else N -> try_me_else <following clause key>
                // retry_me_else N -> retry_me_else <following clause key>
                p.clauses[p.clause_keys[index-1]].code[1] = p.clause_keys[index+1];
                // and remove the key
                p.clauses[key] = undefined;
                for (var i = 0; i < p.clause_keys.length; i++)
                {
                    if (p.clause_keys[i] === key)
                    {
                        p.clause_keys.splice(i, 1);
                        update_choicepoint_data(index-1);
                        return true;
                    }
                }
                abort("No such key?!");
            }
        }
    }
    return false; // Nothing to retract
}

// With the use of modules, the concatenatedHead and moduleHead structures may differ
// where concatenatedHead = 'bar:foo'(a,b) while moduleHead = bar : foo(a,b).
// In this example, the functor for 'moduleHead' is ':' (the module operator).

function unify_head_args(concatenatedHead, moduleHead, arity) {
    // Assume that the concatenatedHead and moduleHead are two forms of reference
    // to the same functor 'bar:foo'/2.
    // These heads unify if their corresponding arguments unify.

    let cBase = VAL(concatenatedHead);
    let mBase = VAL(moduleHead);

         for(let ofst = 0;ofst < arity;ofst++) {
            if(! unify(deref(cBase + 1 + ofst), deref(mBase + 1 + ofst))) {
                return false;
            }
        }
        return true;
}

function predicate_sub_atom(source, start, length, remaining, subatom)
{
    var index;
    if (TAG(source) === TAG_REF)
        return instantiation_error(source);
    else if (TAG(source) !== TAG_ATM)
        return type_error("atom", source);
    if (TAG(subatom) !== TAG_ATM && TAG(subatom) !== TAG_REF)
        return type_error("atom", subatom);
    var input = atable[VAL(source)];
    if (!state.foreign_retry)
    {
        index = {start:0, fixed_start:false, length:0, fixed_length:false, remaining:input.length, fixed_remaining:false};
        if (TAG(start) === TAG_INT)
        {
            index.fixed_start = true;
            index.start = VAL(start);
        }
        if (TAG(length) === TAG_INT)
        {
            index.fixed_length = true;
            index.length = VAL(length);
        }
        if (TAG(remaining) === TAG_INT)
        {
            index.fixed_remaining = true;
            index.remaining = VAL(remaining);
        }
        if (index.fixed_start && index.fixed_remaining && !index.fixed_length)
        {
            // Deterministic: Fall through to bottom case
            index.length = input.length-index.start-index.remaining;
            index.fixed_length = true;
        }
        if (index.fixed_remaining && index.fixed_length && !index.fixed_start)
        {
            // Deterministic: Fall through to bottom case
            index.start = input.length-index.length-index.remaining;
            index.fixed_start = true;
        }
        if (index.fixed_start && index.fixed_length)
        {
            // Deterministic general case.
            return unify(remaining, (input.length-index.start-index.length) ^ (TAG_INT << WORD_BITS)) && 
                unify(start, (index.start) ^ (TAG_INT << WORD_BITS)) && 
                unify(length, (index.length) ^ (TAG_INT << WORD_BITS)) && 
                unify(subatom, lookup_atom(input.substring(index.start, index.start+index.length)));
        }
        // Otherwise nondeterministic
        create_choicepoint();
    }
    else
    {
        index = state.foreign_value;
        if (!index.fixed_length)
        {
            index.length++;
            if (index.start + index.length > input.length)
            {
                index.length = 0;
                if (!index.fixed_start)
                {
                    index.start++;
                    if (index.start > input.length)
                    {
                        destroy_choicepoint();
                        return false;
                    }
                }
                else
                {
                    // start is fixed, so length and remaining are free
                    // but remaining is always just computed
                    destroy_choicepoint();
                    return false;
                }
            }
        }
        else
        {
            // length is fixed, so start and remaining must be free
            index.start++;
            index.remaining--;
            if (index.length + index.start > input.length)
            {
                destroy_choicepoint();
                return false;
            }
        }
    }
    update_choicepoint_data(index);
    return unify(remaining, (input.length-index.start-index.length) ^ (TAG_INT << WORD_BITS)) && 
        unify(start, (index.start) ^ (TAG_INT << WORD_BITS)) && 
        unify(length, (index.length) ^ (TAG_INT << WORD_BITS)) && 
        unify(subatom, lookup_atom(input.substring(index.start, index.start+index.length)));
}

function predicate_eval_javascript(expression, result)
{
    var expressionJS;
    if (TAG(expression) === TAG_ATM) {
        expressionJS = PL_atom_chars(expression);
    } else if (TAG(expression) === TAG_LST) {
        let container = {};
        if(!codes_to_string(expression, container, true)) {
            return false;
        }
        expressionJS = container.value;
    } else {
        return instantiation_error(expression);
    }

    var resultJS = eval(expressionJS);
    var resultPL = resultJS ? string_to_codes(resultJS) : string_to_codes('undefined');
    if(result) {
        return unify(result, resultPL);
    } else {
        return true;
    }
}

function predicate_absolute_file_name(relative, absolute, options) {
    if(TAG(relative) === TAG_REF) {
        return instantiation_error(relative);
    } else if(TAG(relative) !== TAG_ATM) {
        return type_error('atom', relative);
    }

    let relativeJS = PL_get_atom_chars(relative);
    if(!relativeJS) {
        return domain_error('atom', relative);
    }

    if(relativeJS.startsWith("/")) {
        return unify(relative, absolute);
    }

    if(typeof __dirname === 'undefined' || ! __dirname)  {
        return unify(relative, absolute);
    }

    let parent = __dirname; // this only works in NodeJS.

    let absoluteJS;
    while(!absoluteJS) {
        if (relativeJS.startsWith("./")) {
            relativeJS = relativeJS.substr(2);
        } else if (relativeJS.startsWith("../")) {
            relativeJS = relativeJS.substr(3);
            let lastIndex = parent.lastIndexOf("/");
            if (lastIndex === 0) {
                // at root
                parent = "/";
                absoluteJS = parent + "/" + relativeJS;
            } else if(lastIndex > 0) {
                parent = parent.substring(0, lastIndex);
            } else {
                return engine_error("reference directory has no slash: " + parent);
            }
        } else {
            absoluteJS = parent + "/" + relativeJS;
        }
    }

    return unify(absolute, lookup_atom((absoluteJS)));
}

/* errors */
function type_error(expected, got)
{
    let ref_in;

    if(state.current_predicate !== null) {
        // var ftor_predicate = lookup_functor('/', 2);
        // var ref_predicate = state.H ^ (TAG_STR << WORD_BITS);
        // memory[state.H++] = ftor_predicate;
        // memory[state.H++] = PL_put_atom(ftable[state.current_predicate.key][0]);
        // memory[state.H++] = PL_put_integer(ftable[state.current_predicate.key][1]);

        let instruction = decode_instruction(state.current_predicate, state.P);

        var ftor_in = lookup_functor('in', 1);
        ref_in = state.H ^ (TAG_STR << WORD_BITS);
        memory[state.H++] = ftor_in;
//        memory[state.H++] = ref_predicate;
        memory[state.H++] = PL_put_atom_chars(instruction.string);
    } else {
        ref_in = PL_put_atom_chars("no current predicate");
    }

    var ftor = lookup_functor('type_error', 2);
    var ref = state.H ^ (TAG_STR << WORD_BITS);
    memory[state.H++] = ftor;
    memory[state.H++] = lookup_atom(expected);
    memory[state.H++] = got;
    memory[state.H++] = ref_in;
    return predicate_throw(ref);
}

function permission_error(action, type, instance)
{
    var ftor = lookup_functor('permission_error', 3);
    var ref = state.H ^ (TAG_STR << WORD_BITS);
    memory[state.H++] = ftor;
    memory[state.H++] = lookup_atom(action);
    memory[state.H++] = lookup_atom(type);
    memory[state.H++] = instance;
    return predicate_throw(ref);
}

function instantiation_error(v)
{
    var ftor = lookup_functor('instantiation_error', 1);
    var ref = state.H ^ (TAG_STR << WORD_BITS);
    memory[state.H++] = ftor;
    memory[state.H++] = v;
    return predicate_throw(ref);
}

function domain_error(domain, got)
{
    var ftor = lookup_functor('domain_error', 2);
    var ref = state.H ^ (TAG_STR << WORD_BITS);
    memory[state.H++] = ftor;
    memory[state.H++] = lookup_atom(domain);
    memory[state.H++] = got;
    return predicate_throw(ref);

}

function format_error(message)
{
    var ftor = lookup_functor('format_error', 1);
    var ref = state.H ^ (TAG_STR << WORD_BITS);
    memory[state.H++] = ftor;
    memory[state.H++] = lookup_atom(message);
    return predicate_throw(ref);
}

function existence_error(type, instance)
{
    let ref_in;

    if(state.current_predicate !== null) {
        // var ftor_predicate = lookup_functor('/', 2);
        // var ref_predicate = state.H ^ (TAG_STR << WORD_BITS);
        // memory[state.H++] = ftor_predicate;
        // memory[state.H++] = PL_put_atom(ftable[state.current_predicate.key][0]);
        // memory[state.H++] = PL_put_integer(ftable[state.current_predicate.key][1]);

        let instruction = decode_instruction(state.current_predicate, state.P);

        var ftor_in = lookup_functor('in', 1);
        ref_in = state.H ^ (TAG_STR << WORD_BITS);
        memory[state.H++] = ftor_in;
//        memory[state.H++] = ref_predicate;
        memory[state.H++] = PL_put_atom_chars(instruction.string);
    } else {
        ref_in = PL_put_atom_chars("no current predicate");
    }

    var ftor = lookup_functor('existence_error', 3);
    var ref = state.H ^ (TAG_STR << WORD_BITS);
    memory[state.H++] = ftor;
    memory[state.H++] = lookup_atom(type);
    memory[state.H++] = instance;
    memory[state.H++] = ref_in;

    return predicate_throw(ref);
}

function representation_error(type, instance)
{
    var ftor = lookup_functor('representation_error', 2);
    var ref = state.H ^ (TAG_STR << WORD_BITS);
    memory[state.H++] = ftor;
    memory[state.H++] = lookup_atom(type);
    memory[state.H++] = instance;
    return predicate_throw(ref);
}

function syntax_error(message)
{
    var ftor = lookup_functor('syntax_error', 1);
    var ref = state.H ^ (TAG_STR << WORD_BITS);
    memory[state.H++] = ftor;
    memory[state.H++] = lookup_atom(JSON.stringify(message));
    return predicate_throw(ref);
}

function io_error(message)
{
    var ftor = lookup_functor('io_error', 1);
    var ref = state.H ^ (TAG_STR << WORD_BITS);
    memory[state.H++] = ftor;
    memory[state.H++] = lookup_atom(message);
    return predicate_throw(ref);
}

function evaluation_error(message)
{
    var ftor = lookup_functor('evaluation_error', 1);
    var ref = state.H ^ (TAG_STR << WORD_BITS);
    memory[state.H++] = ftor;
    memory[state.H++] = lookup_atom(message);
    return predicate_throw(ref);
}

function engine_error(message) {
    var ftor = lookup_functor('engine_error', 1);
    var ref = state.H ^ (TAG_STR << WORD_BITS);
    memory[state.H++] = ftor;
    memory[state.H++] = lookup_atom(message);
    return predicate_throw(ref);

}