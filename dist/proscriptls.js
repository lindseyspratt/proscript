/* This file generated automatically. It defines the runtime engine for ProscriptLS.*/
// File foreign.js
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
        evaluated.value = PL_get_integer(expression);
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
    else if (TAG(expression) === TAG_ATM && expression === lookup_atom("random_float"))
    {
        // returns a random value in the open interval 0.0 < Random < 1.0
        let randomValue = Math.random();
        while(randomValue === 0.0) {
            randomValue = Math.random();
        }
        evaluated.value = randomValue;
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
        else if (name === "random" && arity === 1) {
            // random(L) returns integer X in range 0 =< X < L.
            let max = Math.floor(v[0]);
            evaluated.value =  Math.floor(Math.random() * max ); //The maximum is exclusive
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
//    else
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
                list_term = get_arg(list_term, 1); //memory[VAL(list_term) + 1];
                // Now write the args
                while (TAG(list_term) === TAG_LST) {
                    // Write head
                    memory[state.H++] = get_arg(list_term, 0); //memory[VAL(list_term)];
                    // Update tail
                    list_term = get_arg(list_term, 1); //memory[VAL(list_term) + 1];
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
    c = deref(c);

    if(TAG(c) === TAG_INT) {
        value = VAL(c);
    } else if(TAG(c) === TAG_STR) {
        // c = extended_address(BaseC)
        // value = VAL(BaseC) ^ 0x80000000
        let functor = VAL(get_arg(c, 0)); //memory[VAL(c)]);
        let functorName = atable[ftable[functor][0]];
        if(functorName === 'extended_address' &&
        ftable[functor][1] === 1) {
            let arg = get_arg(c, 1); //memory[VAL(c)+1];
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

// dynamic implies public. In ProscriptLS, public also implies dynamic.
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
    var head = get_arg(listPL, 0); //memory[VAL(listPL)];
    var tail = get_arg(listPL, 1); //memory[VAL(listPL)+1];
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
            head = get_arg(tail, 0); //memory[VAL(tail)];
            tail = get_arg(tail, 1); //memory[VAL(tail)+1];
        }
        else
            throw('Invalid atom list. Last item was not NIL.');
    }
}

function atom_or_var_list_to_term_array(listPL) {

    let result = [];
    var head = get_arg(listPL, 0); //[VAL(listPL)];
    var tail = get_arg(listPL, 1); //memory[VAL(listPL)+1];
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
            head = get_arg(tail, 0); //memory[VAL(tail)];
            tail = get_arg(tail, 1); //memory[VAL(tail)+1];
        }
        else
            throw('Invalid atom-or-var list. Last item was not NIL.');
    }
}


function integer_list_to_term_array(listPL) {

    let result = [];
    var head = get_arg(listPL, 0); //memory[VAL(listPL)];
    var tail = get_arg(listPL, 1); //memory[VAL(listPL)+1];
    while (true)
    {
        if(TAG(head) !== TAG_INT) {
            throw('Invalid integer list. Item is not an integer.');
        }

        result.push(PL_get_integer(head));

        if (tail === NIL)
            return result;
        else if (TAG(tail) === TAG_LST)
        {
            head = get_arg(tail, 0); //memory[VAL(tail)];
            tail = get_arg(tail, 1); //memory[VAL(tail)+1];
        }
        else
            throw('Invalid integer list. Last item was not NIL.');
    }
}

function integer_list_list_to_term_array(listPL) {

    let result = [];
    var head = get_arg(listPL, 0); //memory[VAL(listPL)];
    var tail = get_arg(listPL, 1); //memory[VAL(listPL)+1];
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
            head = get_arg(tail, 0); //memory[VAL(tail)];
            tail = get_arg(tail, 1); //memory[VAL(tail)+1];
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
    else gensyms[root]++;

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
        // let headString = term_to_string(head);
        // if(prolog_flag_values.wam_log === 'none' && headString.includes('location_modelx')) {
        //     prolog_flag_values.wam_log = 'local_storage_ring';
        // } else if(prolog_flag_values.wam_log !== 'none' && ! headString.includes('location_modelx')) {
        //     log(prolog_flag_values.wam_log, 'ending log at ' + headString);
        //     prolog_flag_values.wam_log = 'none';
        // }
        //
        // if(prolog_flag_values.wam_log !== 'none') {
        //     log(prolog_flag_values.wam_log, 'start ' + headString);
        // }
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

        // if(prolog_flag_values.wam_log !== 'none') {
        //     log(prolog_flag_values.wam_log, '...completed ' + headString);
        // }

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
    if (TAG(label) === TAG_STR && memory[VAL(label)] === lookup_functor("defined", 1))
    {
        add_clause_to_existing(VAL(memory[VAL(label)+1]), VAL(n) ^ 0x80000000);
        unify(l, lt);
    }
    else
    {
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
    memory[state.B+FCP_V] = value;
    return true;
}

// used in proscriptls_interpreter_terminal.js
// noinspection JSUnusedLocalSymbols
function destroy_all_choicepoints() {
    while(state.B !== 0) {
        destroy_choicepoint();
    }
}

function destroy_choicepoint()
{
    var n = memory[state.B];
    unwind_trail(memory[state.B + n + CP_TR], state.TR);
    state.B = memory[state.B + n + CP_B];
    state.HB = memory[state.B+ memory[state.B] + CP_H];
}

// For testing only! Assumes -,+ mode
function predicate_member_test(element, list)
{
    if (state.foreign_retry)
    {
        list = state.foreign_value;
    }
    else
    {
        create_choicepoint();
    }    
    while(TAG(list) === TAG_LST)
    {
        var head = get_arg(list, 0); //memory[VAL(list)];
        if (unify(head, element))
        {
            update_choicepoint_data(get_arg(list, 1)); //memory[VAL(list)+1]);
            return true;
        } else {
            // undo any bindings created by failed unify(head, element) call.
            var n = memory[state.B];
            unwind_trail(memory[state.B + n + CP_TR], state.TR);
        }
        list = get_arg(list, 1); //memory[VAL(list)+1]
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
        vars.push(get_arg(vars_list, 0)); //memory[VAL(vars_list)]);
        vars_list = get_arg(vars_list, 1); //memory[VAL(vars_list) + 1];
    }
    if (vars_list !== NIL)
        abort("Invalid list in mark_top_choicepoint");

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
            let arg = get_arg(stream, 1); //memory[VAL(stream)+1];
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
                    arg = get_arg(arg, 1); //memory[VAL(arg)+1];
                    break;
                case 'W':
                    var a = get_arg(arg, 0); //memory[VAL(arg)];
                    arg = get_arg(arg, 1); //memory[VAL(arg)+1];
                    var options = parse_term_options(get_arg(arg, 0));
                    result += format_term(a, options);
                    arg = get_arg(arg, 1); //memory[VAL(arg)+1];
                    break;
                    
                case 'i':
                    arg = get_arg(arg, 1); //memory[VAL(arg)+1];
                    break;
                case 'q':
                    result += format_term(get_arg(arg, 0), {ignore_ops:false, numbervars:true, quoted:true});
                    arg = get_arg(arg, 1); //memory[VAL(arg)+1];
                    break;
                case 'k':
                    result += format_term(memory[VAL(arg)], {ignore_ops:true, numbervars:true, quoted:true});
                    arg = get_arg(arg, 1); // memory[VAL(arg)+1];
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
                    if (!evaluate_expression(get_arg(arg, 0), e)) //memory[VAL(arg)]
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
    memory[newB] = state.num_of_args+2;
    var n = memory[newB];
    memory[newB + FCP_V] = 0;
    memory[newB + FCP_C] = {code: code,
                        offset: state.P};
    for (var i = 0; i < state.num_of_args; i++)
    {
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
            chars = get_arg(chars, 1); //memory[VAL(chars)+1];
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
            codes = get_arg(codes, 1); //memory[VAL(codes)+1];
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
function compare_terms(aBase, bBase)
{
    let a = deref(aBase);
    let b = deref(bBase);

    switch(TAG(a)) {
        case TAG_REF:
            if (TAG(b) === TAG_REF) {
                if (a === b)
                    return 0;
                else if (a > b)
                    return 1;
            }
            return -1;
        case TAG_FLT:
            if (TAG(b) === TAG_REF)
                return 1;
            if (TAG(b) === TAG_FLT) {
                if (floats[VAL(a)] === floats[VAL(b)])
                    return 0;
                else if (floats[VAL(a)] > floats[VAL(b)])
                    return 1;
            }
            return -1;
        case TAG_INT:
            if (TAG(b) === TAG_REF || TAG(b) === TAG_FLT)
                return 1;
            if (TAG(b) === TAG_INT) {
                if (VAL(a) === VAL(b))
                    return 0;
                else {
                    if (PL_get_integer(a) > PL_get_integer(b))
                        return 1;
                }
            }
            return -1;
        case TAG_ATM:
            if (TAG(b) === TAG_REF || TAG(b) === TAG_FLT || TAG(b) === TAG_INT)
                return 1;
            if (TAG(b) === TAG_ATM) {
                if (atable[VAL(a)] === atable[VAL(b)])
                    return 0;
                else if (atable[VAL(a)] > atable[VAL(b)])
                    return 1;
            }
            return -1;
        case TAG_STR:
            if (TAG(b) !== TAG_STR)
                return (TAG_STR - TAG(b)) > 0 ? 1 : -1;

            let aftor = get_arg(a, 0); //memory[VAL(a)];
            let bftor = get_arg(b, 0); //memory[VAL(b)];
            if (ftable[VAL(aftor)][1] > ftable[VAL(bftor)][1])
                return 1;
            else if (ftable[VAL(aftor)][1] < ftable[VAL(bftor)][1])
                return -1;
            // At this point the arity is equal and we must compare the functor names
            if (atable[ftable[VAL(aftor)][0]] > atable[ftable[VAL(bftor)][0]])
                return 1;
            else if (atable[ftable[VAL(aftor)][0]] < atable[ftable[VAL(bftor)][0]])
                return -1;
            // So the functors are the same and we must compare the arguments.
            for (var i = 0; i < ftable[VAL(aftor)][1]; i++) {
                let result = compare_terms(memory[VAL(a) + 1 + i], memory[VAL(b) + 1 + i]);
                if (result !== 0)
                    return result;
            }
            return 0;

        case TAG_LST: {
            if (TAG(b) !== TAG_LST)
                return (TAG_LST - TAG(b)) > 0 ? 1 : -1;

            // compare the head terms
            let result = compare_terms(memory[VAL(a)], memory[VAL(b)]);
            if (result !== 0)
                return result;
            else
            // compare the tail terms
                return compare_terms(memory[VAL(a) + 1], memory[VAL(b) + 1]);
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
            chars = get_arg(chars, 1); //memory[VAL(chars)+1];
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
            codes = get_arg(codes, 1); //memory[VAL(codes)+1];
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
                    {name:"dialect", fn:flag_dialect},
                    {name:"wam_log", fn:flag_wam_log},
                    {name:"wam_log_size", fn:flag_wam_log_size}];

var prolog_flag_values = {char_conversion: false,
                          debug: false,
                          unknown: "error",
                          double_quotes: "codes",
                          wam_log: "none",
                          wam_log_size: 50};

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

function flag_wam_log(set, value) {
    if (set)
    {
        if (TAG(value) === TAG_ATM && atable[VAL(value)] === "console")
            prolog_flag_values.wam_log = "console";
        else if (TAG(value) === TAG_ATM && atable[VAL(value)] === "local_storage")
            prolog_flag_values.wam_log = "local_storage";
        else if (TAG(value) === TAG_ATM && atable[VAL(value)] === "local_storage_ring")
            prolog_flag_values.wam_log = "local_storage_ring";
        else if (TAG(value) === TAG_ATM && atable[VAL(value)] === "none")
            prolog_flag_values.wam_log = "none";
        else
        {
            return type_error("wam_log_value", value);
        }
        return true;
    }
    return unify(value, lookup_atom(prolog_flag_values.wam_log));
}

function flag_wam_log_size(set, value)
{
    if (set) {
        if (TAG(value) === TAG_REF) {
            return instantiation_error(value);
        } else if (TAG(value) === TAG_INT) {
            prolog_flag_values.wam_log_size = VAL(value);
            return true;
        } else {
            return type_error('integer', value);
        }
    }

    return unify(value, PL_put_integer(prolog_flag_values.wam_log_size));
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
                    let moduleName = get_arg(indicator, 1); //memory[VAL(indicator) + 1];
                    slashStructure = get_arg(indicator, 2); //memory[VAL(indicator) + 2];
                    if (TAG(slashStructure) !== TAG_STR) {
                        return type_error('predicate_indicator', slashStructure);
                    }
                    name = get_arg(slashStructure, 1); //memory[VAL(slashStructure) + 1];
                    arity = get_arg(slashStructure, 2); //memory[VAL(slashStructure) + 2];

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
                    name = get_arg(indicator, 1); //memory[VAL(indicator) + 1];
                    arity = get_arg(indicator, 2); //memory[VAL(indicator) + 2];

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
// File promise.js
"use strict";

// Promise object in javascript encapsulates asynchronous
// processing. The functions in this file support the integration
// of some Promise features with ProscriptLS.
//
// The basic integration with ProscriptLS relies on foreign predicates
// that create Promise objects, Prolog terms of the form
// '$obj'(N) (with object N registered as type 'promise')
// to map to these Javascript Promise objects,
// and two foreign predicates to request results from a
// Promise object and to handle the event (callback) when
// the requested results are made available to the Javascript
// runtime environment; request_promise_results/1 and
// handle_promise_results/2, respectively.
//
// There is a utility ProscriptLS predicate promise_result/2
// that coordinates the use of request_promise_result/1 and
// handle_promise_result/2 with suspending the WAM and
// backtracking to restart the WAM:
//
// promise_result(Promise, _) :- request_result(Promise), halt.
// promise_result(Promise, Result) :- handle_result(Promise, Result).
//
// The halt/0 goal suspends the WAM (stops running it), leaving
// its evaluation state intact.
// When the callback (established by promise_request_results/1 goal) is invoked
// by the asynchronous request result it:
// - caches the result value,
// - uses the backtrack function
// to reset the WAM instruction pointer to the second clause, and
// - starts the WAM.
// This WAM restart then evaluates the promise_handle_results/2 goal
// which unifies the cached promise identifier and result value
// with the Promise and Result arguments, respectively.
//
// This mechanism can be extended to allow multiple promise_request_results/1
// goals to be evaluated prior to the halt/0 goal evaluation. In this case
// the WAM will not be resumed until all of the requested results have
// been returned or failed. Then promise_handle_results/2 may be used
// to get all of these results, e.g. setof(P-R, promise_handle_results(P,R), ResultPairs).

function create_promise_structure(promiseJS) {
    return create_object_structure(promiseJS, 'promise');
}

function get_promise_object(term, ref) {
    if(!get_object_container(term, ref)) {
        return false;
    }

    if(ref.type !== 'promise'){
        return representation_error('promise', term);
    }
    return true;
}

function predicate_request_result(promise) {
    let promiseObject = {};
    if (!get_promise_object(promise, promiseObject)) {
        return false;
    }

    let promiseJS = promiseObject.value;

    let promiseResultJS = request_result(promise, promiseJS);
    promise_requests.set(promise, promiseResultJS);
    return true;
}

async function request_result(promise, promiseJS) {
    await promiseJS.then(
        (result) => {promise_callback(promise, result);}
    );

}

var promise_requests = new Map();
var promise_results = new Map();
var promise_description = new Map();

function promise_callback(promise, result) {
    promise_results.set(promise, result);
    promise_requests.delete(promise);
    if(promise_requests.size === 0) {
        promise_backtrack();
    } else {
        // waiting on one or more requests.
    }
}

/**
 * promise_backtrack backtracks the WAM state then invokes the wam() function.
 * This is done indirectly if the proscriptls_interpreter_terminal if present.
 */
function promise_backtrack() {
    if(typeof try_backtrack === 'undefined' || !try_backtrack) {
        if (backtrack()) {
            if (!wam()) {
                throw 'promise_callback failed';
            }
        } else {
            throw 'promise_callback backtrack failed';
        }
    } else {
        // stdout('running promise_backtrack\n');
        try_backtrack();
        if (state.suspended) {
            let term = $('#proscriptinterpreter').terminal();
            setup_term_for_input(term);
        }
    }

}

var promise_results_key_array = [];
const memory_file_description = new Map();

function predicate_handle_result(promise, result) {
    if (TAG(promise) !== TAG_REF) {
        let text = promise_results.get(promise);
        if (text) {
            let promiseIDContainer = {};
            if(get_object_id_container(promise, promiseIDContainer)) {
                let description = promise_description.get(promiseIDContainer.value);
                let memory_file = create_memory_file_structure(text, description);
                //stdout('handle_result: ' + description + '\n');
                return unify(result, memory_file);
            } else {
                return representation_error('promise', promise);
            }
        } else {
            return false;
        }
    }

    let result_index = -1;
    if (state.foreign_retry) {
        result_index = state.foreign_value;

    } else {
        create_choicepoint();
        let key_iterator = promise_results.keys();
        promise_results_key_array = [];
        for (let ofst = 0; ofst < promise_results.size; ofst++) {
            promise_results_key_array.push(key_iterator.next());
        }

    }

    result_index++;

    let text;
    let memory_file;
    while(! text && result_index < promise_results_key_array.length) {
        let promise_key = promise_results_key_array[result_index];
        text = promise_results.get(promise_key);
        if (text) {
            let promiseIDContainer = {};
            if(get_object_id_container(promise_key, promiseIDContainer)) {
                let description = promise_description.get(promiseIDContainer.value);
                memory_file = create_memory_file_structure(text, description);
            } else {
                return representation_error('promise', promise_key);
            }
        } else {
            result_index++;
        }
    }

    if(result_index < promise_results_key_array.length) {
        update_choicepoint_data(result_index);
        return unify(promise, promise_key) && unify(result, memory_file);
     } else {
        destroy_choicepoint();
        return false;
    }
}

function predicate_fetch_promise(url, promise) {
    if(TAG(url) !== TAG_ATM) {
        return type_error('atom', url);
    }

    let promiseJS = fetch_promise(atable[VAL(url)]);
    let promisePL = create_promise_structure(promiseJS);
    let promiseIDContainer = {};
    if(get_object_id_container(promisePL, promiseIDContainer)) {
        promise_description.set(promiseIDContainer.value, 'fetch ' + atable[VAL(url)]);
        return unify(promise, promisePL);
    } else {
        return representation_error('promise', promisePL);
    }
}

async function fetch_promise(urlJS) {
    if (!urlJS.includes(".")) {
        urlJS += ".pl";
    }
    if(typeof document !== 'undefined') {
        const response = await fetch(urlJS);
        return response.text();
    } else if(typeof fs !== 'undefined') {
        return node_fetch(urlJS);
    } else {
        throw 'invalid environment: no "document" and no "fs" (file system module for node).';
    }
}

async function node_fetch(urlJS) {
    var options = {encoding: 'utf-8', flag: 'r'};

    return await fs.promises.readFile(urlJS, options);
}
//
// async function consult(urls, next_goal) {
//     // fetch all the URLs in parallel
//     const textPromises = urls.map(async url => {
//         if(! url.includes(".")) {
//             url += ".pl";
//         }
//         const response = await fetch(url);
//         return response.text();
//     });
//
//     // compile them in sequence
//     for (const textPromise of textPromises) {
//         await textPromise.then(function(text){
//             let index = text_to_memory_file(text);
//             return "'$memory_file'(" + index + ")";
//         }).then(function(memfile){
//             proscriptls("compile_and_free_memory_file(" + memfile + ")");
//
//         });
//     }
//
//     if(next_goal && next_goal !== '') {
//         proscriptls(next_goal);
//     }
// }
// File memory_files.js
"use strict";

/* Memory files */
var memory_files = [];

function toByteArray(str)
{
    var byteArray = [];
    for (var i = 0; i < str.length; i++)
    {
        if (str.charCodeAt(i) <= 0x7F)
        {
            byteArray.push(str.charCodeAt(i));
        }
        else
        {
            var h = encodeURIComponent(str.charAt(i)).substr(1).split('%');
            for (var j = 0; j < h.length; j++)
            {
                byteArray.push(parseInt(h[j], 16));
            }
        }
    }
    return byteArray;
}

// function JSfromByteArray(byteArray)
// {
//     var str = '';
//     for (var i = 0; i < byteArray.length; i++)
//     {
//         str +=  byteArray[i] <= 0x7F?
//                 byteArray[i] === 0x25 ? "%25" : // %
//                 String.fromCharCode(byteArray[i]) :
//                 "%" + byteArray[i].toString(16).toUpperCase();
//     }
//     return decodeURIComponent(str);
// }

function fromByteArray(byteArray)
{
    var str = '';
    for (var i = 0; i < byteArray.length; i++) {
        if (byteArray[i] <= 0x7F) {
            str += String.fromCharCode(byteArray[i]);
        }
        else {
            // Have to decode manually
            var ch = 0;
            var j = 0;
            for (var mask = 0x20; mask !== 0; mask >>=1 )
            {
                var next = byteArray[j+1];
                if (next === undefined)
                {
                    abort("Unicode break in fromByteArray. The input is garbage");
                }
                ch = (ch << 6) ^ (next & 0x3f);
                if ((byteArray[i] & mask) === 0)
                    break;
                j++;
            }
            ch ^= (ch & (0xff >> (i+3))) << (6*(i+1));
            str += String.fromCharCode(ch);
        }
    }
    return str;
}
//
// function predicate_consult(urlsPL, nextGoalPL) {
//     if(TAG(urlsPL) !== TAG_LST) {
//         return type_error("list", urlsPL);
//     }
//     let urlsJS = atom_list_to_array(urlsPL);
//     let nextGoalJS = atable[VAL(nextGoalPL)];
//     consult(urlsJS, nextGoalJS);
//     return true;
// }

function atom_list_to_array(listPL) {

    let result = [];
    var head = get_arg(listPL, 0); //memory[VAL(listPL)];
    var tail = get_arg(listPL, 1); //memory[VAL(listPL)+1];
    while (true)
    {
        if(TAG(head) !== TAG_ATM) {
            throw('Invalid atom list. Item is not an atom.');
        }

        result.push(atable[VAL(head)]);

        if (tail === NIL)
            return result;
        else if (TAG(tail) === TAG_LST)
        {
            head = get_arg(listPL, 0); //memory[VAL(tail)];
            tail = get_arg(listPL, 1); //memory[VAL(tail)+1];
        }
        else
            throw('Invalid atom list. Last item was not NIL.');
    }
}

function text_to_memory_file(text) {
    let index = memory_files.length;
    let byteArray = toByteArray(text);
    memory_files[index] = {data:byteArray, ptr:byteArray.length};
    return index;
}

function create_memory_file_structure(text, description) {
    var index = text_to_memory_file(text);
    // '$memory_file'(index)
    var ftor = lookup_functor('$memory_file', 1);
    var memory_file = alloc_structure(ftor);
    memory[state.H++] = index ^ (TAG_INT << WORD_BITS);
    let idContainer = {};
    if(get_memory_file_id_container(memory_file, idContainer, true)) {
        memory_file_description.set(idContainer.value, description);
        return memory_file;
    } else {
        return false;
    }
}

function get_memory_file_id_container(term, idContainer, reportError) {
    if (TAG(term) !== TAG_STR)
        return reportError && type_error('memory_file', term);
    var ftor = VAL(memory[VAL(term)]);
    if (atable[ftable[ftor][0]] === '$memory_file' && ftable_arity(ftor) === 1) {
        var arg = get_arg(term, 1); //memory[VAL(term) + 1];
        if (TAG(arg) !== TAG_INT)
            return reportError && type_error("memory_file arg integer", arg);
        return getIntegerPropertyValue(arg, idContainer, reportError);
    }
    return reportError && type_error('memory_file', term);
}


function predicate_memory_file_description(memory_file, description) {
    if(TAG(memory_file) === TAG_REF) {
        return instantiation_error('memory_file', memory_file);
    } else if(TAG(memory_file) !== TAG_STR) {
        return type_error('memory_file', memory_file);
    } else if(TAG(description) !== TAG_REF && TAG(description) !== TAG_ATM) {
        return type_error('description', description);
    }

    let idContainer = {};
    if(get_memory_file_id_container(memory_file, idContainer, true)) {

        let descriptionJS = memory_file_description.get(idContainer.value);
        if (descriptionJS) {
            let descriptionPL = lookup_atom(descriptionJS);
            return unify(description, descriptionPL);
        } else {
            return domain_error('memory_file with description', memory_file);
        }
    } else {
        return false;
    }
}

function atom_to_memory_file(atom, memfile)
{
    var ref = create_memory_file_structure(atable[VAL(atom)], 'atom');
    return unify(memfile, ref);
}

function memory_file_to_atom(memfile, atom)
{
    if (TAG(memfile) !== TAG_STR)
        return type_error("memory_file", memfile);
    var ftor = VAL(memory[VAL(memfile)]);
    if (atable[ftable[ftor][0]] === "$memory_file" && ftable_arity(ftor) === 1)
    {
        var f = memory_files[VAL(memory[VAL(memfile)+1])];
        return unify(atom, lookup_atom(fromByteArray(f.data)));
    }
    return type_error("memory_file", memfile);
}

function new_memory_file(memfile)
{
    var ref = create_memory_file_structure('', 'new');
    return unify(memfile, ref);
}

function close_memory_file(stream)
{
    return true;
}

function read_memory_file(stream, size, count, buffer)
{
    var bytes_read = 0;
    var records_read;
    var memfile = memory_files[stream.data];
    for (records_read = 0; records_read < count; records_read++)
    {
        for (var b = 0; b < size; b++)
        {
            var t = memfile.data[memfile.ptr++];
            if (t === undefined)
                return records_read;
            buffer[bytes_read++] = t;
        }
    }
    return records_read;
}

function write_memory_file(stream, size, count, buffer)
{
    var bytes_written = 0;
    var records_written;
    var memfile = memory_files[stream.data];
    for (records_written = 0; records_written < count; records_written++)
    {
        for (var b = 0; b < size; b++)
        {
            memfile.data[memfile.ptr++] = buffer[bytes_written++];
        }
    }
    return records_written;
}

function tell_memory_file(stream)
{
    return memory_files[stream.data].ptr;
}


function open_memory_file(memfile, mode, stream)
{
    var index = streams.length;
    let memfileContainer = {};
    if(!valid_memfile(memfile, memfileContainer)) {
        return false;
    }

    let memindex = memfileContainer.value;

    if (TAG(mode) === TAG_REF)
        return instantiation_error(mode);
    else if (TAG(mode) !== TAG_ATM)
        return type_error("atom", mode);
    if (atable[VAL(mode)] === 'read')
    {
        memory_files[memindex].ptr = 0; // start reading at the beginning of the file.
        streams[index] = new_stream(read_memory_file, null, null, close_memory_file, tell_memory_file, memindex);

    }
    else if (atable[VAL(mode)] === 'write')
    {
        memory_files[memindex].ptr = 0; // start writing at the beginning of the file.
        streams[index] = new_stream(null, write_memory_file, null, close_memory_file, tell_memory_file, memindex);
    }
    else if (atable[VAL(mode)] === 'append')
    {
        memory_files[memindex].ptr = memory_files[memindex].data.length; // start writing at the end of the file.
        streams[index] = new_stream(null, write_memory_file, null, close_memory_file, tell_memory_file, memindex);
    }
    else
        return type_error("io_mode", mode);
    var ftor = lookup_functor('$stream', 1);
    var ref = alloc_structure(ftor);
    memory[state.H++] = index ^ (TAG_INT << WORD_BITS);
    return unify(stream, ref);
}

function valid_memfile(memfile, container) {
    if (TAG(memfile) === TAG_REF)
        return instantiation_error(memfile);
    if (TAG(memfile) !== TAG_STR || memory[VAL(memfile)] !== lookup_functor("$memory_file", 1))
        return type_error("memory_file", memfile);
    var memindex = get_arg(memfile, 1);
    if (TAG(memindex) !== TAG_INT)
        return type_error("memory_file", memfile);
    container.value = VAL(memindex);
    return true;
}

function free_memory_file(memfile)
{
    var m = memory_files[VAL(get_arg(memfile, 1))];
    memory_files[m] = null;
    return true;
}

function predicate_copy_memory_file_to_local_storage(memfile, key) {
    let memfileContainer = {};
    if(!valid_memfile(memfile, memfileContainer)) {
        return false;
    }

    let memindex = memfileContainer.value;

    if (TAG(key) === TAG_REF)
        return instantiation_error(mode);
    else if (TAG(key) !== TAG_ATM)
        return type_error("atom", key);

    window.localStorage.setItem(PL_get_atom_chars(key),fromByteArray(memory_files[memindex].data) );
    return true;
}


function predicate_copy_local_storage_to_memory_file(key, memfile) {
    if(TAG(memfile) !== TAG_REF) {
        return type_error('unbound', memfile);
    }

    if (TAG(key) === TAG_REF)
        return instantiation_error(key);
    else if (TAG(key) !== TAG_ATM)
        return type_error("atom", key);

    let keyJS = PL_get_atom_chars(key);
    let item = window.localStorage.getItem(keyJS);
    if(typeof item === 'undefined' || item === null) {
        return domain_error('local_storage key', key);
    }

    let memfileCreated = create_memory_file_structure(item, 'local_storage '+keyJS);


    return unify(memfile, memfileCreated);
}
// File wam.js
"use strict";

/* For general documentation, see wam_compiler.pl

Some helpful diagrams:
Environment frame looks like this:
              -----------
    state.E ->|0  | CE  |
              |1  | CP  |
              |2  | Y0  |
              |...|     |
              |n+1| Yn  |
              -----------

Choicepoint frame where we have tried something but can try 'Next' if it fails looks like this:
(There are n arguments, labelled from A0 to An-1)

              -------------
    state.B ->|0  | n     |
              |1  | A0    |
              |2  | A1    |
              |...|       |
              |n  | An-1  |
              |n+1| E     |
              |n+2| CP    |
              |n+3| B     |
              |n+4| Next  |
              |n+5| TR    |
              |n+6| H     |
              |n+7| B0    |
              |n+8| TC    |
              |n+9| TI    |
              -------------

*/

//const E_CE = 0;
const E_CP = 1;
// const E_Y0 = 2;
// const E_Y1 = 3;
// const E_Y2 = 4;

//const CP_n = 0;
const CP_E = 1;
const CP_CP = 2;
const CP_B = 3;
const CP_Next = 4;
const CP_TR = 5;
const CP_H = 6;
const CP_B0 = 7;
const CP_TC = 8;
const CP_TI = 9;
const CP_SIZE = 10;

const FCP_V = 1; // foreign choicepoint 'value'
const FCP_C = 2; // foreign choicepoint 'code'
const FCP_R = 3; // foreign choicepoint registers (0 to n-1) start at this offset from state.B.


let ftable = [];
let dtable = [];
let atable = ['[]']; // Reserve first atom as [].
let floats = [];
let predicates = {};
let indexed_predicates = [];
let exception = null;
let itable = [];
let stable = [];
let maxStackSize = 0;
let maxHeapSize = 0;

/* Constants. Should be auto-generated */
const HEAP_SIZE = 1410700;
const STACK_SIZE = 65535;
const TRAIL_SIZE = 1000;
const READ = 0;
const WRITE = 1;
const TAG_REF = 0; // 0x00000000
const TAG_STR = 1; // 0x08000000
const TAG_LST = 2; // 0x10000000
const TAG_INT = 3; // 0x18000000
const TAG_ATM = 4; // 0x20000000
const TAG_FLT = 5; // 0x28000000
///////////// 6 is currently unused
//const TAG_EXT = 7; // Reserved!
const TAG_MASK = 7;
// 3 bits are used for the tag
// 2 bits are used for GC
// This leaves 27 for the actual value, since javascript does not have 64-bit integers
const WORD_BITS = 27;
const M_BIT = 1 << 30;
const F_BIT = 1 << 31;
const NV_MASK = M_BIT | F_BIT | (TAG_MASK << WORD_BITS);

const NIL = (TAG_ATM << WORD_BITS); // atable[0] = '[]', so NIL is 0 xor TAG_ATM, which is just TAG_ATM.
const FAIL_ADDRESS = 1000000; // used in switch_on_term to recognize an address that 'fails'.

let memory = new Array(HEAP_SIZE + STACK_SIZE + TRAIL_SIZE);
let code = [255];
let register = new Array(256);
let state;

// Stack for managing cleanup handlers needed during a cut
let cleanups = [];

let bootstrap_code; // 'defined' by load_state() in proscriptls_state.js.
let retry_foreign_offset;  // 'defined' by load_state() in proscriptls_state.js.
let foreign_predicates; // 'defined' by load_state() in proscriptls_state.js.
let system;  // 'defined' by load_state() in proscriptls_state.js.
let initialization;  // 'defined' by load_state() in proscriptls_state.js.
let module_exports;  // 'defined' by load_state() in proscriptls_state.js.
let module_imports;  // 'defined' by load_state() in proscriptls_state.js.
let meta_predicate_signatures;  // 'defined' by load_state() in proscriptls_state.js.

/* Special purpose machine registers:

   P: Pointer to the next opcode to execute (in the code[] array)
  CP: Continuation Pointer. Points to the next code to execute if the current environment succeeds (basically the return address when calling a function)
mode: READ or WRITE depending on whether are matching or creating an exemplar on the heap
   H: Pointer to the next available heap cell
  HB: Pointer to where the heap should be truncated to if we backtrack
  TR: Pointer to the next available trail cell
   S: Pointer to the next symbol on the heap to match when unifying
   E: Pointer to the top environment frame   
   B: Pointer to the top choicepoint
  B0: Pointer to the choicepoint to return to after backtracking over a cut (ie the choicepoint created by the most recent call opcode)


  It is important to note that B and E do not point to the *next available* place to put an environment frame or choicepoint, but the *current* one.
*/
let debugging = false;

// debug_msg function may not be called due to js_preprocess.js removing references to it.
// noinspection JSUnusedLocalSymbols
function debug_msg(msg)
{
    if (debugging)
        debug(msg);
}

function initialize()
{
    let trace_ftor = VAL(lookup_functor('debugger:$traceR', 3));
    let trace_predicate = predicates[trace_ftor];
    let key = (trace_predicate.index) ? trace_predicate.index : trace_predicate.clause_keys[0];
    let trace_code = trace_predicate.clauses[key].code;

    state = {H: 0,
             HB: 0,
             S: 0,
             P: 2,             
             CP: {code: bootstrap_code, 
                  predicate: null,
                  offset:1}, // halt
             B0: 0, // No backtrack frame
             B: 0,  // No backtrack frame
             E: HEAP_SIZE,
             TR: HEAP_SIZE + STACK_SIZE,
             mode: READ,
             running: false,
             foreign_retry: false,
             num_of_args: 0,
             current_predicate: null,
             trace_info: NIL,
             trace_call: 'no_trace',
             trace_identifier: 0,
             trace_predicate: trace_predicate,
             trace_code: trace_code,
             trace_prompt: '>',
             suspended: false,
             wamYielded: false};
    code = bootstrap_code;
    cleanups = [];
}

function abort(why)
{        
    debug(why);
    throw why;
}

function bind(a, b)
{
    if (TAG(a) === TAG_REF && (TAG(b) !== TAG_REF || VAL(b) < VAL(a)))
    {
        memory[VAL(a)] = b;
        trail(a);
    }
    else
    {
        memory[VAL(b)] = a;
        trail(b);
    }
}

function tidy_trail()
{
    let t = memory[state.B + memory[state.B] + CP_TR];
    if (t < HEAP_SIZE + STACK_SIZE)
        abort("Backtrack pointer " + state.B + " has garbage for TR: " + hex(t));
    while (t < state.TR)
    {
        if ((memory[t] < state.HB) || (state.H < memory[t] && memory[t] < state.B))
        {
            // This trailing is still required
            t = t + 1;
        }
        else
        {
            memory[t] = memory[state.TR - 1];
            state.TR = state.TR - 1;
        }
    }   
}

function trail(v)
{
    if (v < state.HB || (state.H < v && v < state.B))
    {
        memory[state.TR++] = v;
    }
    else
    {
    }
}

function unwind_trail(from, to)
{
    for (let i = from; i < to; i++)
    {
        memory[memory[i]] = memory[i] ^ (TAG_REF << WORD_BITS);
    }
}

// Returns boolean
function unify(a, b)
{
    let PDL = [];

    PDL.push(a);
    PDL.push(b);
    let failed = false;
    while (PDL.length !== 0 && !failed)
    {
        let d1 = deref(PDL.pop());
        let d2 = deref(PDL.pop());
        // if d1 == d2 then just proceed with the rest of the PDL. Otherwise we need to try and unify them, or fail
        if (d1 !== d2)
        {
            let type1 = TAG(d1);
            let val1 = VAL(d1);
            let type2 = TAG(d2);
            let val2 = VAL(d2);
            if (type1 === TAG_REF)
            {
                bind(d1, d2);
            }
            else
            {
                switch(type2)
                {
                case TAG_REF:
                    bind(d1, d2);
                    break;
                case TAG_ATM:
                case TAG_INT:
                    failed = true;
                    break;
                case TAG_FLT:
                    if (type1 === TAG_FLT)
                    {
                        debug(floats[val1] + " vs " + floats[val2]);
                    }
                    failed = true;
                    break;
                case TAG_LST:
                    if (type1 === TAG_LST)
                    {                        
                        PDL.push(memory[val1]); // unify heads
                        PDL.push(memory[val2]);
                        PDL.push(memory[val1+1]); // unify tails
                        PDL.push(memory[val2+1]);
                    }
                    else
                        failed = true; // list and non-list
                    break;
                case TAG_STR:
                    if (type1 === TAG_STR)
                    {
                        let f1 = VAL(memory[val1]);
                        let f2 = VAL(memory[val2]);
                        if (f1 === f2)
                        {
                            for (let i = 0; i < ftable[f1][1]; i++)
                            {
                                PDL.push(val1 + 1 + i);
                                PDL.push(val2 + 1 + i);
                            }
                        }
                        else
                            failed = true; // different functors
                    }
                    else
                        failed = true; // str and atom/list
                }
            }
        }
    }
    return !failed;
}

function deref(p)
{
    while(TAG(p) === TAG_REF && VAL(p) !== memory[VAL(p)])
    {
        let q = memory[VAL(p)];
        if (q === undefined) // FIXME: Check that q =< p?
        {
            abort("Bad memory access: @" + p);
        }
        else
            p = q;
    }
    return p;
}

// This should be a macro
/**
 * @return {number}
 */
function TAG(p)
{
    // >>> is unsigned-right-shift. Nice.
    return (p >>> WORD_BITS) & TAG_MASK;
}

// This should be a macro
/**
 * @return {number}
 */
function VAL(p)
{
    return p & ((1 << WORD_BITS)-1);
}

function ftable_arity(ftor) {
    if(ftable[ftor] === undefined) {
        throw('no ftable entry at ' + ftor);
    }
    return ftable[ftor][1];
}

// Ideally this would be inlined, but javascript does not support macros. Ultimately this could be generated dynamically.
function backtrack()
{    
    if (state.B <= HEAP_SIZE)
    {
        return false;
    }
    state.B0 = memory[state.B + memory[state.B] + CP_B0];
    // Also unwind any trailed bindings
    unwind_trail(memory[state.B + memory[state.B] + CP_TR], state.TR);
    let next = memory[state.B + memory[state.B] + CP_Next];
    state.P = next.offset;
    code = next.code;
    if(! code) {
        throw 'code is undefined';
    }

    state.current_predicate = next.predicate;
    if(state.current_predicate) {
        state.num_of_args = ftable[state.current_predicate.key][1];
    }

    if(state.trace_call !== 'no_trace') {
        let traceCallPL = memory[state.B + memory[state.B] + CP_TC];
        state.trace_call = atable[VAL(traceCallPL)];
        if(! state.trace_call) {
            throw 'backtrack trace_call is undefined';
        }
        state.trace_info = memory[state.B + memory[state.B] + CP_TI];
    }
    return true;
}

function predicate_get_backtrack_frame(B) {
    let term = PL_put_integer(state.B);
    //stdout('Backtrack frame = ' + VAL(term) + '\n');
    return unify(B, term);
}

function predicate_set_backtrack_frame(B) {
    //let type = TAG(B);
    state.B = VAL(B);
    //stdout('Backtrack frame set to ' + state.B + '\n');
    return true;
}

// Returns a <STR, f/n> cell. This MUST be followed (eventually) by n args. Attempting to print the term (or otherwise use) the term before then will result in chaos
// ftor must have the ATM tag!
function alloc_structure(ftor)
{
    let tmp = state.H;
    memory[state.H++] = ftor;
    if(state.H > maxHeapSize) {
        maxHeapSize = state.H;
    }
    return tmp ^ (TAG_STR << WORD_BITS);
}

function alloc_var()
{
    let result = state.H ^ (TAG_REF << WORD_BITS);
    memory[state.H] = result;    
    state.H++;
    if(state.H > maxHeapSize) {
        maxHeapSize = state.H;
    }
    return result;
}

function alloc_list()
{
    let result = (state.H+1) ^ (TAG_LST << WORD_BITS);
    memory[state.H] = result;    
    state.H++;
    if(state.H > maxHeapSize) {
        maxHeapSize = state.H;
    }
    return result;
}

function wam_setup_trace_call(target_ftor_ofst) {
    // Create a 'traceArgStructure' for 'X(A0, ..., An-1)', copying
    // args A0 through An from register[0] to register[n-1]
    // where n = arity of predicate.

    let traceArgArity = ftable[target_ftor_ofst][1];
    if(traceArgArity === 0) {
        register[0] = (ftable[target_ftor_ofst][0]) ^ (TAG_ATM << WORD_BITS);
    } else {
        let target_ftor = target_ftor_ofst ^ (TAG_ATM << WORD_BITS);
        let traceArgStructure = alloc_structure(target_ftor);
        let argOfst = 0;
        for (; argOfst < traceArgArity; argOfst++) {
            memory[state.H++] = register[argOfst];
        }
        if(state.H > maxHeapSize) {
            maxHeapSize = state.H;
        }

        // Make the traceArgStructure the first argument.
        // The info term is the second argument. It is set
        // by '$trace' before evaluating call/.

        register[0] = traceArgStructure;
    }
    register[1] = state.trace_info;
    register[2] = PL_put_integer(state.trace_identifier);
    return traceArgArity;
}

let call_log = [];
function add_to_call_log(msg) {
   let currentPredicateString = (state.current_predicate == null)
       ?("no predicate")
       :(atable[ftable[state.current_predicate.key][0]] + "/" + ftable[state.current_predicate.key][1]);

    if(call_log.length > 100) {
        call_log = call_log.slice(1);
    }
    call_log.push(currentPredicateString + ": " + msg);
}

function wam_complete_call_or_execute(predicate) {
   if (predicate.clauses && predicate.clause_keys && predicate.clause_keys.length > 0
           && predicate.clauses[predicate.clause_keys[0]]) {
        //stdout("Complete " + atable[ftable[code[state.P + 1]][0]] + "/" + ftable[code[state.P + 1]][1] + '\n');
//        add_to_call_log(atable[ftable[code[state.P + 1]][0]] + "/" + ftable[code[state.P + 1]][1]);
        state.B0 = state.B;
        state.num_of_args = ftable[code[state.P + 1]][1];
        state.current_predicate = predicate;
        let key = (predicate.index) ? predicate.index : predicate.clause_keys[0];
        code = predicate.clauses[key].code;
       if(! code) {
           throw 'code is undefined';
       }

       state.P = 0;
       return true;
    } else {
        return false;
    }
}

function wam_setup_and_call_foreign() {
//    add_to_call_log(atable[ftable[code[state.P + 1]][0]] + "/" + ftable[code[state.P + 1]][1]);

    state.num_of_args = ftable[code[state.P+1]][1];
    let args = new Array(state.num_of_args);
    for (let i = 0; i < state.num_of_args; i++)
        args[i] = deref(register[i]);
    let result = foreign_predicates[code[state.P+1]].apply(null, args);
    state.foreign_retry = false;
    return result;
}


function wam_create_choicepoint(nextCP, prefix) {
    // 'n' (memory[newB]) is the number of slots of the dynamic initial portion of the choicepoint frame.
    // The choicepoint frame starting at memory[newB + n] is a fixed size, CP_SIZE, where
    // each slot i at memory[newB + n + i] has a fixed interpretation and a constant name
    // of the form CP_*. E.g. CP_TR is the 'trail' slot at memory[newB + n + CP_TR].
    //
    // The dynamic portion of the choicepoint frame for foreign calls
    // starts with a 'value' slot (at FCP_V == 1) and a 'code' slot (at FCP_C == 2).
    // This is followed (at FCP_R == 3) by a slot for each 'register' to be saved (generally
    // one register per predicate argument), as indicated by state.num_of_args.
    // The choicepoint frame for 'standard' (non-foreign) calls does not
    // have the initial two slots: the saved registers start at slot CP_R = 1.

    let newB;
    if (state.E > state.B) {
        // In this case, it is an environment. In the real WAM, which does stack trimming (see Ait-Kaci chapter 5.7), we only have CE, CP and then N saved Y-registers.
        // Therefore, we need to write the new choicepoint at 2 + N. What is N, though? Well, it turns out N gets gradually smaller as time goes on, which
        // is why it is not stored in the frame itself. If call(f) is outfitted with a second argument to become call(f, n) then we can decode this in try_me_else
        // (and ignore it if we did not want to create a new environment) by looking at CP, which points to the instruction after the call() opcode. Therefore,
        // code[CP-1] ought to be N.

        // -----------
        // |0  | CE  |
        // |1  | CP  |
        // |3  | Y0  |
        //  ...
        // |n+2| Yn  |
        // -----------
        newB = state.E + state.CP.code[state.CP.offset - 1] + 2; // this is corrected according to Ait-Kaci wamerratum.txt.
    } else {
        // In this case, the top frame is a choicepoint. This is a bit easier: A choicepoint contains 7 saved special-purpose registers, the N root arguments
        // for the goal, and, happily, the value of N as the very first argument. Therefore, we can read the 0th entry of the current frame (at state.B)
        // and add CP_SIZE (=10) to it to get the top of the stack.
        newB = state.B + memory[state.B] + CP_SIZE;
    }

    memory[newB] = state.num_of_args + prefix.length;
    let n = memory[newB];
    for (let prefixOfst = 0; prefixOfst < prefix.length; prefixOfst++) {
        memory[newB + prefixOfst + 1] = prefix[prefixOfst];
    }
    let prefixAdjust = prefix.length + 1;

    for (let i = 0; i < state.num_of_args; i++) {
        memory[newB + prefixAdjust + i] = register[i];
    }
    // Save the current context
    memory[newB + n + CP_E] = state.E;
    memory[newB + n + CP_CP] = state.CP;
    memory[newB + n + CP_B] = state.B;
    memory[newB + n + CP_Next] = nextCP;
    memory[newB + n + CP_TR] = state.TR;
    memory[newB + n + CP_H] = state.H;
    memory[newB + n + CP_B0] = state.B0;
    memory[newB + n + CP_TC] = lookup_atom(state.trace_call);
    memory[newB + n + CP_TI] = state.trace_info;
    state.B = newB;
    state.HB = state.H;

    let stackTop = newB + n + CP_SIZE - 1;

    if(maxStackSize < stackTop) {
        maxStackSize = stackTop;
    }
}

function wam_trace_call_or_execute(functor) {
    return ! functor.startsWith('debugger:') && ! functor.startsWith('system:$trace') &&
        ! state.foreign_retry &&
        functor !== 'true' && functor !== 'system:true' && state.trace_predicate &&
        (state.trace_call === 'trace' || state.trace_call === 'leap_trace');
}

function wam_suspend_trace() {
    if(state.trace_call === 'trace') {
        state.trace_call = 'skip_trace';
    } else if (state.trace_call === 'leap_trace') {
        state.trace_call = 'suspend_leap_trace';
    } else {
        abort("The state.trace_call register has an invalid value: '"
            + state.trace_call + "'. It must be either 'trace' or 'leap_trace'");
    }

    // ensure instruction tracing is disabled.
    state.trace_instruction = 'no_trace';
    state.trace_instruction_prompt = undefined;
    instruction_suspend_set('false');

}

function wam_advance_next_trace_conditionally() {
    // 'trace_next' and 'leap_trace_next' only occur when call/1 is invoked by '$trace'.
    if (state.trace_call === 'trace_next') {
        state.trace_call = 'trace';
    } else if (state.trace_call === 'leap_trace_next') {
        state.trace_call = 'leap_trace';
    }
}

let wamDuration = 0;
let activeWamStartTime = undefined;
let wamNestedInvocations = 0;

function wamEntrance() {
    wamNestedInvocations++;
    if(wamNestedInvocations === 1) {
        activeWamStartTime = Date.now();
        //stdout('start: ' + activeWamStartTime + '\n');
    } else {
        //stdout('start nested: ' + wamNestedInvocations+ '\n');
    }
}

function wamExit(result) {
    wamNestedInvocations--;
    if(wamNestedInvocations === 0) {
        let wamTimeExit = Date.now();
        let duration = Math.max(wamTimeExit - activeWamStartTime, 0.1);
        wamDuration += duration;
        activeWamStartTime = undefined;
        //stdout('exit: ' + wamTimeExit + ', duration: ' + duration + '\n');
    } else {
        //stdout('exit nested: ' + wamNestedInvocations + '\n');
    }
    return result;
}

let reportedLargeStack = false;

function wamValidStackAddr(description, tmpE, instruction) {
    if (tmpE < HEAP_SIZE ) {
        abort("The " + description + " " + tmpE + " is less than minimum stack address (HEAP_SIZE=" + HEAP_SIZE + ") in " + instruction + " instruction.");
    } else if (tmpE > HEAP_SIZE + STACK_SIZE) {
        abort("The " + description + " " + tmpE + " is greater than maximum stack address (HEAP_SIZE+STACK_SIZE="+HEAP_SIZE+"+"+STACK_SIZE+"="+(HEAP_SIZE+STACK_SIZE) + ") in " + instruction + " instruction.");
    } else if (! reportedLargeStack && tmpE > HEAP_SIZE + 0.75 * STACK_SIZE ) {
        dumpWrite('Large stack (' + description + '): ' + tmpE);
        dump_environments();
        dump_choicepoints();
        reportedLargeStack = true;
    } else if (reportedLargeStack && tmpE <  HEAP_SIZE + 0.70 * STACK_SIZE ) {
        reportedLargeStack = false;
    }
}

function wamValidStackVarAddr(description, varID, tmpE, instruction) {
    if (tmpE < HEAP_SIZE ) {
        dump_environments();
        abort("The " + description + " " + varID + ' ' + tmpE + " is less than minimum stack address (HEAP_SIZE=" + HEAP_SIZE + ") in " + instruction + " instruction.");
    } else if (tmpE > HEAP_SIZE + STACK_SIZE) {
        dump_environments();
        dump_choicepoints();
        abort("The " + description + " " + varID + ' ' + tmpE + " is greater than maximum stack address (HEAP_SIZE+STACK_SIZE="+HEAP_SIZE+"+"+STACK_SIZE+"="+(HEAP_SIZE+STACK_SIZE) + ") in " + instruction + " instruction.");
    } else if (! reportedLargeStack && tmpE > HEAP_SIZE + 0.75 * STACK_SIZE ) {
        dumpWrite('Large stack (' + description + ' ' + varID + '): ' + tmpE);
        dump_environments();
        dump_choicepoints();
        reportedLargeStack = true;
    } else if (reportedLargeStack && tmpE <  HEAP_SIZE + 0.70 * STACK_SIZE ) {
        reportedLargeStack = false;
    }
}

function wamValidHeapOrStackAddr(description, tmpE, instruction) {
    if (tmpE < 0 ) {
        abort("The " + description + " " + tmpE + " is less than minimum heap+stack address (0) in " + instruction + " instruction.");
    } else if (tmpE > HEAP_SIZE + STACK_SIZE) {
        abort("The " + description + " " + tmpE + " is greater than maximum heap+stack address (HEAP_SIZE+STACK_SIZE="+HEAP_SIZE+"+"+STACK_SIZE+"="+(HEAP_SIZE+STACK_SIZE) + ") in " + instruction + " instruction.");
    } else if (! reportedLargeStack && tmpE > HEAP_SIZE + 0.75 * STACK_SIZE ) {
        dumpWrite('Large stack (' + description + '): ' + tmpE);
        dump_environments();
        dump_choicepoints();
        reportedLargeStack = true;
    } else if (reportedLargeStack && tmpE <  HEAP_SIZE + 0.70 * STACK_SIZE ) {
        reportedLargeStack = false;
    }
}

function wamValidHeapAddr(description, addr, instruction) {
    if (addr < 0 ) {
        abort("The " + description + " " + addr + " is less than minimum heap address (0) in " + instruction + " instruction.");
    } else if (addr >= HEAP_SIZE) {
        abort("The " + description + " " + addr + " is equal to or greater than maximum heap address (HEAP_SIZE="+HEAP_SIZE + ") in " + instruction + " instruction.");
    }
}

function wam() {
    try {
        return wam1();
    } catch (e) {
        wamExit(e);
        dump_environments();
        dump_choicepoints();
        throw e;
    }
}

function wam1()
{
    let predicate;
    let source;
    let sym;
    let arg;
    let offset;
    let functor;

    wamEntrance();

    state.running = true;
    while (state.running)
    {

        // The conditional "prolog_flag_values.wam_log !== 'none'" avoids the call to log() in the common case.
        // This makes a noticeable difference in performance.

        if(prolog_flag_values.wam_log !== 'none')
            log(prolog_flag_values.wam_log, decode_instruction(state.current_predicate, state.P).string);

        if(state.trace_call === 'trace' && state.trace_instruction &&
            (state.trace_instruction === 'trace' || state.trace_instruction === 'step')) {
            let instruction = decode_instruction(state.current_predicate, state.P);
            if (state.trace_instruction === 'step') {
                // set up the prompt to be displayed before reading a command character into
                // input_buffer
                state.trace_instruction_prompt = instruction.string;
                let char = get_terminal_char();
                if (char) {
                    if (char === 'm') {
                        // show internal debug info for current instruction (if DEBUG===true in Makefile)
                        debugging = true;
                        // break at next instruction
                        state.trace_instruction = 'step';
                    } else if (char === 'x') {
                        // do not show internal debug info for current instruction
                        debugging = false;
                        // break at next instruction
                        state.trace_instruction = 'step';
                    } else if (char === 'y') {
                        // do not show internal debug info for current and subsequent instructions
                        debugging = false;
                        // do not break at next instruction
                        state.trace_instruction = 'trace';
                    } else if (char === 'z') {
                        // show internal debug info for current and subsequent instructions (if DEBUG===true in Makefile)
                        debugging = true;
                        // do not break at next instruction
                        state.trace_instruction = 'trace';
                    } else {
                        stdout('Invalid command ' + char + '. Command must be "m", "x", "y", or "z".\n');
                        instruction_suspend_set('true');
                        return wamExit(true);
                    }
                } else {
                    // No command character was available,
                    // return to the wam caller (jquery terminal) where the
                    // current prompt will be displayed (showing the current instruction)
                    // the user will enter a command character,
                    // the wam caller (jquery terminal) calls the wam again with the same
                    // wam state as was present earlier so that the re-called wam
                    // can continue the evaluation.
                    instruction_suspend_set('true');
                    return wamExit(true);
                }
            } else {
                stdout(instruction.string + '\n');
            }
        } else {
           debugging = false;
        }

        if(! code) {
            throw 'code is undefined';
        }
        // Decode an instruction
        switch(code[state.P]) {
            case 1: // allocate
            {
                let tmpE;
                if (state.E > state.B) {
                    let nextEnvironmentOfst = state.CP.code[state.CP.offset - 1] + 2; // this is as corrected by Ait-Kaci in wamerratum.txt.
                    // if(nextEnvironmentOfst > 10000) {
                    //     dump_environments();
                    //     dump_choicepoints();
                    //     gcWrite('very large environment. size='+ nextEnvironmentOfst);
                    // }
                    tmpE = state.E + nextEnvironmentOfst;
                } else {
                    tmpE = state.B + memory[state.B] + CP_SIZE;
                }

                // if(tmpE > HEAP_SIZE + 60000) {
                //     dump_environments();
                //     dump_choicepoints();
                //     gcWrite('environment for large stack. new E='+tmpE);
                // }

                if (tmpE === undefined || isNaN(tmpE))
                    abort("Top of frame is garbage: " + tmpE);

                wamValidStackAddr('CP of new environment frame', tmpE+1, 'allocate');

                // Save old environment and continuation
                memory[tmpE] = state.E;
                memory[tmpE + 1] = state.CP;
                state.E = tmpE;
                state.P += 1;

                if(maxStackSize < tmpE+1) {
                    maxStackSize = tmpE+1;
                }

            }
                continue;

            case 2: // deallocate
                state.CP = memory[state.E + E_CP];
                wamValidStackAddr('previous base of environment frame', memory[state.E], 'deallocate');

                state.E = memory[state.E];
                state.P += 1;
                continue;

            case 3: // call

                functor = atable[ftable[code[state.P + 1]][0]];

                if (wam_trace_call_or_execute(functor)) {
                    // Trace this call of X(...).
                    // Suspend tracing to prevent the trace mechanism from tracing itself.
                    state.CP = {
                        code: code,
                        predicate: state.current_predicate,
                        offset: state.P + 3
                    };
                    state.B0 = state.B;

                    wam_suspend_trace();


                    state.trace_identifier++;

                    let target_ftor_ofst = code[state.P + 1];
                    wam_setup_trace_call(target_ftor_ofst);


                    state.num_of_args = 3;
                    state.current_predicate = state.trace_predicate;
                    code = state.trace_code;
                    state.P = 0;
                } else {
                    wam_advance_next_trace_conditionally();

                    predicate = predicates[code[state.P + 1]];
                    if (predicate !== undefined) {
                        // Set CP to the next instruction so that when the predicate is finished executing we know where to come back to
                        state.CP = {
                            code: code,
                            predicate: state.current_predicate,
                            offset: state.P + 3
                        };

                        //stdout("Calling " + atable[ftable[code[state.P + 1]][0]] + "/" + ftable[code[state.P + 1]][1] + '\n');
                        let result =
                            wam_complete_call_or_execute(predicate);
                        if (!result && !backtrack()) {
                            return wamExit(false);
                        }
                    } else if (foreign_predicates[code[state.P + 1]] !== undefined) {
                        // This is a bit counter-intuitive since it seems like we are never going to get a proceed to use the CP.
                        // Remember that any time we might need CP to be saved, it will be. (If there is more than one goal, there will be an environment).
                        // If there is only one goal (ie a chain rule) then we will be in execute already, not call.
                        // This means it is never unsafe to set CP in a call port.
                        // Further, remember that state.CP is used to create choicepoints (and environments), and since foreign frames may create these, we must set CP to
                        // something sensible, even though we never expect to use it to actually continue execution from.
                        state.CP = {
                            code: code,
                            predicate: state.current_predicate,
                            offset: state.P + 3
                        };
                        //stdout("Calling (foreign) " + atable[ftable[code[state.P+1]][0]] + "/" + ftable[code[state.P+1]][1] + '\n');
                        let result = wam_setup_and_call_foreign();
                        if (result)
                            state.P = state.P + 3;
                        else if (!backtrack()) {
                            return wamExit(false);
                        }
                    } else {
                        if (!undefined_predicate(code[state.P + 1]))
                            return wamExit(false);
                    }
                }
                continue;

            case 4: // execute
                functor = atable[ftable[code[state.P + 1]][0]];

                if (wam_trace_call_or_execute(functor)) {
                    wam_suspend_trace();

                    state.trace_identifier++;
                    let target_ftor = code[state.P + 1];
                    wam_setup_trace_call(target_ftor);

                    state.B0 = state.B;
                    state.num_of_args = 3;
                    state.current_predicate = state.trace_predicate;
                    code = state.trace_code;
                    state.P = 0;
                } else {
                    wam_advance_next_trace_conditionally();

                    predicate = predicates[code[state.P + 1]];

                    if (predicate !== undefined) {
                        // No need to save continuation for execute

                        //stdout("Executing " + atable[ftable[code[state.P+1]][0]] + "/" + ftable[code[state.P+1]][1] + '\n');
                        let result =
                            wam_complete_call_or_execute(predicate);
                        if (!result && !backtrack()) {
                            return wamExit(false);
                        }
                    } else if (foreign_predicates[code[state.P + 1]] !== undefined) {
                        //stdout("Executing (foreign) " + atable[ftable[code[state.P+1]][0]] + "/" + ftable[code[state.P+1]][1] + '\n');
                        let result = wam_setup_and_call_foreign();
                        if (result) {
                            state.current_predicate = state.CP.predicate;
                            code = state.CP.code;
                            if (!code) {
                                throw 'code is undefined';
                            }

                            state.P = state.CP.offset;
                        } else if (!backtrack())
                            return wamExit(false);
                    } else {
                        if (!undefined_predicate(code[state.P + 1]))
                            return wamExit(false);
                    }
                }
                continue;

            case 5: // proceed
                state.P = state.CP.offset;
                state.current_predicate = state.CP.predicate;
                if(state.current_predicate) {
                    state.num_of_args = ftable[state.current_predicate.key][1];
                }
                code = state.CP.code;
                if (!code) {
                    throw 'code is undefined';
                }

                continue;

            case 6: // put_variable: Initialize a new variable in Yn, and also put it into Ai
            {
                let varID = code[state.P + 1];
                let register_location6 = state.E + varID + 2;
                wamValidStackAddr('environment variable ' + code[state.P + 1], register_location6, 'put_variable');
                memory[register_location6] = register_location6 ^ (TAG_REF << WORD_BITS);
                register[code[state.P + 2]] = register_location6 ^ (TAG_REF << WORD_BITS);
                state.P += 3;
                // noinspection UnnecessaryContinueJS
                continue;
            }

            case 7: // put_variable: Put fresh var into registers Ai and Xn
            {

                let freshvar = state.H ^ (TAG_REF << WORD_BITS);
                wamValidHeapAddr('heap top', state.H, 'put_variable');
                memory[state.H] = freshvar;
                register[code[state.P + 1]] = freshvar;
                register[code[state.P + 2]] = freshvar;
                state.H++;
                if(state.H > maxHeapSize) {
                    maxHeapSize = state.H;
                }
                state.P += 3;
            }
            continue;

        case 8: // put_value
            if (code[state.P+1] === 0) // Y-register
            {
                let register_location8 = state.E + code[state.P+2] + 2;
                wamValidStackVarAddr('read stack var Y', code[state.P+2], register_location8, 'put_value');

                if (memory[register_location8] === undefined)
                    abort("Invalid memory access in put_value");
                register[code[state.P+3]] = memory[register_location8];
            }
            else
            {
                register[code[state.P+3]] = register[code[state.P+2]];
            }
            state.P += 4;
            continue;

        case 9: // put_unsafe_value
        {
            let register_location9 = state.E + code[state.P + 1] + 2;
            // This is the unsafe bit. If the value now in register[code[state.P+2]] is on the stack (that is, it is > E) then we have to push a new variables
            // onto the stack to avoid dangling references to things that are about to be cleaned up
            wamValidStackVarAddr('read stack var Y', code[state.P+1], register_location9, 'put_unsafe_value');
            if (memory[register_location9] < state.E) {
                // No, so we can just behave like put_value
                register[code[state.P + 2]] = deref(memory[register_location9])
            } else {
                // Yes, so we need to push a new variable instead
                let v = alloc_var();
                bind(v, memory[register_location9]);
                register[code[state.P + 2]] = v;
            }
            state.P += 3;
            // noinspection UnnecessaryContinueJS
            continue;
        }
        case 10: // put_constant C into Ai            
            register[code[state.P+2]] = code[state.P+1] ^ (TAG_ATM << WORD_BITS);
            state.P += 3;
            continue;

        case 11: // put_nil into Ai
            register[code[state.P+1]] = NIL;
            state.P += 2;
            continue;

        case 12: // put_structure
            register[code[state.P+2]] = alloc_structure(code[state.P+1] ^ (TAG_ATM << WORD_BITS));
            state.mode = WRITE;
            state.P += 3;
            continue;

        case 13: // put_list
            register[code[state.P+1]] = alloc_list();
            state.mode = WRITE;
            state.P += 2;
            continue;

        case 14: // put_integer I into Ai
            register[code[state.P+2]] = (code[state.P+1] & ((1 << WORD_BITS)-1)) ^ (TAG_INT << WORD_BITS);
            state.P += 3;
            continue;           

        case 15: // get_variable
            if (code[state.P+1] === 0) // Y-register
            {
                let register_location15 = state.E + code[state.P+2] + 2;
                wamValidStackVarAddr('write stack var Y', code[state.P+2], register_location15, 'put_unsafe_value');
                memory[register_location15] = register[code[state.P+3]];
            }
            else
            {
                register[code[state.P+2]] = register[code[state.P+3]];
            }
            state.P+= 4;
            continue;
            
        case 16: // get_value
        {
            let target = register[code[state.P + 3]];
            gc_check(target);
            if (code[state.P + 1] === 0) // Y-register
            {
                let register_location16 = state.E + code[state.P + 2] + 2;
                wamValidStackVarAddr('read stack var Y', code[state.P+2], register_location16, 'get_value');
                source = memory[register_location16];
            } else {
                source = register[code[state.P + 2]];
            }
            state.P += 4;
            if (!unify(source, target))
                if (!backtrack())
                    return wamExit(false);
        }
            continue;

        case 17: // get_constant C from Ai            
            // First, get what is in Ai into sym
            sym = deref(register[code[state.P+2]]);
            // Then get arg. This is an atom index, not a <CON, i> cell. It needs to be made into the latter!
            arg = code[state.P+1] ^ (TAG_ATM << WORD_BITS);
            state.P += 3;
            if (TAG(sym) === TAG_REF)
            {
                // If Ai is variable, then we need to bind. This is when foo(bar) is called like foo(X).
                bind(sym, arg);
            }
            else if (sym !== arg)
            {
                if (!backtrack())
                    return wamExit(false);
            }
            continue;

        case 18: // get_nil
            sym = deref(register[code[state.P+1]]);
            state.P += 1;
            if (TAG(sym) === TAG_REF)
                bind(sym, NIL);
            else if (sym !== NIL)
                if (!backtrack())
                    return wamExit(false);
            continue;
            

        case 19: // get_structure
        {
            let structure_ftor = code[state.P + 1] ^ (TAG_ATM << WORD_BITS);
            let addr = deref(register[code[state.P + 2]]);
            state.P += 3;
            if (TAG(addr) === TAG_REF) {
                state.mode = WRITE;
                let a = alloc_structure(structure_ftor);
                wamValidHeapOrStackAddr('bind addr', addr, 'get_list');
                bind(memory[addr], a);
            } else if (TAG(addr) === TAG_STR && memory[VAL(addr)] === structure_ftor) {
                state.mode = READ;
                state.S = VAL(addr) + 1;
            } else {
                if (!backtrack())
                    return wamExit(false);
            }
        }
            continue;

        case 20: // get_list from Ai
        {
            let addr = deref(register[code[state.P + 1]]);
            state.P += 2;
            if (TAG(addr) === TAG_REF) {
                // predicate called with var and we are expecting a list
                let l = state.H ^ (TAG_LST << WORD_BITS);
                wamValidHeapOrStackAddr('bind addr', addr, 'get_list');
                bind(memory[addr], l);
                state.mode = WRITE;
            } else if (TAG(addr) === TAG_LST) {
                state.S = VAL(addr);
                state.mode = READ;
            } else if (!backtrack())
                return wamExit(false);
        }
            continue;

        case 21: // get_integer I from Ai            
            // First, get what is in Ai into sym
            sym = deref(register[code[state.P+2]]);
            // Then get arg. This is the actual integer, not a <INT, i> cell. It needs to be made into the latter!
            arg = (code[state.P+1] & ((1 << WORD_BITS)-1)) ^ (TAG_INT << WORD_BITS);
            state.P += 3;
            if (TAG(sym) === TAG_REF)
            {
                // If Ai is variable, then we need to bind. This is when foo(7) is called like foo(X).
                bind(sym, arg);
            }
            else if (sym !== arg)
            {
                if (!backtrack())
                    return wamExit(false);
            }
            continue;

        case 22: // unify_void
            if (state.mode === READ)
                state.S += code[state.P+1];
            else
                for (let i = 0; i < code[state.P+1]; i++)
                    alloc_var();
            state.P += 2;
            continue;

        case 23: //unify_variable
            if (state.mode === READ) // If reading, consume the next symbol
            {
                wamValidHeapOrStackAddr('read S', state.S, 'unify_variable');
                source = memory[state.S++];
            }
            else
            {
                source = alloc_var(); // If writing, create a new var
            }
            if (code[state.P+1] === 0) // Y-register
            {
                let register_location23 = state.E + code[state.P+2] + 2;
                // GC: This needs to be trailed if state.B is not 0, apparently
                wamValidStackVarAddr('stack environment var Y', code[state.P+2], register_location23, 'unify_variable');
                bind(memory[register_location23], source);
            }
            else
            {
                register[code[state.P+2]] = source;
            }
            state.P += 3;
            continue;

        case 24: // unify_value
        {
            let did_fail = false;
            if (state.mode === READ) {
                wamValidHeapOrStackAddr('read S', state.S, 'unify_value');
                source = memory[state.S++];
                if (code[state.P + 1] === 0) // Y-register
                {
                    let register_location24 = state.E + code[state.P + 2] + 2;
                    wamValidStackVarAddr('stack environment var Y', code[state.P+2], register_location24, 'unify_value');
                    did_fail = !unify(memory[register_location24], source);
                } else {
                    did_fail = !unify(register[code[state.P + 2]], source);
                }
            } else {
                if (code[state.P + 1] === 0) // Y-register
                {
                    let register_location24 = state.E + code[state.P + 2] + 2;
                    wamValidStackVarAddr('stack environment var Y', code[state.P+2], register_location24, 'unify_value');
                    wamValidHeapAddr('heap top', state.H, 'unify_value');
                    memory[state.H++] = memory[register_location24];
                } else {
                    wamValidHeapAddr('heap top', state.H, 'unify_value');
                    memory[state.H++] = register[code[state.P + 2]];
                }

                if(state.H > maxHeapSize) {
                    maxHeapSize = state.H;
                }
            }
            state.P += 3;
            if (did_fail)
                if (!backtrack())
                    return wamExit(false);
        }
            continue;
        case 25: // unify_local_value
        {
            let did_fail = false;
            if (state.mode === READ) {
                wamValidHeapOrStackAddr('read S', state.S, 'unify_local_value');
                source = memory[state.S++];
                if (code[state.P + 1] === 0) // Y-register
                {
                    let register_location25 = state.E + code[state.P + 2] + 2;
                    wamValidStackVarAddr('stack environment var Y', code[state.P+2], register_location25, 'unify_local_value');
                    did_fail = !unify(memory[register_location25], source);
                } else {
                    did_fail = !unify(register[code[state.P + 2]], source);
                }
            } else {
                let addr;
                if (code[state.P + 1] === 0) // Y-register;
                {
                    let register_location25 = state.E + code[state.P + 2] + 2;
                    wamValidStackVarAddr('stack environment var Y', code[state.P+2], register_location25, 'unify_local_value');
                    addr = memory[register_location25];
                } else {
                    addr = register[code[state.P + 2]];
                }
                addr = deref(addr);
                if (VAL(addr) < state.H) {
                    // Address is on the heap. Just push the value onto the top of the heap
                    wamValidHeapAddr('heap top', state.H, 'unify_local_value');
                    memory[state.H++] = addr;
                } else {
                    // Address is on the stack. Push a new variable onto the heap and bind to the value
                    let fresh = state.H ^ (TAG_REF << WORD_BITS);
                    wamValidHeapAddr('heap top', state.H, 'unify_local_value');
                    memory[state.H++] = fresh;
                    bind(fresh, addr);
                    if (code[state.P + 1] === 1)
                        register[code[state.P + 2]] = fresh; // also set X(i) if X-register
                }

                if(state.H > maxHeapSize) {
                    maxHeapSize = state.H;
                }
            }
            state.P += 3;
            if (did_fail)
                if (!backtrack())
                    return wamExit(false);
        }
        continue;

        case 26: // unify_constant
            if (state.mode === READ)
            {
                wamValidHeapOrStackAddr('read S', state.S, 'unify_constant');
                let sym = deref(memory[state.S++]); // the state.s++ increment is as indicated by Ait-Kaci wamerratum.txt.
                let arg = code[state.P+1] ^ (TAG_ATM << WORD_BITS);
                state.P += 2;
                if (TAG(sym) === TAG_REF)
                {
                    bind(sym, arg);
                }
                else if (sym !== arg)
                    if (!backtrack())
                        return wamExit(false);
            }
            else
            {
                wamValidHeapAddr('heap top', state.H, 'unify_constant');
                memory[state.H++] = code[state.P+1] ^ (TAG_ATM << WORD_BITS);
                if(state.H > maxHeapSize) {
                    maxHeapSize = state.H;
                }
                state.P += 2;
            }
            continue;
        case 27: // unify_integer
            if (state.mode === READ)
            {
                wamValidHeapOrStackAddr('read S', state.S, 'unify_integer');
                let sym = deref(memory[state.S++]);
                let arg = (code[state.P+1] & ((1 << WORD_BITS)-1)) ^ (TAG_INT << WORD_BITS);
                state.P += 2;
                if (TAG(sym) === TAG_REF)
                {
                    bind(sym, arg);
                }
                else if (sym !== arg)
                    if (!backtrack())
                        return wamExit(false);
            }
            else
            {
                wamValidHeapAddr('heap top', state.H, 'unify_integer');
                memory[state.H++] = (code[state.P+1] & ((1 << WORD_BITS)-1)) ^ (TAG_INT << WORD_BITS);
                if(state.H > maxHeapSize) {
                    maxHeapSize = state.H;
                }
                state.P += 2;
            }
            continue;

        case 28: // try_me_else
        {
            // We need to allocate a new choicepoint, but first we need to determine /where/ to put it, since we do not keep a reference to the top of the stack.
            // The last item on the stack is either an environment, or a choicepoint.

            let next = code[state.P + 1];
            let nextCP;
            if ((next & 0x80000000) === 0) {
                // next is a clause index in the current predicate
                nextCP = {
                    code: state.current_predicate.clauses[next].code,
                    predicate: state.current_predicate,
                    offset: 0
                };
            } else {

                // next is an absolute address in the current clause: Used for auxiliary clauses only
                nextCP = {
                    code: code,
                    predicate: state.current_predicate,
                    offset: next ^ 0x80000000
                };
            }

            wam_create_choicepoint(nextCP, []);

            state.P += 2;
        }
            continue;
        case 29: // retry_me_else
        {
            // Unwind the last goal. The arity if the first thing on the stack, then the saved values for A1...An
            wamValidStackAddr('choicepoint arity', state.B, 'retry_me_else');
            let arity = memory[state.B];
            wamValidStackAddr('choicepoint end', state.B + arity + CP_SIZE, 'retry_me_else');
            for (let i = 0; i < arity; i++)
                register[i] = memory[state.B + i + 1];
            // Now restore all the special-purpose registers
            if (memory[state.B + arity + CP_E] < HEAP_SIZE)
                abort("Top of frame contains E which is in the heap");
            if (memory[state.B + arity + CP_E] > HEAP_SIZE + STACK_SIZE)
                abort("Top of frame contains E which exceeds the stack");
            state.E = memory[state.B + arity + CP_E];
            state.CP = memory[state.B + arity + CP_CP];
            let next = code[state.P + 1];
            // set up the 'else' part of retry_me_else by adjusting the saved value of B            
//            memory[state.B + arity + CP_Next] = {code: state.current_predicate.clauses[state.current_predicate.clause_keys[code[state.P+1]]].code, predicate:state.current_predicate, offset:0};
            if ((next & 0x80000000) === 0) {
                // next is a clause index in the current predicate
                memory[state.B + arity + CP_Next] = {
                    code: state.current_predicate.clauses[next].code,
                    predicate: state.current_predicate,
                    offset: 0
                };
            } else {
                // next is an absolute address in the current clause: Used for auxiliary clauses only
                memory[state.B + arity + CP_Next] = {
                    code: code,
                    predicate: state.current_predicate,
                    offset: next ^ 0x80000000
                };
            }
            unwind_trail(memory[state.B + arity + CP_TR], state.TR);

            state.TR = memory[state.B + arity + CP_TR];
            state.H = memory[state.B + arity + CP_H];
            state.HB = state.H;
            if (state.trace_call !== 'no_trace') {
                let traceCallPL = memory[state.B + arity + CP_TC];
                state.trace_call = atable[VAL(traceCallPL)];
                if(! state.trace_call) {
                    throw 'retry_me_else backtrack trace_call is undefined';
                }
                state.trace_info = memory[state.B + arity + CP_TI];
            }
            state.P += 2;
        }
            continue;
        case 30: // trust_me
        {
            // Unwind the last goal. The arity if the first thing on the stack, then the saved values for A1...An
            wamValidStackAddr('choicepoint arity', state.B, 'trust_me');
            let n = memory[state.B];
            wamValidStackAddr('choicepoint end', state.B + n + CP_SIZE, 'trust_me');
            for (let i = 0; i < n; i++) {
                register[i] = memory[state.B + i + 1];
            }
            // Now restore all the special-purpose registers
            if (memory[state.B + n + CP_E] < HEAP_SIZE || memory[state.B + n + CP_E] > HEAP_SIZE + STACK_SIZE)
                abort("Top of frame exceeds bounds in trust. Read from memory[" + (state.B + n + CP_E) + "]. State.B is " + state.B);
            state.E = memory[state.B + n + CP_E];
            state.CP = memory[state.B + n + CP_CP];
            unwind_trail(memory[state.B + n + CP_TR], state.TR);
            state.TR = memory[state.B + n + CP_TR];
            state.H = memory[state.B + n + CP_H];
            if (state.trace_call !== 'no_trace') {
                let traceCallPL = memory[state.B + n + CP_TC];
                state.trace_call = atable[VAL(traceCallPL)];
                if(! state.trace_call) {
                    throw 'trust_me backtrack trace_call is undefined';
                }
                state.trace_info = memory[state.B + n + CP_TI];
            }
            state.B = memory[state.B + n + CP_B];
            if(state.B === 0) {
                state.HB = 0;
            } else {
                wamValidStackAddr('previous choicepoint heap addr', state.B + memory[state.B] + CP_H, 'trust_me');
                state.HB = memory[state.B + memory[state.B] + CP_H];
            }
            state.P += 2;
        }
            continue;

        case 31: // neck_cut
        {
            // Move B back to B0 and tidy the trail. If B == B0 then do nothing (ie if there is a useless cut in the only clause of a predicate)
            let result = true;
            if (state.B > state.B0) {
                while (cleanups[0] !== undefined && cleanups[0].B > state.B0 && cleanups[0].B < state.B) {
                    result = run_cleanup(cleanups[0]) && result;
                    cleanups.shift();
                }
                state.B = state.B0;
                if (state.B > 0) {
                    wamValidStackAddr('previous choicepoint heap addr', state.B + memory[state.B] + CP_H, 'neck_cut');
                    state.HB = memory[state.B + memory[state.B] + CP_H]; // fix from wamerratum.txt
                    tidy_trail();
                }
            }
            if (result)
                state.P += 1;
            else if (!backtrack())
                return wamExit(false);
        }
            continue;
        case 32: // cut(I)
        {
            let register_location32 = state.E + 2 + code[state.P + 1];
            wamValidStackVarAddr('stack environment var Y', code[state.P+1], register_location32, 'cut');
            let y = VAL(memory[register_location32]);
            let result = true;
            if (state.B > y) {
                while (cleanups[0] !== undefined && cleanups[0].B > y && cleanups[0].B < state.B0) {
                    result = run_cleanup(cleanups[0]) && result;
                    cleanups.shift();
                }
                state.B = y;
                if (state.B > 0) {
                    wamValidStackAddr('previous choicepoint heap addr', state.B + memory[state.B] + CP_H, 'cut');
                    state.HB = memory[state.B + memory[state.B] + CP_H]; // fix from wamerratum.txt
                    tidy_trail();
                }
            } else {
            }
            if (result)
                state.P += 2;
            else if (!backtrack())
                return wamExit(false);
            // noinspection UnnecessaryContinueJS
            continue;
        }

        case 33: // get_level(I)
            let register_location33 = state.E + 2 + code[state.P + 1];
            wamValidStackVarAddr('stack environment var Y', code[state.P+1], register_location33, 'get_level');
            memory[register_location33] = state.B0 ^ (TAG_INT << WORD_BITS);
            state.P += 2;
            continue;

        case 40: // call_aux
            offset = code[state.P+1];
            state.CP = {code:code,
                        predicate: state.current_predicate,
                        offset:state.P + 4};
            state.num_of_args = code[state.P+2];
            state.P = offset;
            state.B0 = state.B;
            continue;

        case 41: // execute_aux
            offset = code[state.P+1];
            state.num_of_args = code[state.P+2];
            state.P = offset;
            state.B0 = state.B;            
            continue;

        case 42: // retry_foreign
        {
            wamValidStackAddr('choicepoint specials', state.B + FCP_C, 'retry_foreign');
            state.foreign_value = memory[state.B + FCP_V];
            state.P = memory[state.B + FCP_C].offset;
            code = memory[state.B + FCP_C].code;
            if (!code) {
                throw 'code is undefined';
            }

            state.current_predicate = memory[state.B + FCP_C].current_predicate;
            if(state.current_predicate) {
                state.num_of_args = ftable[state.current_predicate.key][1];
            }

            let n = memory[state.B];
            wamValidStackAddr('choicepoint end', state.B + n + CP_SIZE, 'retry_foreign');
            state.foreign_retry = true;
            for (let i = 0; i <= n - FCP_R; i++) {
                register[i] = memory[state.B + FCP_R + i];
            }
            state.E = memory[state.B + n + CP_E];
            state.CP = memory[state.B + n + CP_CP];
            unwind_trail(memory[state.B + n + CP_TR], state.TR);
            state.TR = memory[state.B + n + CP_TR];
            state.H = memory[state.B + n + CP_H];
            state.HB = state.H;
            if (state.trace_call !== 'no_trace') {
                let traceCallPL = memory[state.B + n + CP_TC];
                state.trace_call = atable[VAL(traceCallPL)];
                if(! state.trace_call) {
                    throw 'retry_foreign backtrack trace_call is undefined';
                }
                state.trace_info = memory[state.B + n + CP_TI];
            }
        }
            continue;
        case 43: // get_choicepoint
        {
            let i = code[state.P + 1];
            let max = i;
            let choice = state.B;
            while (i !== 0) {
                let ordinal = max - i;
                let suffix = ordinal === 1 ? 'st' : ordinal === 2 ? 'nd' : ordinal === 3 ? 'rd' : 'th';
                wamValidStackAddr(ordinal + suffix + ' previous choicepoint base', choice, 'get_choicepoint');
                choice = memory[choice + memory[choice] + CP_B];
                i--;
            }

            let register_location43 = state.E + 2 + code[state.P + 2];
            wamValidStackVarAddr('stack environment var Y', code[state.P+2], register_location43, 'get_level');
            memory[register_location43] = (choice ^ TAG_INT << WORD_BITS);
            state.P += 3;
        }
            continue;

            case 44: // switch_on_term: [44, V, CA, CI, CF, L, S]
        {
            let codePosition = state.P;
            let argument1 = deref(register[0]);

            let V = code[codePosition + 1];
            let CA = code[codePosition + 2];
            let CI = code[codePosition + 3];
            let CF = code[codePosition + 4];
            let L = code[codePosition + 5];
            let S = code[codePosition + 6];

            switch(TAG(argument1)) {
                case TAG_REF:
                    gotoAddress(V, 0); break; // offset = 0 directs gotoAddress to start with control instruction of 'address' (clause index) V.
                case TAG_ATM:
                    if(CA !== FAIL_ADDRESS) {
                        gotoAddress(CA);
                    } else if (!backtrack())
                        return wamExit(false);
                    break;
                case TAG_FLT:
                    if(CF !== FAIL_ADDRESS) {
                        gotoAddress(CF);
                    } else if (!backtrack())
                        return wamExit(false);
                    break;
                case TAG_INT:
                    if(CI !== FAIL_ADDRESS) {
                        gotoAddress(CI);
                    } else if (!backtrack())
                        return wamExit(false);
                    break;
                case TAG_LST:
                    if(L !== FAIL_ADDRESS) {
                        gotoAddress(L);
                    } else if (!backtrack())
                        return wamExit(false);
                    break;
                case TAG_STR:
                    if(S !== FAIL_ADDRESS) {
                        gotoAddress(S);
                    } else if (!backtrack())
                        return wamExit(false);
                    break;
                default:
                    throw('invalid TAG ' + TAG(argument1) + ' on argument1 with value ' + VAL(argument1));
            }
        }
        continue;

            case 45: // switch_on_constant: [45, T, ...]
        {
            let codePosition = state.P;
            let argument1 = VAL(deref(register[0])); // the table is all of one data type: atom, integer, or float.
            let T = code[codePosition + 1];
            let result = search_table_type(T, argument1, codePosition + 2);

            if(result.found) {
                gotoAddress(result.value);
            } else  if (!backtrack()) {
                return wamExit(false);
            }
        }
        continue;

            case 46: // switch_on_structure: [46, T, ...]
        {
            let codePosition = state.P;
            let argument1 = deref(register[0]);
            wamValidHeapOrStackAddr('switch argument', VAL(argument1), 'switch_on_structure');
            let predicateIndicator = VAL(memory[VAL(argument1)]);
            let T = code[codePosition + 1];
            let result = search_table_type(T, predicateIndicator, codePosition + 2);
            if(result.found) {
                gotoAddress(result.value);
            } else if (!backtrack()) {
                return wamExit(false);
            }
        }
            continue;

        case 71: // try: [71, L]
        {
            let codePosition = state.P;
            let L = code[codePosition + 1];
            let nextCP = {
                code: code,
                predicate: state.current_predicate,
                offset: state.P + 2
            };
            wam_create_choicepoint(nextCP, []);
            gotoAddress(L);
        }
        continue;

        case 72: // retry: [72, L]
        {
            let codePosition = state.P;
            let L = code[codePosition + 1];
            // Unwind the last goal. The arity if the first thing on the stack, then the saved values for A1...An
            wamValidStackAddr('choicepoint base', state.B, 'retry');
            let arity = memory[state.B];
            wamValidStackAddr('choicepoint end', state.B + arity + CP_SIZE, 'retry');
            for (let i = 0; i < arity; i++)
                register[i] = memory[state.B + i + 1];
            // Now restore all the special-purpose registers

            wamValidStackAddr('choicepoint environment', memory[state.B + arity + CP_E], 'retry');

            state.E = memory[state.B + arity + CP_E];
            state.CP = memory[state.B + arity + CP_CP];
            let next = state.P + 2;


            memory[state.B + arity + CP_Next] = {
                    code: code,
                    predicate: state.current_predicate,
                    offset: next
                };

            unwind_trail(memory[state.B + arity + CP_TR], state.TR);

            state.TR = memory[state.B + arity + CP_TR];
            state.H = memory[state.B + arity + CP_H];
            state.HB = state.H;
            if (state.trace_call !== 'no_trace') {
                let traceCallPL = memory[state.B + arity + CP_TC];
                state.trace_call = atable[VAL(traceCallPL)];
                if(! state.trace_call) {
                    throw 'retry backtrack trace_call is undefined';
                }
                state.trace_info = memory[state.B + arity + CP_TI];
            }
            gotoAddress(L);
        }
            continue;

        case 73: // trust(L)
        {
            let codePosition = state.P;
            let L = code[codePosition + 1];
            // Unwind the last goal. The arity if the first thing on the stack, then the saved values for A1...An
            wamValidStackAddr('choicepoint base', state.B, 'trust');
            let n = memory[state.B];
            wamValidStackAddr('choicepoint end', state.B + n + CP_SIZE, 'trust');
            for (let i = 0; i < n; i++) {
                register[i] = memory[state.B + i + 1];
            }
            // Now restore all the special-purpose registers
            wamValidStackAddr('choicepoint environment', memory[state.B + n + CP_E], 'trust');
            state.E = memory[state.B + n + CP_E];
            state.CP = memory[state.B + n + CP_CP];
            unwind_trail(memory[state.B + n + CP_TR], state.TR);
            state.TR = memory[state.B + n + CP_TR];
            state.H = memory[state.B + n + CP_H];
            if (state.trace_call !== 'no_trace') {
                let traceCallPL = memory[state.B + n + CP_TC];
                state.trace_call = atable[VAL(traceCallPL)];
                if(! state.trace_call) {
                    throw 'trust backtrack trace_call is undefined';
                }
                state.trace_info = memory[state.B + n + CP_TI];
            }
            state.B = memory[state.B + n + CP_B];
            if(state.B === 0) {
                state.HB = 0;
            } else {
                wamValidStackAddr('previous choicepoint heap addr', state.B + memory[state.B] + CP_H, 'trust');
                state.HB = memory[state.B + memory[state.B] + CP_H];
            }
            gotoAddress(L);
        }
            continue;

            // All the floating point operations are here because I added them as an afterthought!
        case 50: // get_float I from Ai
        sym = deref(register[code[state.P+2]]);
        arg = code[state.P+1] ^ (TAG_FLT << WORD_BITS);
        state.P += 3;
        if (TAG(sym) === TAG_REF)
        {
            // If Ai is variable, then we need to bind. This is when foo(7) is called like foo(X).
            bind(sym, arg);
        }
        else if (sym !== arg)
        {
            if (!backtrack())
                return wamExit(false);
        }
        continue;

        case 74: //    goto_clause: [74, L]
        {
            gotoAddress(code[state.P + 1]);
        }
        continue;

        case 51: // put_float I into Ai
            register[code[state.P+2]] = code[state.P+1] ^ (TAG_FLT << WORD_BITS);
            state.P += 3;
            continue;

        case 52: // unify_float
            if (state.mode === READ)
            {
                wamValidHeapOrStackAddr('read S', state.S, 'unify_float');
                sym = deref(memory[state.S++]);
                arg = code[state.P+1] ^ (TAG_FLT << WORD_BITS);
                state.P += 2;
                if (TAG(sym) === TAG_REF)
                {
                    bind(sym, arg);
                }
                else if (sym !== arg)
                    if (!backtrack())
                        return wamExit(false);
            }
            else
            {
                wamValidHeapAddr('heap top', state.H, 'unify_float');
                memory[state.H++] = code[state.P+1] ^ (TAG_FLT << WORD_BITS);
                if(state.H > maxHeapSize) {
                    maxHeapSize = state.H;
                }
                state.P += 2;
            }
            continue;

        case 60: // put_variable Yn
        {
            // Note that this is different from put_variable(Yn, Ai) in that it ONLY puts a fresh variable into Yn
            // This is needed to make garbage collection safe
            let register_location60 = state.E + code[state.P + 1] + 2;
            wamValidStackVarAddr('new var Y', code[state.P + 1], register_location60, 'put_variable');
            memory[register_location60] = register_location60 ^ (TAG_REF << WORD_BITS);
            state.P += 2;
            // noinspection UnnecessaryContinueJS
            continue;
        }
            

        case 254: // Only clause: NOP
            state.P += 2;
            continue;
        case 255: // halt
            state.running = false;
            continue;
        default:
            abort("unexpected opcode at P=" + (((state.current_predicate == null)?("no predicate"):(atable[ftable[state.current_predicate.key][0]] + "/" + ftable[state.current_predicate.key][1])) + "@" + state.P + ": " + code[state.P]));
        }        
    }
    return wamExit(true);
}

function gotoAddress(address, offset) {
    if((address & 0x80000000) === 0) {
        // address is a clause index in current predicate. 'Go' to that clause and set the
        // program pointer to skip the first instruction (two words): this
        // instruction is always NOP, try_me_else, retry_me_else, or trust_me.
        code = state.current_predicate.clauses[state.current_predicate.clause_keys[address]].code;
        if(! code) {
            throw('code is undefined for gotoAddress '+ address + '.');
        }
        let skip;
        if(typeof offset === 'undefined') {
            skip = 2; // skip the opening control instruction (e.g. try_me_else).
        } else {
            skip = offset;
        }
        state.P = skip;
    } else {
        // address is a position in the current clause code.
        state.P = address ^ 0x80000000;
        if(! code[state.P]) {
            throw('instruction ' + state.P + ' undefined for code of gotoAddress.');
        }
    }
}

function search_table_type(type, value, tableStartPosition) {
    // There are two types of tables: hash and key/value pair sequence.

    let tableSize = code[tableStartPosition];
    if(type === 0) {
        // key/value pair sequence
        return search_table(value, tableStartPosition+1, tableSize);
    } else if(type === 1) {
        return search_table_hash(value, tableStartPosition+1, tableSize);
    } else {
        throw('invalid search table type: ' + type);
    }
}

function search_table(value, tableStartPosition, tableSize) {
    // The table is a sequence of tableSize pairs of key-value words
    // starting at code[tableStartPosition].
    // The pairs are in Prolog term order by keys.
    // For a large table this function can use a binary search.
    // For small tables it is sufficient to do a linear search.

    if(tableSize > 15) {
        return search_table_binary(value, tableStartPosition, tableSize);
    }

    let limit = (tableSize*2)+tableStartPosition;
    for(let searchPosition = tableStartPosition;searchPosition < limit;searchPosition+=2) {
        let key = code[searchPosition];
        if(key === value) {
            return {found: true, value: code[searchPosition+1]};
        }
    }

    return {found: false};
}

function search_table_binary(value, tableStartPosition, tableSize) {
    // The table is a sequence of tableSize pairs of key-value words
    // starting at code[tableStartPosition].
    // The pairs are in Prolog term order by keys.
    // For a large table this function can use a binary search.
    // For small tables it is sufficient to do a linear search.

    let searchLimit = tableSize;
    let searchLimitSuccessor = searchLimit + 1;
    let searchID = searchLimitSuccessor / 2;
    let reducedTableStartPosition = tableStartPosition - 2;
    let found;
    let searchPosition;
    while(true) {
        searchPosition = searchID*2 + reducedTableStartPosition;
        let key = code[searchPosition];
        let comparison = value - key;
        if(comparison < 0) {
            if(searchID === 1) {
                found = false;
                break;
            }
            searchID = (searchID-1) / 2;
        } else if(comparison > 0) {
            if(searchID === searchLimit) {
                found = false;
                break;
            }
            searchID = (searchID + searchLimitSuccessor) / 2;
        } else {
            // comparison === 0 -> value === key
            found = true;
            break;
        }
    }

    if(found) {
        return {found: true, value: code[searchPosition+1]};
    }
    return {found: false};
}

function search_table_hash(value, tableStartPosition, tableSize) {
    // The table is in two layers.
    // The top layer is a sequence of tableSize 'buckets' (tableSize is a power of 2) - each
    // bucket is an address of a second layer table.
    // A second layer table has a word at code[address] with tableSize followed
    // by a sequence of tableSize pairs of key-value words
    // starting at code[address+1].
    // The pairs are in Prolog term order by keys.
    // For a large table this function can use a binary search.
    // For small tables it is sufficient to do a linear search.

    // tableSize bucket hash. Key is 0 to tableSize-1.
    // hash is low order log2(tableSize) bits.

    let mask = tableSize - 1;

    let bucketID = (value & mask) + 1;
    let bucketOfst = bucketID - 1;
    let bucketPosition = tableStartPosition + bucketOfst;
    let subtableAddress = code[bucketPosition] ^ 0x80000000; // subtableAddress in code is from 'linking' a label.
    let subtableSize = code[subtableAddress];
    let subtableStartPosition = subtableAddress + 1;
    return search_table(value, subtableStartPosition, subtableSize)
}

function predicate_suspend_set(value) {
    return suspend_set(atable[VAL(value)]);
}

function suspend_set(value) {
    state.suspended = (value === 'true');
    return true;
}

function instruction_suspend_set(value) {
    state.instruction_suspended = (value === 'true');
    return true;
}

function predicate_trace_set(value) {
    state.trace_call = atable[VAL(value)];
    if(! state.trace_call) {
        throw 'predicate_trace_set trace_call is undefined';
    }
    return true;
}

function predicate_trace_value(value) {
    return unify(value, lookup_atom(state.trace_call));
}

function predicate_trace_set_info(term) {
    state.trace_info = term;
    //stdout('info: ' + term_to_string(term) + '\n');
    return true;
}

function predicate_trace_instruction_set(value) {
    state.trace_instruction = atable[VAL(value)];
    return true;
}

/* Testing */

function hex(number)
{
    if (number === undefined)
        return "undefined";
    if (number < 0)
    {
    	number = 0xFFFFFFFF + number + 1;
    }
    if (typeof number !== 'number') {
        return "NaN";
    }
    // noinspection JSCheckFunctionSignatures
    return "0x" + number.toString(16).toUpperCase();
}

function allocate_first_frame()
{
    // Allocate an environment for the toplevel?
    state.E = HEAP_SIZE; 
    memory[state.E] = 0;     // previous environment = NULL
    memory[state.E + 1] = state.CP;
}

function term_to_string(t)
{
    return format_term(t, {ignore_ops:false, quoted: true, numbervars: false});
}

function copy_state(s)
{
    return {H: s.H,
            HB: s.HB,
            S: s.S,
            P: s.P,
            CP: s.CP, 
            B0: s.B0,
            B: s.B,
            E: s.E,
            TR: s.TR,
            mode: s.mode,
            running: s.running,
            num_of_args: s.num_of_args,
            foreign_retry: s.foreign_retry,
            foreign_value: s.foreign_value,
            current_predicate: s.current_predicate,
            trace_call: s.trace_call,
            trace_predicate: s.trace_predicate,
            trace_code: s.trace_code,
            trace_info: s.trace_info,
            trace_prompt: s.trace_prompt,
            suspended: s.suspended};
}

function copy_registers(r)
{
    return r.slice(0);
}

function copy_memory(m) {
    return m.slice(0);
}


function run_cleanup(c)
{
    let saved_state = copy_state(state);
    let saved_registers = copy_registers(register);
    let saved_code = code;
    state.P = c.P;
    register = c.V.slice(0);
    code = c.code;
    debugging = true;
    let result = true;

    if (!wam())
    {
        // Failure is ignored, but exceptions are raised
        if (exception != null)
            result = false;
    }
    register = copy_registers(saved_registers);
    let saved_heap = state.H;
    state = copy_state(saved_state);
    state.H = saved_heap;
    code = saved_code;
    return result;
}

// Exceptions are implemented as the 'compromise' solution in Bart Demoen's 1989 paper
// http://citeseerx.ist.psu.edu/viewdoc/download?doi=10.1.1.57.4354&rep=rep1&type=pdf
function predicate_throw(t)
{
    exception = record_term(t);
    return unwind_stack();
}

function unwind_stack()
{
    if (state.block === undefined)
    {
        abort("Uncaught exception: " + term_to_string(recall_term(exception, {})));
    }
    state.B = state.block;
    return false;
}

function get_current_block(b)
{
    return unify(b, state.block ^ (TAG_INT << WORD_BITS));
}

function install_new_block(nb)
{
    state.block = state.B;
    return unify(nb, state.B ^ (TAG_INT << WORD_BITS));
}

function reset_block(x)
{
    state.block = VAL(x);
    return true;
}

function clean_up_block(nb)
{
    // If alternative to B is nb, then select it now
    if (memory[state.B+memory[state.B]+CP_B] === VAL(nb))
        state.B = VAL(memory[VAL(nb)+memory[VAL(nb)]+CP_B]);
    return true;

}
function get_exception(e)
{
    if (exception !== null)
    {
        return unify(e, recall_term(exception, {}));
    }
    else
    {
        return false;
    }
}

function clear_exception()
{
    exception = null;
    return true;
}

function undefined_predicate(ftor)
{
    if (prolog_flag_values.unknown === "error")
    {
        let indicator = state.H ^ (TAG_STR << WORD_BITS);
        memory[state.H++] = lookup_functor("/", 2);
        memory[state.H++] = ftable[ftor][0] ^ (TAG_ATM << WORD_BITS);
        memory[state.H++] = ftable_arity(ftor) ^ (TAG_INT << WORD_BITS);
        if(state.H > maxHeapSize) {
            maxHeapSize = state.H;
        }
        existence_error("procedure", indicator);
    }
    else if (prolog_flag_values.unknown === "warning")
    {
        stdout("Undefined predicate " + atable[ftable[ftor][0]] + "/" + ftable_arity(ftor) + "\n");
    }
    if (!backtrack())
    {
        debug("Could not backtrack");
        return false;
    }
    return true;
}

// End exceptions code

function reset_compile_buffer()
{
    compilation_environment.buffer = [];
    return true;
}

function predicate_compile_buffer_codes(codes) {
    let currentCodes;
    if(typeof compilation_environment.buffer === 'undefined' || compilation_environment.buffer.length === 0) {
        currentCodes = NIL;
    } else {
        currentCodes = integers_to_list(compilation_environment.buffer);
    }

    return unify(codes, currentCodes);
}

function predicate_indexing_mode(mode) {
    if(typeof compilation_environment.indexing_mode === 'undefined') {
        return unify(mode, PL_new_atom('basic'));
    }

    return unify(mode, PL_new_atom(compilation_environment.indexing_mode));
}

function predicate_set_indexing_mode(mode) {
    if(TAG(mode) === TAG_REF) {
        return instantiation_error('indexing mode atom', mode);
    }

    if(TAG(mode) !== TAG_ATM) {
        return type_error('indexing mode atom', mode);
    }

    let modeJS = PL_atom_chars(mode);

    if(modeJS !== 'none' && modeJS !== 'basic') {
        return domain_error('indexing mode atom = "none" or "basic"', mode);
    }

    compilation_environment.indexing_mode = modeJS;
    return true;
}

function predicate_compiled_state_boot_code(bootCode)
{
    if(TAG(bootCode) !== TAG_REF && TAG(bootCode) !== TAG_LST) {
        return type_error('var or list', bootCode);
    }
    let list = integers_to_list(bootstrap_code);
    return unify(bootCode, list);
}


function integers_to_list(integers) {
    if(integers.length === 0) {
        return NIL;
    }

    let tmp = state.H ^ (TAG_LST << WORD_BITS);
    for (let i = 0; i < integers.length; i++)
    {
        memory[state.H] = integers[i] ^ (TAG_INT << WORD_BITS);
        // If there are no more items we will overwrite the last entry with [] when we exit the loop
        memory[state.H+1] = ((state.H+2) ^ (TAG_LST << WORD_BITS));
        state.H += 2;
    }
    memory[state.H-1] = NIL;
    return tmp;
}

function terms_to_list(terms) {
    if(terms.length === 0) {
        return NIL;
    }

    let tmp = state.H ^ (TAG_LST << WORD_BITS);
    for (let i = 0; i < terms.length; i++)
    {
        memory[state.H] = terms[i];
        // If there are no more items we will overwrite the last entry with [] when we exit the loop
        memory[state.H+1] = ((state.H+2) ^ (TAG_LST << WORD_BITS));
        state.H += 2;
    }
    memory[state.H-1] = NIL;
    return tmp;
}
function strings_to_atom_list(strings) {
    if(strings.length === 0) {
        return NIL;
    }

    let tmp = state.H ^ (TAG_LST << WORD_BITS);
    for (let i = 0; i < strings.length; i++)
    {
        memory[state.H] = lookup_atom(strings[i]);
        // If there are no more items we will overwrite the last entry with [] when we exit the loop
        memory[state.H+1] = ((state.H+2) ^ (TAG_LST << WORD_BITS));
        state.H += 2;
    }
    memory[state.H-1] = NIL;
    return tmp;
}

function log(target, msg) {
    if(target !== 'none') {
        if (target === 'console') {
            dumpWrite(msg);
        } else if (target === 'local_storage') {
            logToLocalStorage(msg);
        } else if (target === 'local_storage_ring') {
            log_ring(msg);
        } else {
            throw 'invalid log target: ' + target;
        }
    }
}
// File read.js
"use strict";

/* Term reading */
// See http://journal.stuffwithstuff.com/2011/03/19/pratt-parsers-expression-parsing-made-easy/
// Parsers return either:
// 1) An string, in case of an atom
// 2) An integer, in case of an integer
// 3) An object with .list and .tail if a list (because apparently it is not easy to determine if the type of something is a list at runtime!?)
//      If it is a proper list, .tail == NIL
// 4) An object with .variable_name, if a variable
// 5) An object with .functor (a string) and .args (an array) defined if a term

function parse_infix(s, lhs, precedence)
{
    var token = {};
    if (!read_token(s, token))
        return false;
    token = token.value;
    var rhs = {};
    if (!read_expression(s, precedence, false, false, rhs))
        return false;
    return {functor: token,
            args: [lhs, rhs.value]};
}

function parse_postfix(s, lhs)
{
    var token = {};
    if (!read_token(s, token))
        return false;
    return {functor: token.value,
            args: [lhs]};
}

// A reminder: yfx means an infix operator f, with precedence p, where the lhs has a precedence <= p and the rhs has a precedence < p.

// noinspection Annotator
// noinspection JSDuplicatedDeclaration
var prefix_operators = {
    ":-": {precedence: 1200, fixity: "fx"},
    "?-": {precedence: 1200, fixity: "fx"},
    "dynamic": {precedence: 1150, fixity: "fx"},
    "discontiguous": {precedence: 1150, fixity: "fx"},
    "initialization": {precedence: 1150, fixity: "fx"},
    "meta_predicate": {precedence: 1150, fixity: "fx"},
    "module_transparent": {precedence: 1150, fixity: "fx"},
    "multifile": {precedence: 1150, fixity: "fx"},
    "thread_local": {precedence: 1150, fixity: "fx"},
    "volatile": {precedence: 1150, fixity: "fx"},
    "\\+": {precedence: 900, fixity: "fy"},
    "~": {precedence: 900, fixity: "fx"},
    "?": {precedence: 500, fixity: "fx"},
    "+": {precedence: 200, fixity: "fy"},
    "-": {precedence: 200, fixity: "fy"},
    "\\": {precedence: 200, fixity: "fy"}
};

// noinspection Annotator
var postfix_operators = {};

// noinspection Annotator
// noinspection JSDuplicatedDeclaration
var infix_operators = {":-": {precedence: 1200, fixity: "xfx"},
                       "-->": {precedence: 1200, fixity: "xfx"},
                       ";": {precedence: 1100, fixity: "xfy"},
                       "|": {precedence: 1100, fixity: "xfy"},
                       "->": {precedence: 1050, fixity: "xfy"},
                       "*->": {precedence: 1050, fixity: "xfy"},
                       ",": {precedence: 1000, fixity: "xfy"},
                       ":=": {precedence: 990, fixity: "xfx"},
                       "<": {precedence: 700, fixity: "xfx"},
                       "=": {precedence: 700, fixity: "xfx"},
                       "=..": {precedence: 700, fixity: "xfx"},
                       "=@=": {precedence: 700, fixity: "xfx"},
                       "=:=": {precedence: 700, fixity: "xfx"},
                       "=<": {precedence: 700, fixity: "xfx"},
                       "==": {precedence: 700, fixity: "xfx"},
                       "=\\=": {precedence: 700, fixity: "xfx"},
                       ">": {precedence: 700, fixity: "xfx"},
                       ">=": {precedence: 700, fixity: "xfx"},
                       "@<": {precedence: 700, fixity: "xfx"},
                       "@=<": {precedence: 700, fixity: "xfx"},
                       "@>": {precedence: 700, fixity: "xfx"},
                       "@>=": {precedence: 700, fixity: "xfx"},
                       "\\=": {precedence: 700, fixity: "xfx"},
                       "\\==": {precedence: 700, fixity: "xfx"},
                       "is": {precedence: 700, fixity: "xfx"},
                       ">:<": {precedence: 700, fixity: "xfx"},
                       ":<": {precedence: 700, fixity: "xfx"},
                       ":": {precedence: 600, fixity: "xfy"},
                       "+": {precedence: 500, fixity: "yfx"},
                       "-": {precedence: 500, fixity: "yfx"},
                       "/\\": {precedence: 500, fixity: "yfx"},
                       "\\/": {precedence: 500, fixity: "yfx"},
                       "xor": {precedence: 500, fixity: "yfx"},
                       "*": {precedence: 400, fixity: "yfx"},
                       "/": {precedence: 400, fixity: "yfx"},
                       "//": {precedence: 400, fixity: "yfx"},
                       "rdiv": {precedence: 400, fixity: "yfx"},
                       "<<": {precedence: 400, fixity: "yfx"},
                       ">>": {precedence: 400, fixity: "yfx"},
                       "mod": {precedence: 400, fixity: "yfx"},
                       "rem": {precedence: 400, fixity: "yfx"},
                       "**": {precedence: 200, fixity: "xfx"},
                       "^": {precedence: 200, fixity: "xfy"}};

// This returns a javascript object representation of the term. It takes the two extra args because of some oddities with Prolog:
// 1) If we are reading foo(a, b) and we are at the a, we would accept the , as part of the LHS. ie, we think (a,b) is the sole argument. Instead, we should make , have
//    very high precedence if we are reading an arg. Of course, () can reduce this again, so that foo((a,b)) does indeed read ,(a,b) as the single argument
// 2) | behaves slightly differently in lists, in a similar sort of way
function read_expression(s, precedence, isarg, islist, expression)
{
    var token = {};
    if (!read_token(s, token))
        return false;
    token = token.value;
    if (token == null)
    {        
        expression.value = {end_of_file:true};
        return true;
    }

    var lhs;
    var args;
    var arg;
    var t;
    var next;
    // Either the token is an operator, or it must be an atom (or the start of a list or curly-list)
    var op;

    if (token === "\"") {
        // We have to just read chars until we get a close " (taking care with \" in the middle)
        args = [];

        var mode = 0;
        if (prolog_flag_values['double_quotes'] === "chars")
            mode = 0;
        else if (prolog_flag_values['double_quotes'] === "codes")
            mode = 1;
        else if (prolog_flag_values['double_quotes'] === "atom")
            mode = 2;
        while (true) {
            t = get_raw_char_with_conversion(s.stream);
            if (t === '"')
                break;
            if (t === "\\") {
                if (peek_raw_char_with_conversion(s.stream) === '"') {
                    get_raw_char_with_conversion(s.stream);
                    if (mode === 1)
                        args.push('"'.charCodeAt(0));
                    else
                        args.push('"');
                    continue;
                }
            }
            if (mode === 1)
                args.push(t.charCodeAt(0));
            else
                args.push(t);
        }
        if (mode === 2)
            lhs = args.join('');
        else
            lhs = {list: args, tail: "[]"};
    } else {
        op = prefix_operators[token];
        let pt = {};
        if (peek_token(s, pt)) {
            // if the next token is ',', '.', '|', ']', or')' then
            // do not treat the current token as an operator.
            let ptToken = pt.value;
            if (ptToken === "," || ptToken === "." || (ptToken === "|" && islist) || ptToken === "]" || ptToken === ")") {
                op = undefined;
            }
        }

        if (op === undefined) {
            if (token === "[" || token === "{") {
                // The principle for both of these is very similar
                args = [];

                while (true) {
                    t = {};
                    if (!read_expression(s, Infinity, true, true, t))
                        return false;
                    t = t.value;
                    if (t === "]") {
                        lhs = "[]";
                        break;
                        // Special case for the empty list, since the first argument is ']'
                    }
                    args.push(t);
                    next = {};
                    if (!read_token(s, next))
                        return false;
                    next = next.value;

                    if (next !== ',') {
                        if (next === "]" && token === "[") {
                            lhs = {list: args, tail: "[]"};
                            break;
                        } else if (next === "}" && token === "{") {
                            lhs = {functor: "{}", args: args};
                            break;
                        } else if (next === "|" && token === "[") {
                            var tail = {};
                            if (!read_expression(s, Infinity, true, true, tail))
                                return false;
                            lhs = {list: args, tail: tail.value};
                            next = {};
                            if (!read_token(s, next))
                                return false;
                            next = next.value;
                            if (next === "]")
                                break;
                            else
                                return syntax_error("missing ]");
                        } else {
                            return syntax_error("mismatched " + token + " at " + next);
                        }
                    }
                }
            } else if (token === "(") {
                // Is this right? () just increases the precedence to infinity and reads another term?
                lhs = {};
                if (!read_expression(s, Infinity, false, false, lhs))
                    return false;
                lhs = lhs.value;
                next = {};
                if (!read_token(s, next))
                    return false;
                next = next.value;
                if (next !== ")")
                    return syntax_error("mismatched ( at " + next);
            } else if (token === "]") {
                expression.value = token;
                return true;
            } else {
                // It is an atom
                lhs = token;
            }
        } else if (op.fixity === "fx") {
            arg = {};
            if (!read_expression(s, op.precedence, isarg, islist, arg))
                return false;
            lhs = {functor: token, args: [arg.value]};
        } else if (op.fixity === "fy") {
            arg = {};
            if (!read_expression(s, op.precedence + 0.5, isarg, islist, arg))
                return false;
            lhs = {functor: token, args: [arg.value]};
        } else
            return false; // Parse error
    }

    while (true)
    {
        var infix_operator = {};
        if (!peek_token(s, infix_operator))
            return false;
        infix_operator = infix_operator.value;
        if (typeof(infix_operator) === "number" && infix_operator <= 0)
        {
            // Yuck. This is when we read something like X is A-1. Really the - is -/2 in this case
            read_token(s, {});
            unread_token(s, Math.abs(infix_operator));
            unread_token(s, "-");
            infix_operator = "-";
        }

        if (infix_operator === '(')
        {
            // We are reading a term. Keep reading expressions: After each one we should
            // either get , or )
            // First though, consume the (
            read_token(s, {});
            args = [];
            next = {};
            while (true)
            {
                arg = {};
                if (!read_expression(s, Infinity, true, false, arg))
                    return false;
                args.push(arg.value);
                next = {};
                    if (!read_token(s, next))
                        return false;
                    next = next.value;
                if (next === ')')
                    break;
                else if (next !== ',')
                {
                    if (next == null)
                        return syntax_error("end_of_file after "+arg.value);
                    else
                        return syntax_error(next);
                }
            }
            // ./2 is a list
            if (lhs === "." && args.length === 2)
            {
                lhs = {list: args[0],
                       tail: args[1]};
            }
            else
            {
                lhs = {functor: lhs,
                       args:args};
            }
            // Now, where were we?
            infix_operator = {};
            if (!peek_token(s, infix_operator))
                return false;
            infix_operator = infix_operator.value;
        }
        // Pretend that . is an operator with infinite precedence
        if (infix_operator === ".")
        {
            expression.value = lhs;
            return true;
        }
        if (infix_operator === "," && isarg)
        {
            expression.value = lhs;
            return true;
        }
        if (infix_operator === "|" && islist)
        {
            expression.value = lhs;
            return true;
        }
        if (infix_operator == null)
        {
            expression.value = lhs;
            return true;
        }

        op = infix_operators[infix_operator];
        if (op !== undefined)
        {
            if (op.fixity === "xfx" && precedence > op.precedence)
            {
                lhs = parse_infix(s, lhs, op.precedence);
                if (lhs === false)
                    return false;
            }
            else if (op.fixity === "xfy" && precedence > op.precedence)
            {
                // Is this 0.5 thing right? Will it eventually drive up precedence to the wrong place? We never want to reach the next integer...
                lhs = parse_infix(s, lhs, op.precedence+0.5); 
                if (lhs === false)
                    return false;
            }
            else if (op.fixity === "yfx" && precedence > op.precedence)
            {
                lhs = parse_infix(s, lhs, op.precedence-0.5);
                if (lhs === false)
                    return false;
            }
            else
            {
                expression.value = lhs;
                return true;
            }
        } else {

            op = postfix_operators[infix_operator];
            if (op !== undefined) {
                if (op.fixity === "xf" && precedence > op.precedence) {
                    lhs = parse_postfix(s, lhs, op.precedence);
                    if (lhs === false)
                        return false;
                }
                else if (op.fixity === "yf" && precedence >= op.precedence) {
                    lhs = parse_postfix(s, lhs, op.precedence);
                    if (lhs === false)
                        return false;
                }
                else {
                    expression.value = lhs;
                    return true;
                }
            }
            else {
                expression.value = lhs;
                return true;
            }
        }
    }
}

function parse_term_options(options)
{
    var result = {};
    var yes = lookup_atom("true");
    while (options !== NIL)
    {
        if (TAG(options) !== TAG_LST)
            return type_error("list", options);
        var head = get_arg(options, 0); //memory[VAL(options)];
        if (TAG(head) !== TAG_STR)
            return type_error("option", head);
        var ftor = get_arg(head, 0); //memory[VAL(head)];
        if (ftor === lookup_functor("quoted",1))
        {
            result.quoted = (memory[VAL(head)+1] === yes)  // TODO: Should this (and following) use deref?
        } 
        else if (ftor === lookup_functor("ignore_ops",1))
        {
            result.ignore_ops = (memory[VAL(head)+1] === yes)
        }
        else if (ftor === lookup_functor("numbervars",1))
        {
            result.numbervars = (memory[VAL(head)+1] === yes)
        }
        else if (ftor === lookup_functor("variables",1))
        {
            result.variables = get_arg(head, 1); //memory[VAL(head)+1];
        }
        else if (ftor === lookup_functor("variable_names",1))
        {
            result.variable_names = get_arg(head, 1); //memory[VAL(head)+1];
        }
        else if (ftor === lookup_functor("singletons",1))
        {
            result.singletons = get_arg(head, 1); //memory[VAL(head)+1];
        }
        else if (ftor === lookup_functor("max_depth",1))
        {
            result.max_depth = PL_get_integer(get_arg(head, 1)); //memory[VAL(head)+1];
        }
        else
        {
            return type_error(options, head);
        }
        options =  get_arg(options, 1); //memory[VAL(options)+1];
    }
    return result;
}

function read_term(stream, term, options)
{
    if (!(options = parse_term_options(options)))
        return false;
    var streamindex = VAL(get_arg(stream, 1));
    var s = streams[streamindex];
    var context = {stream:s, peeked_token: undefined};
    var expression = {};
    if (!read_expression(context, Infinity, false, false, expression))
        return false;
    expression = expression.value;
    // Depending on the situation, we may expect a . now on the stream. 
    // There will not be one if we are going to return end_of_file because it is actually the eof
    // (Of course, if the file contains end_of_file. then we will return end_of_file AND read the .
    // Luckily we can distinguish the two cases
    // There will also not be one if we are in atom_to_term mode, which is not yet implemented    
    if (typeof expression.end_of_file === "undefined")
    {
        var period = {};
        if (!read_token(context, period))
            return false;
        if (period.value !== ".") // Missing period === eof
            return syntax_error("end_of_file missing period " + period.value + " followed expression " + JSON.stringify(expression));
    }
    else
        expression = "end_of_file";
    
    var varmap = {};
    var singletons = {};
    let readObject = {};
    if(!expression_to_term(expression, varmap, singletons, readObject)) {
        return false;
    }
    var t1 = readObject.value;
    var keys;
    if (options.variables !== undefined || options.singletons !== undefined)
    {
        var equals2 = lookup_functor("=", 2);
        keys = Object.keys(varmap);
        for (var keyOfst = 0; keyOfst < keys.length; keyOfst++)
        {
            var varname = keys[keyOfst];
            if (options.variables !== undefined)
            {                
                if (!unify(state.H ^ (TAG_LST << WORD_BITS), options.variables))
                    return false;
                memory[state.H] = varmap[varname];
                memory[state.H+1] = (state.H+1) ^ (TAG_REF << WORD_BITS);
                options.variables = memory[state.H+1];
                state.H+=2;
            }
            if (options.variable_names !== undefined)
            {                
                if (!unify(state.H ^ (TAG_LST << WORD_BITS), options.variable_names))
                {
                    debug("not unifiable: " + term_to_string(options.variable_names));
                    return false;
                }
                memory[state.H] = (state.H+2) ^ (TAG_STR << WORD_BITS);
                memory[state.H+1] = (state.H+1) ^ (TAG_REF << WORD_BITS);
                options.variable_names = memory[state.H+1];
                memory[state.H+2] = equals2;
                memory[state.H+3] = lookup_atom(varname);
                memory[state.H+4] = varmap[varname];
                state.H+=5;
            }
        }
        if (options.variables !== undefined)
            if (!unify(options.variables, NIL))
                return false;
        if (options.variable_names !== undefined)
            if (!unify(options.variable_names, NIL))
                return false;       
    }
    if (options.singletons !== undefined)
    {
        keys = Object.keys(singletons);
        for (var i = 0; i < keys.length; i++)
        {
            var varname2 = keys[i];
            if (singletons[varname2] === 1 && (! varname2.startsWith('_')
            || (varname2.length > 1 && is_lowercase(varname2.substr(1,1)))))
            {
                if (!unify(state.H ^ (TAG_LST << WORD_BITS), options.singletons))
                    return false;
                memory[state.H] = (state.H+2) ^ (TAG_STR << WORD_BITS);
                memory[state.H+1] = (state.H+1) ^ (TAG_REF << WORD_BITS);
                options.singletons = memory[state.H+1];
                memory[state.H+2] = equals2;
                memory[state.H+3] = lookup_atom(varname2);
                memory[state.H+4] = varmap[varname2];
                state.H+=5;
            }
        }
        if (!unify(options.singletons, NIL))
            return false;
    }
    return unify(term, t1);
}

function is_lowercase(c) {
    return c === c.toLowerCase() && c !== c.toUpperCase();
}

function predicate_write_term(stream, term, options)
{
    if (!(options = parse_term_options(options)))
        return false;
    var value = format_term(term, options);

    //look for atom(X)
    if (TAG(stream) === TAG_STR) {
        let ftor = VAL(memory[VAL(stream)]);
        if (atable[ftable[ftor][0]] === "atom" && ftable_arity(ftor) === 1) {
            let arg = get_arg(stream, 1); //memory[VAL(stream)+1];
            return unify(arg, lookup_atom(value));
        }
    }

    var s = {};
    if (!get_stream(stream, s))
        return false;
    s = s.value;
    if (s.write == null)
        return permission_error("output", "stream", stream);
    
    var bytes = toByteArray(value);
    return (s.write(s, 1, bytes.length, bytes) >= 0)
}

function escape_atom(a)
{
    var chars = a.split('');
    var result = "";
    for (var i = 0; i < chars.length; i++)
    {
        if (chars[i] === "'")
            result += "\\'";
        else if (chars[i] === "\\")
            result += "\\\\";
        else
            result += chars[i];       
    }
    return result;
}

function quote_atom(a)
{
    if (! a.charAt) {
        return a;
    }

    if (a.charAt(0) >= "0" && a.charAt(0) <= "9") {
        return "'" + escape_atom(a) + "'";
    }

    if (a.charAt(0) >= "A" && a.charAt(0) <= "Z")
        return "'" + escape_atom(a) + "'";

    var chars = a.split('');
    if (is_punctuation(chars[0]))
    {
        for (var i = 0; i < chars.length; i++)
        {
            if (!is_punctuation(chars[i]))
                return "'" + escape_atom(a) + "'";
        }
    }
    else
    {
        for (var j = 0; j < chars.length; j++)
        {
            if (is_punctuation(chars[j]) || chars[j] === ' ' || chars[j] === '\\')
                return "'" + escape_atom(a) + "'";
        }
    }
    return a;
}

function is_operator(ftor)
{
    ftor = VAL(ftor);
    if (ftable_arity(ftor) === 2 && infix_operators[atable[ftable[ftor][0]]] !== undefined)
        return true;
    return ftable_arity(ftor) === 1 && prefix_operators[atable[ftable[ftor][0]]] !== undefined;

}


function format_term(value, options, depth)
{
    var result;
    let localDepth = depth;
    if(typeof localDepth === 'undefined') {
        localDepth = 1;
    }
    let maxDepth = options.max_depth;
    if(typeof maxDepth === 'undefined') {
        maxDepth = 0; // '0' means no max depth limit.
    }

    if(maxDepth > 0 && localDepth > maxDepth) {
        return '...';
    }

    let nextDepth = localDepth + 1;

    if (value === undefined)
        return '!undefined!';
        //abort("Illegal memory access in format_term: " + hex(value) + ". Dumping...");
    value = deref(value);
    var lTop;
    switch(TAG(value))
    {
    case TAG_REF:
        if (VAL(value) > HEAP_SIZE)
        {
            if (state.E > state.B)
                lTop = state.E + state.CP.code[state.CP.offset - 1] + 2;
            else
                lTop = state.B + memory[state.B] + 8;
            return "_L" + (lTop - VAL(value));
        }
        else
            return "_G" + VAL(value);
    case TAG_ATM:
        var atom = atable[VAL(value)];
        if (atom === undefined)
            abort("No such atom: " + VAL(value));
        if (options.quoted === true)
            return quote_atom(atom);
        return atom;
    case TAG_INT:
        return PL_get_integer(value) + "";
        // fall-through
    case TAG_FLT:
        return floats[VAL(value)] + "";
    case TAG_STR:
        var ftor = VAL(get_arg(value, 0)); //memory[VAL(value)]);
        let arg = get_arg(value, 1); //memory[VAL(value)+1];

        if (options.numbervars === true && ftor === VAL(lookup_functor('$VAR', 1)) && TAG(arg) === TAG_INT)
        {
            var index = VAL(arg);
            result = String.fromCharCode(65 + (index % 26));
            if (index >= 26)
                result = result + Math.floor(index / 26);
            return result;
        }
        if (!is_operator(ftor) || options.ignore_ops === true)
        {
            // Print in canonical form functor(arg1, arg2, ...)

            result = format_term(ftable[ftor][0] ^ (TAG_ATM << WORD_BITS), options, localDepth) + "("; //
            for (var i = 0; i < ftable_arity(ftor); i++)
            {
                result += format_term(memory[VAL(value)+1+i], options, nextDepth);
                if(maxDepth > 0 && nextDepth > maxDepth) {
                    break;
                }

                if (i+1 < ftable_arity(ftor)) {
                    result += ",";
                }

                nextDepth++;
            }
            return result + ")";            
        }
        else
        {
            // Print as an operator
            var fname = atable[ftable[ftor][0]];
            if (ftable_arity(ftor) === 2 && infix_operators[fname] !== undefined)
            {
                // Infix operator
                var lhs = format_term(memory[VAL(value)+1], options, nextDepth);
                if (is_punctuation_charAt(lhs, lhs.length-1) && !is_punctuation(fname.charAt(0)))
                    result = lhs + fname;
                else if (!is_punctuation_charAt(lhs, lhs.length-1) && is_punctuation(fname.charAt(0)))
                    result = lhs + fname;
                else
                {
                    result = lhs + " " + fname;
                }
                var rhs1 = format_term(memory[VAL(value)+2], options, nextDepth+1);

                if (is_punctuation_charAt(rhs1, 0) && !is_punctuation(fname.charAt(fname.length-1)))
                    return result + rhs1;
                else if (!is_punctuation_charAt(rhs1, 0) && is_punctuation(fname.charAt(fname.length-1)))
                    return result + rhs1;
                else
                    return result + " " + rhs1;
            }
            else if (ftable_arity(ftor) === 1 && prefix_operators[fname] !== undefined)
            {
                // Prefix operator
                var rhs2 = format_term(memory[VAL(value)+1], options, nextDepth);
                if (is_punctuation_charAt(rhs2, 0) && !is_punctuation(fname.charAt(fname.length-1)))
                    return fname + rhs2;
                else if (!is_punctuation_charAt(rhs2,0) && is_punctuation(fname.charAt(fname.length-1)))
                    return fname + rhs2;
                else
                    return fname + " " + rhs2;

            } else {
                return
            }
        }
    case TAG_LST:
        if (options.ignore_ops)
            return "'.'(" + format_term(memory[VAL(value)], options, nextDepth) + "," + format_term(memory[VAL(value)+1], options, nextDepth+1) + ")";
        // Otherwise we need to print the list in list-form
        result = "[";
        var head = get_arg(value, 0); //memory[VAL(value)];
        var tail = get_arg(value, 1); //memory[VAL(value)+1];
        while (true)
        {
            result += format_term(head, options, nextDepth);

            if (tail === NIL || (maxDepth > 0 && nextDepth > maxDepth))
                return result + "]";
            else if (TAG(tail) === TAG_LST)
            {
                head = get_arg(tail, 0); //memory[VAL(tail)];
                tail = get_arg(tail, 1); //memory[VAL(tail)+1];
                result += ",";
            }
            else 
                return result + "|" + format_term(tail, options, nextDepth) + "]";
            nextDepth++ ;
        }
    }
}

function is_punctuation_charAt(object, position) {
    return typeof object.charAt === 'function' && is_punctuation(object.charAt(position));
}

function expression_to_term(s, varmap, singletons, termObject)
{
    let newTerm;

    if (typeof(s) === "string")
        newTerm = lookup_atom(s);
    else if (typeof(s) === "number")
    {
        if (s === ~~s)
        {
            newTerm = PL_put_integer(s); //(s & ((1 << WORD_BITS)-1)) ^ (TAG_INT << WORD_BITS);
        }
        else
        {
            newTerm = lookup_float(s);
        }
    }
    else if (s.variable_name !== undefined)
    {
        if (s.variable_name === '_')
        {
            result = alloc_var(); // every '_' references a distinct variable.
        }
        else if (varmap[s.variable_name] !== undefined)
        {
            result = state.H;            
            memory[state.H] = varmap[s.variable_name];
            state.H++;
        }
        else
        {
            result = alloc_var();
            varmap[s.variable_name] = result;            
        }
        if (singletons[s.variable_name] === undefined)
            singletons[s.variable_name] = 1;
        else
            singletons[s.variable_name]++;
        newTerm = result;
    }
    else if (s.list !== undefined)
    {   
        // Special case for [], as usual, since we do not actually allocate any lists!
        if (s.list.length === 0)
            newTerm = NIL;
        else {

            var result = alloc_var();
            var tail = result;
            var head;
            for (var i = 0; i < s.list.length; i++) {
                unify(tail, state.H ^ (TAG_LST << WORD_BITS));
                head = alloc_var();
                tail = alloc_var();
                let headObject = {};
                if(!expression_to_term(s.list[i], varmap, singletons, headObject)) {
                    if(termObject) {
                        return false;
                    } else {
                        abort("Invalid list expression: " + JSON.stringify(s));
                    }
                }
                unify(head, headObject.value);
            }
            let tailObject = {};
            if(!expression_to_term(s.tail, varmap, singletons, tailObject)) {
                if(termObject) {
                    return false;
                } else {
                    abort("Invalid list expression: " + JSON.stringify(s));
                }
            }
            unify(tail, tailObject.value);
            newTerm = result;
        }
    }
    else if (s.functor !== undefined)
    {
        var t = (state.H ^ TAG_STR << WORD_BITS);
        memory[state.H++] = lookup_functor(s.functor, s.args.length);
        // Reserve space for the args
        var var_args = [];
        for (var j = 0; j < s.args.length; j++)
            var_args[j] = alloc_var();
        for (var k = 0; k < s.args.length; k++)
        {
            let argObject = {};
            if(! expression_to_term(s.args[k], varmap, singletons, argObject)) {
                if(termObject) {
                    return false;
                } else {
                    abort("Invalid structure expression: " + JSON.stringify(s));
                }
            }
            var z = argObject.value;
            unify(z, var_args[k]);
        }
        newTerm = t;
    }
    else {
        if(termObject) {
            return syntax_error("Invalid expression: " + JSON.stringify(s));
        } else {
            abort("Invalid expression: " + JSON.stringify(s));
        }
    }

    if(termObject) {
        termObject.value = newTerm;
        return true;
    } else {
        return newTerm;
    }

}

function peek_token(s, t)
{
    if (s.peeked_tokens === undefined || s.peeked_tokens.length === 0 )
    {
        var tt = {};
        if (!read_token(s, tt))
            return false;
        s.peeked_tokens = [tt.value];
    }
    t.value = s.peeked_tokens[0];
    return true;
}

function unread_token(s, t)
{
    if (s.peeked_tokens === undefined)
        s.peeked_tokens = [t];
    else
        s.peeked_tokens.push(t);
}

function read_token(s, t)
{
    if (s.peeked_tokens !== undefined && s.peeked_tokens.length !== 0)
    {
        t.value = s.peeked_tokens.pop();
        return true;
    }
    return lex(s.stream, t);

}

function is_char(c)
{
    return ((c >= 'a' && c <= 'z') ||
            (c >= 'A' && c <= 'Z') ||
            (c >= '0' && c <= '9') ||
            c === '_');
}

var punctuation_array = ['`', '~', '@', '#', '$', '^', '&', '*', '-', '+', '=', '<', '>', '/', '?', ':', '\\', '.'];

function is_punctuation(c)
{
    return punctuation_array.indexOf(c) !== -1;
}

// lex(stream, t) returns a single token in t.value and fails if an exception is raised
function lex(s, t)
{
    var token;
    while(true)
    {
        var c = get_raw_char_with_conversion(s);
        if (c === -1)
        {
            t.value = null;
            return true;
        }
        var d;
        // Consume any whitespace
        if (c === ' ' || c === '\n' || c === '\t')
            continue;        
        else if (c === '%')
        {
            do
            {
                d = get_raw_char_with_conversion(s);
                if (d === -1)
                {
                    t.value = null;
                    return true;
                }
            } while(d !== '\n');
            continue;
        }
        else if (c === '/')
        {
            d = peek_raw_char_with_conversion(s);
            if (d === '*')
            {
                // Block comment
                get_raw_char_with_conversion(s);
                while(true)
                {
                    d = get_raw_char_with_conversion(s);
                    if (d === -1)
                        return syntax_error("end of file in block comment");
                    if (d === '*' && get_raw_char_with_conversion(s) === '/')
                        break;
                }
                continue;
            }
            else
            {
                // My mistake, the term actually begins with /. c is still set to the right thing
                break;
            }
        }
        break;
    }

    if (c === ',') {
        t.value = c;
        return true;
    }
    else
        if ((c >= 'A' && c <= 'Z') || c === '_')
    {
        token = {variable_name: "" + c};
        // Variable. May contain a-zA-Z0-9_
        while (true)
        {
            c = peek_raw_char_with_conversion(s);
            if (is_char(c))
            {
                token.variable_name += get_raw_char_with_conversion(s);
            }
            else
            {
                t.value = token; 
                return true;
            }
        } 
    }
    else if (c === '0' && peek_raw_char_with_conversion(s) === 'x')
    {
        // hex positive integer
        // 0x10 === 16
        let radix = get_raw_char_with_conversion(s);
        // radix is 'x'.

        token = 0;

        while (true)
        {
            c = peek_raw_char_with_conversion(s);
            let hex;
            if(c >= '0' && c <= '9') {
                hex = (get_raw_char_with_conversion(s) - '0');
            } else if(c >= 'a' && c <= 'f') {
                hex = (get_raw_char_with_conversion(s) - 'a') + 10;
            } else if(c >= 'A' && c <= 'F') {
                hex = (get_raw_char_with_conversion(s) - 'A') + 10;
            } else if (is_char(c)) {
                return syntax_error("invalid hex number " + token + ": " + c);
            } else {
                t.value = token;
                return true;
            }

            token = token * 16 + hex;
        }
    }
    else if ((c >= '0' && c <= '9') || (c === '-' && peek_raw_char_with_conversion(s) >= '0' && peek_raw_char_with_conversion(s) <= '9'))
    {
        // Integer. May contain 0-9 only. Floats complicate this a bit
        var negate = false;
        if (c === '-')
        {
            token = '';
            negate = true;
        }
        else
            token = c - '0';
        var decimal_places = 0;
        var seen_decimal = false;
        while (true)
        {            
            c = peek_raw_char_with_conversion(s);       
            if (seen_decimal)
                decimal_places++;
            if ((c >= '0' && c <= '9'))
            {
                token = token * 10 + (get_raw_char_with_conversion(s) - '0');
            }
            else if (c === '.' && !seen_decimal)
            {
                let x = peek_raw_char_with_conversion(s, 2);
                if(x && x >= '0' && x <= '9') {
                    seen_decimal = true;
                    get_raw_char_with_conversion(s);
                } else {
                    t.value = negate?(-token):token;
                    return true;
                }
            }
            else if (is_char(c))
                return syntax_error("illegal number" + token + ": " + c);
            else
            {
                if (seen_decimal)
                {
                    for (var i = 1; i < decimal_places; i++)
                        token = token / 10;
                }
                t.value = negate?(-token):token;
                return true;
            }
        }
    }
    else 
    {
        // Either:
        // 1) a term
        // 2) an atom (which is a term with no arguments) 
        // 3) An operator
        // In all cases, first we have to read an atom
        var buffer = "";
        var state = 0;
        if (c === '\'')
        {
            // Easy. The atom is quoted!
            //
            // Escaped characters:
            // '\\' becomes '\'
            // '\n' becomes a newline character
            // '\t' becomes a tab character.

            while(true)
            {
                c = get_raw_char_with_conversion(s);
                if (c === '\\') {
                    state = (state + 1) % 2;
                    if(state === 0) {
                        buffer += c; // an escaped '\'.
                    }
                }
                else if (c === -1) {
                    return syntax_error("end of file in atom");
                }
                else if (c === '\'' && state === 0) {
                    break;
                }
                else {
                    if(state === 1) {
                        state = 0; // 'c' is not a '\', so we are finished with a possibly-empty sequence of '\'.
                        // Value of 'c' is not '\'. It is some escaped character.
                        switch(c) {
                            case 'n': c = '\n'; break;
                            case 't': c = '\t'; break;
                            case 'b': c = '\b'; break;
                            case 'f': c = '\f'; break;
                            case 'r': c = '\r'; break;
                            case 'v': c = '\v'; break;
                            case 'x': {
                                // unicode: '\xXXXX' (legacy Edinburgh) or '\xXXXX\' (ISO spec)
                                let unicode = '';
                                for (let i = 0; i < 4; i++) {
                                    c = get_raw_char_with_conversion(s);
                                    unicode += c;
                                }
                                let x = peek_raw_char_with_conversion(s);
                                if(x === '\\') {
                                    // skip the terminating '\'
                                    get_raw_char_with_conversion(s);
                                }
                                c = String.fromCharCode(parseInt(unicode, 16));
                                break;
                            }
                            case 'u': {
                                // unicode: '\uXXXX'
                                let unicode = '';
                                for (let i = 0; i < 4; i++) {
                                    c = get_raw_char_with_conversion(s);
                                    unicode += c;
                                }
                                c = String.fromCharCode(parseInt(unicode, 16));
                                break;
                            }
                            case 'U': {
                                // unicode: '\uXXXXXXXX'
                                let unicode = '';
                                for (let i = 0; i < 8; i++) {
                                    c = get_raw_char_with_conversion(s);
                                    unicode += c;
                                }
                                c = String.fromCharCode(parseInt(unicode, 16));
                                break;
                            }
                            default:
                                // For unspecified characters the escaped char
                                // represents the char: e.g. '\k' === 'k'.
                        }

                        /*
                        \a
Alert character. Normally the ASCII character 7 (beep).
\b
Backspace character.
\c
No output. All input characters up to but not including the first non-layout character are skipped. This allows for the specification of pretty-looking long lines. Not supported by ISO. Example:
format('This is a long line that looks better if it was \c
       split across multiple physical lines in the input')
\<NEWLINE>
When in ISO mode (see the Prolog flag iso), only skip this sequence. In native mode, white space that follows the newline is skipped as well and a warning is printed, indicating that this construct is deprecated and advising to use \c. We advise using \c or putting the layout before the \, as shown below. Using \c is supported by various other Prolog implementations and will remain supported by SWI-Prolog. The style shown below is the most compatible solution.25
format('This is a long line that looks better if it was \
split across multiple physical lines in the input')
instead of

format('This is a long line that looks better if it was\
 split across multiple physical lines in the input')
Note that SWI-Prolog also allows unescaped newlines to appear in quoted material. This is not allowed by the ISO standard, but used to be common practice before.

\e
Escape character (ASCII 27). Not ISO, but widely supported.
\f
Form-feed character.
\n
Next-line character.
\r
Carriage-return only (i.e., go back to the start of the line).
\s
Space character. Intended to allow writing 0'\s to get the character code of the space character. Not ISO.
\t
Horizontal tab character.
\v
Vertical tab character (ASCII 11).
\xXX..\
Hexadecimal specification of a character. The closing \ is obligatory according to the ISO standard, but optional in SWI-Prolog to enhance compatibility with the older Edinburgh standard. The code \xa\3 emits the character 10 (hexadecimal `a') followed by `3'. Characters specified this way are interpreted as Unicode characters. See also \u.
\uXXXX
Unicode character specification where the character is specified using exactly 4 hexadecimal digits. This is an extension to the ISO standard, fixing two problems. First, where \x defines a numeric character code, it doesn't specify the character set in which the character should be interpreted. Second, it is not needed to use the idiosyncratic closing \ ISO Prolog syntax.
\UXXXXXXXX
Same as \uXXXX, but using 8 digits to cover the whole Unicode set.
\40
Octal character specification. The rules and remarks for hexadecimal specifications apply to octal specifications as well.
\\
Escapes the backslash itself. Thus, '\\' is an atom consisting of a single \.
\'
Single quote. Note that '\'' and '''' both describe the atom with a single ', i.e., '\'' == '''' is true.
\"
Double quote.
\`
Back quote.
                         */

                    }
                    buffer += c;
                }
            }
            
        }
        else // Not so simple. Have to read an atom using rules, which are actually available only for a fee from ISO...
        {
            buffer += c;
            // An unquoted atom may contain either all punctuation or all A-Za-z0-9_. There are probably more complicated rules, but this will do
            var char_atom = is_char(c);
            var punctuation_atom = is_punctuation(c);
            while (true)
            {                
                c = peek_raw_char_with_conversion(s);                
                if (c === -1)
                    break;
                if (char_atom && is_char(c))
                    buffer += get_raw_char_with_conversion(s);
                else if (punctuation_atom && is_punctuation(c))
                    buffer += get_raw_char_with_conversion(s);
                else
                    break;
            }            
        }
        t.value=buffer;
        return true;
    }
}

// This is one of the more ridiculous things in the ISO standard
var char_conversion_override = {};
function predicate_char_conversion(a, b)
{
    if (TAG(a) !== TAG_ATM)
        return type_error("atom", a);
    if (TAG(b) !== TAG_ATM)
        return type_error("atom", b);
    char_conversion_override[atable[VAL(a)]] = atable[VAL(b)];
    return true;
}

function predicate_current_char_conversion(a, b)
{
    var index;
    if (TAG(a) === TAG_ATM)
    {
        var aname = atable[VAL(a)];
        if (char_conversion_override[aname] === undefined)
            return unify(a, b);
        else
            return unify(lookup_atom(char_conversion_override[aname]), b);
    }
    else if (TAG(b) === TAG_ATM)
    {
        var bname = atable[VAL(b)];
        var keys = Object.keys(char_conversion_override);
        for (var i = 0; i < keys.length; i++)
        {
            if (char_conversion_override[keys[i]] === bname)
                return unify(lookup_atom(keys[i]), a);
        }
        return unify(a,b);
    }
    if (TAG(a) === TAG_REF && TAG(b) === TAG_REF)
    {
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
        aname = String.fromCharCode(index);
        unify(a, lookup_atom(aname));
        if (char_conversion_override[aname] === undefined)
            return unify(a, b);
        else
            return unify(lookup_atom(char_conversion_override[aname]), b);

    }
    else
        return type_error(a);
}

function get_raw_char_with_conversion(s)
{
    if (!prolog_flag_values['char_conversion'])
        return get_raw_char(s);    
    var t = get_raw_char(s);
    var tt = char_conversion_override[t];
    if (tt === undefined)
        return t;
    else
        return tt;
}

function peek_raw_char_with_conversion(s, position)
{
    if (!prolog_flag_values['char_conversion'])
        return peek_raw_char(s, position);
    var t = peek_raw_char(s, position);
    var tt = char_conversion_override[t];
    if (tt === undefined)
        return t;
    else
        return tt;
}


// noinspection JSUnusedLocalSymbols
function parser_test()
{
    //do_parser_test("test(1,1).\ntest(1:-1).\ntest:- test, test.\ntest((1,1)).");
    //do_parser_test("foo:- a, b, c.");
    do_parser_test("foo([a|b]).");
}

function parser_test_read(stream, size, count, buffer)
{
    var bytes_read = 0;
    var records_read;
    var t;
    for (records_read = 0; records_read < count; records_read++)
    {
        for (var b = 0; b < size; b++)
        {
            t = stream.data.shift();
            if (t === undefined)
            {                
                return records_read;
            }
            buffer[bytes_read++] = t;
        }
    }
    return records_read;
}

function do_parser_test(input_string)
{
    var s = {peeked_token: undefined,
         stream: new_stream(parser_test_read,
                            null,
                            null,
                            null,
                            null,
                            toByteArray(input_string))};
    state = {H:0};
    while(true)
    {        
        var e = {};
        if (!read_expression(s, Infinity, false, false, e))
        {
            debug("Failed to parse");
            return false;
        }
        e = e.value;
        if (e.end_of_file === true)
            break;
        debug("Read expression: " + expression_to_string(e));
        
        var p = {};
        if (!read_token(s, p))
        {
            debug("Failed to parse");
            return false;
        }
        p = p.value;
        if (p === ".")
        {
        }
        else
        {
            debug("Error: Expression terminated with >" + p + "<");
        }
        if (e.end_of_file !== undefined)
            break;
    }
}

function expression_to_string(s)
{
    var t;

    if (typeof(s) === "string")
        return s;
    if (typeof(s) === "number")
        return s;
    if (s.variable_name !== undefined)
        return "_" + s.variable_name;
    if (s.list !== undefined)
    {
        t = "[";
        for (var i = 0; i < s.list.length; i++)
        {
            if (i+1 < s.list.length)
                t += expression_to_string(s.list[i]) + ", ";
            else
            {
                t += expression_to_string(s.list[i]);
                if (s.tail === "[]")
                    t += "]";
                else
                    t += "|" + expression_to_string(s.tail) + "]";
            }
        }
        return t;
    }
    if (s.functor !== undefined)
    {
        t = "" + s.functor + "(";
        for (var j = 0; j < s.args.length; j++)
        {
            if (j+1 < s.args.length)
            {
                t += expression_to_string(s.args[j]) + ", ";
            }
            else
                t += expression_to_string(s.args[j]) + ")";
        }
        return t;
    }
}


function atom_to_term(atom, term, bindings)
{
    var stream = new_stream(read_atom, null, null, null, null, {data:toByteArray(atable[VAL(atom)]), ptr:0});
    var context = {stream:stream, peeked_token: undefined};
    var expression = {};
    if (!read_expression(context, Infinity, false, false, expression))
        return false;
    expression = expression.value;
    var b = {};
    let termObject1 = {};
    if(!expression_to_term(expression, b, {}, termObject1)) {
        return false;
    }

    var t1 = termObject1.value;
    var arglist = [];
    var keys = Object.keys(b);
    for (var i = 0 ; i < keys.length; i++)
        arglist.push({functor:"=", args:[keys[i], {variable_name:keys[i]}]});
    let termObject2 = {};
    if(!expression_to_term({list:arglist, tail:{list: []}}, b, {}, termObject2)) {
        return false;
    }
    var t2 = termObject2.value;
    return unify(term, t1) && unify(bindings, t2);
}

function read_atom(stream, size, count, buffer)
{
    var bytes_read = 0;
    var records_read;
    var info = stream.data;
    for (records_read = 0; records_read < count; records_read++)
    {
        for (var b = 0; b < size; b++)
        {
            var t = info.data[info.ptr++];
            if (t === undefined)
                return records_read;
            buffer[bytes_read++] = t;
        }
    }
    return records_read;
}

// File record.js
"use strict";

/* Need to implement recorda/3, recorded/3 and erase/1 */
var database_ptr = 0;
var database_references = {};
var databases = {};

/* 
   Because we don't have access to pointers in Javascript, this is quite hard to do efficiently. It's quite hard to do at all!
   database_references contains a key-value pair with uniquely generated integer keys. The key is returned as a clause reference.
   The database_reference:value is an object containing two values: Array and Index.
   Array is a key into the databases object. The database:value is an array. Index is the index into that array of the actual value
   stored in the clause reference.
   Eventually I will move the code into databases[0]
*/

function recorda(key, term, ref)
{
    // Get the database associated with key. 
    var d = databases[key];
    var i = 0;
    if (d === undefined)
    {
        // No such database yet. Create one, and store it in databases
        databases[key] = {data:{},
                          keys:[],
                          ptr: 0};
        d = databases[key];
    }
    else
    {
        i = d.ptr;
    }
    // Now store the term in d at i
    d.data[i] = {value: record_term(term),
                 ref: database_ptr};
    // And finally, store the key in the keys arrays, putting it at the front
    d.keys.unshift(i);

    
    d.ptr++;
    // Next, save the clause reference in database_references
    database_references[database_ptr] = {array: key,
                                         index: i};
    // And now we can unify it with ref
    var result = unify(ref, database_ptr ^ (TAG_INT << WORD_BITS));
    // And increment it
    database_ptr++;
    return result;
}


function recordz(key, term, ref)
{
    // Get the database associated with key. 
    var d = databases[key];
    var i = 1;
    if (d === undefined)
    {
        // No such database yet. Create one, and store it in databases
        databases[key] = {data:{},
                          keys:[],
                          ptr: 0};
        d = databases[key];
    }
    else
    {
        i = d.ptr;
    }
    // Now store the term in d at i
    d.data[i] = {value: record_term(term),
                 ref: database_ptr};
    // And finally, store the key in the keys arrays, putting it at the front
    d.keys.push(i);

    
    databases[key].ptr++;
    // Next, save the clause reference in database_references
    database_references[database_ptr] = {array: key,
                                         index: i};
    // And now we can unify it with ref
    var result = unify(ref, database_ptr ^ (TAG_INT << WORD_BITS));
    // And increment it
    database_ptr++;
    return result;
}

function recorded(key, term, ref)
{
    // Ok, first find the database
    var d = databases[key];
    // Check if there is anything recorded. If not, fail.
    if (d === undefined)
    {
        return false; 
    }
    // Ok, now we can get the actual array
    let data = d.data;
    // We need the first actual key. This may not be [0] if something has been erased
    var index = d.keys[0];
    if(typeof index === 'undefined' || typeof data[index] === 'undefined') {
        return false;
    }
    // noinspection UnnecessaryLocalVariableJS
    var result = unify(recall_term(data[index].value, {}), term) && unify(data[index].ref ^ (TAG_INT << WORD_BITS), ref);
    return result;
}

function erase(ref)
{
    // First find the array
    var dr = database_references[VAL(ref)];
    if (dr === undefined)
        return false;
    var d = databases[dr.array];
    // Now set to undefined
    delete d.data[dr.index];
    // Now we must also delete the keys entry. This requires a search, unfortunately since there is no way to keep track of indices if we allow unshifting
    for (var i = 0; i < d.keys.length; i++)
    {
        if (d.keys[i] === dr.index)
        {
            d.keys.splice(i, 1);
            break;
        }
    }

    return true;
}

function predicate_record_term(term, string) {
    return unify(string, JSON.stringify(record_term(term)));
}

// record_term returns a new object which is a javascript representation of the term
function record_term(t)
{
    t = deref(t);
    switch(TAG(t))
    {
    case TAG_REF:
        return {type: TAG_REF,
                key: VAL(t)};
    case TAG_ATM:
        return {type: TAG_ATM,
                value: atable[VAL(t)]};
    case TAG_FLT:
        return {type: TAG_FLT,
                value: floats[VAL(t)]};
    case TAG_INT:
        return {type: TAG_INT,
                value: PL_get_integer(t)};
    case TAG_LST:
        var value = [];
        var list = {type: TAG_LST,
                    value: value};
        while (TAG(t) === TAG_LST)
        {
            value.push(record_term(VAL(t)));
            t = get_arg(t, 1); //memory[VAL(t)+1];
        }
        list.tail = record_term(t);
        return list;
    case TAG_STR:
        var ftor = ftable[VAL(memory[VAL(t)])];
        var args = [];
        var result = {type: TAG_STR,
                      name: atable[ftor[0]],
                      args: args};        
        for (var i = 0; i < ftor[1]; i++)
        {
            args.push(record_term(memory[VAL(t)+1+i]));
        }       
        return result;
    }
}

function recall_term(e, varmap)
{
    // return a reference to an equivalent WAM term to the expression e
    switch(e.type)
    {
    case TAG_REF: {
        let result;
        if (varmap[e.key] !== undefined) {
            result = state.H;
            memory[state.H] = varmap[e.key];
            state.H++;
        } else {
            result = alloc_var();
            varmap[e.key] = result;
        }
        return result;
    }
    case TAG_ATM:
        return lookup_atom(e.value);
    case TAG_FLT:
        return lookup_float(e.value);
    case TAG_INT:
        return PL_put_integer(e.value); //e.value ^ (TAG_INT << WORD_BITS);
    case TAG_LST: {
        let result = alloc_var();
        var tail = result;
        var head;
        for (let i = 0; i < e.value.length; i++) {
            unify(tail, state.H ^ (TAG_LST << WORD_BITS));
            head = alloc_var();
            tail = alloc_var();
            unify(head, recall_term(e.value[i], varmap));
        }
        unify(tail, recall_term(e.tail, varmap));
        return result;
    }
    case TAG_STR:
        var t = (state.H ^ TAG_STR << WORD_BITS);
        memory[state.H++] = lookup_functor(e.name, e.args.length);
        // Reserve space for the args
        var var_args = [];
        for (let i = 0; i < e.args.length; i++)
            var_args[i] = alloc_var();
        for (let i = 0; i < e.args.length; i++)
        {
            let z = recall_term(e.args[i], varmap);
            unify(z, var_args[i]);
        }
        return t;
    default:
        abort("invalid term type: " + JSON.stringify(e));
    }
}
// File fli.js
"use strict";

/* Not implemented:
   All the nondet foreign stuff. That is supported, but not using the SWI-Prolog interface
   Strings
   Floats
   Pointers
   PL_get_chars
   PL_predicate_info
   PL_copy_term_ref
   PL_reset_term_refs
*/

/**
 * @return {number}
 */
/*
function PL_new_term_ref() {
    // FIXME: Should this go on the heap or the stack?
    return alloc_var();
}

function PL_new_term_refs(n)
{
    var first = alloc_var();
    for (i = 0; i < n-1; i++)
        alloc_var();
        
}
*/

/**
 * @return {boolean}
 */
function PL_succeed()
{
    return true;
}

/**
 * @return {boolean}
 */
function PL_fail()
{
    return true;
}

function PL_new_atom(chars)
{
    return lookup_atom(chars);
}

/**
 * @return {string}
 */
function PL_atom_chars(atom)
{
    return atable[VAL(atom)];
}

function PL_new_functor(name, arity)
{
    return lookup_functor(atable[name], arity);
}

function PL_functor_name(ftor)
{
    return ftable[VAL(ftor)][0];
}

function PL_functor_arity(ftor)
{
    return ftable[VAL(ftor)][1];
}

/**
 * @return {number}
 */
function PL_term_type(term)
{
    return TAG(term);
}

/**
 * @return {boolean}
 */
function PL_is_variable(term)
{
    return TAG(term) === TAG_REF;
}

/**
 * @return {boolean}
 */
function PL_is_atom(term)
{
    return TAG(term) === TAG_ATM;
}

/**
 * @return {boolean}
 */
function PL_is_integer(term)
{
    return TAG(term) === TAG_INT;
}

/**
 * @return {boolean}
 */
function PL_is_compound(term)
{
    return TAG(term) === TAG_STR;
}

/**
 * @return {boolean}
 */
function PL_is_functor(term, ftor)
{
    return TAG(term) === TAG_STR && memory[VAL(term)] === ftor;
}

/**
 * @return {boolean}
 */
function PL_is_list(term)
{
    return TAG(term) === TAG_LST || term === NIL;
}

/**
 * @return {boolean}
 */
function PL_is_atomic(term)
{
    return TAG(term) !== TAG_STR && TAG(term) !== TAG_LST && TAG(term) !== TAG_REF;
}

/**
 * @return {boolean}
 */
function PL_is_number(term)
{
    return TAG(term) === TAG_INT || TAG(term) === TAG_FLT;
}

function PL_get_atom(term)
{
    if (TAG(term) === TAG_ATM)
        return atom;
    throw("type_error: atom");
}

/**
 * @return {string}
 */
function PL_get_atom_chars(term)
{
    if (TAG(term) === TAG_ATM)
        return atable[VAL(term)];
    throw("type_error: atom");
}

/**
 * @return {number}
 */
function PL_get_integer(term)
{
    if (TAG(term) === TAG_INT) {
        if ((VAL(term) & (1 << (WORD_BITS-1))) === (1 << (WORD_BITS-1)))
            return (VAL(term) - (1 << WORD_BITS));
        else
            return VAL(term);
    }
    throw("type_error: integer");
}

function PL_get_functor(term)
{
    if (TAG(term) === TAG_STR)
        return memory[VAL(term)];
    throw("type_error: term");
}

function PL_get_arg(index, term)
{
    if (index < 1)
        throw("domain_error: term arity");
    if (TAG(term) === TAG_STR)
    {
        if (index > ftable[VAL(memory[VAL(term)])][1])  // Check arity is OK
            throw("type_error: term arity");
        return memory[VAL(term) + index];
    }
    throw("type_error: term");
}

// Returns an object with head and tail keys
/**
 * @return {null}
 */
function PL_get_list(list)
{
    if (TAG(list) === TAG_LST)
        return {head: memory[VAL(list)],
                tail: memory[VAL(list)+1]};
    return null;
}

/**
 * @return {null}
 */
function PL_get_head(list)
{
    if (TAG(list) === TAG_LST)
        return memory[VAL(list)];
    return null;
}

/**
 * @return {null}
 */
function PL_get_tail(list)
{
    if (TAG(list) === TAG_LST)
        return memory[VAL(list)+1];
    return null;
}

/**
 * @return {null}
 */
function PL_get_nil()
{
    return NIL;
}

/**
 * @return {null}
 */
function PL_put_variable()
{
    return alloc_var();
}

/**
 * @return {null}
 */
function PL_put_atom(atomID)
{
    return atomID ^ (TAG_ATM << WORD_BITS);
}

function PL_put_atom_chars(chars)
{
    return lookup_atom(chars);
}

/**
 * @return {number}
 */
function PL_put_integer(integer)
{
    return (integer & ((1 << WORD_BITS)-1)) ^ (TAG_INT << WORD_BITS)
}

/*
function PL_put_functor(term, ftor)
{
    var r = alloc_structure(ftor);
    for (i = 0; i < ftable[VAL(ftor)][1]; i++)
        alloc_var();
}

function PL_put_list()
{
    var r = alloc_list();
    alloc_var();
    alloc_var();
}
*/

/**
 * @return {null}
 */
function PL_put_nil()
{
    return NIL;
}

/**
 * @return {boolean}
 */
/*
function PL_cons_functor(ftor)
{
    if (state.H + arguments.length + 1 >= HEAP_SIZE)
        return false; // Not enough heap
    var r = state.H ^ (TAG_STR << WORD_BITS);
    memory[state.H++] = ftor;
    for (i = 1; i < arguments.length; i++)
        memory[state.H++] = arguments[i];
    return true;
}

function PL_cons_list(head, tail)
{
    if (state.H +2 >= HEAP_SIZE)
        return false;
    var result = state.H ^ (TAG_LST << WORD_BITS);
    memory[state.H++] = head;
    memory[state.H++] = tail;
    return result;
}
*/

function PL_unify_integer(term, integer)
{
    return unify(term, integer ^ (TAG_INT << WORD_BITS));
}

function PL_unify_float(term, float)
{
    return unify(term, lookup_float(float));
}

function PL_unify_atom_chars(term, chars)
{
    return unify(term, lookup_atom(chars));
}

function PL_unify(t1, t2)
{
    return unify(t1, t2);
}

function PL_unify_atom(term, atom)
{
    return unify(term, atom);
}

function PL_unify_nil(term)
{
    return unify(term, NIL);
}

function PL_unify_arg(index, term, arg)
{
    return unify(memory[VAL(term) + 1 + index], arg);
}

function PL_unify_list(list, head, tail)
{
    return (TAG(list) === TAG_LST) && unify(memory[VAL(list)], head) && unify(memory[VAL(list) + 1], tail);
}

// noinspection JSUnusedLocalSymbols
function PL_pred(ftor, module)
{
    if (predicates[ftor] === undefined)
        throw("Undefined predicate");
    return ftor;
}

function PL_predicate(name, arity, module)
{
    return PL_pred(lookup_functor(name, arity), module);
}

function PL_open_query(module, debug, predicate, args)
{
    initialize();
    allocate_first_frame();
    state.P = predicates[predicate];
    for (let i = 0; i < ftable[predicate][1]; i++)
        register[i] = args[i];
    return {fresh:true};
}

function PL_next_solution(qid)
{    
    if (!qid.fresh)
        backtrack();
    qid.fresh = false;
    return wam();
}

// noinspection JSUnusedLocalSymbols
function PL_call(term, module)
{
    let ftor = VAL(memory[VAL(term)]);
    initialize();
    allocate_first_frame();
    state.P = predicates[ftor];
    for (let i = 0; i < ftable_arity(ftor); i++)
        register[i] = get_arg(term, 1+i); //memory[VAL(term) + 1 + i];
    return wam();    
}

function PL_cut_query(qid)
{
    // This is not implemented
}

function PL_close_query(qid)
{
    // This is not implemented either
}


function PL_call_predicate(module, debug, predicate, args)
{
    var qid = PL_open_query(module, debug, predicate, args);
    var result = PL_next_solution(qid);
    PL_cut_query(qud);
    return result;
}
// File stream.js
"use strict";

var current_input = null;
var current_output = 0;
// FIXME: Ignores size and count!
function stdout_write(stream, size, count, buffer)
{
    var str = fromByteArray(buffer);
    stdout(str);
    return size*count;
}

function predicate_set_input(stream)
{
    var s = {};
    if (!get_stream_fd(stream, s))
        return false;
    current_input = s.value;
    return true;
}

function predicate_set_output(stream)
{
    var s = {};
    if (!get_stream_fd(stream, s))
        return false;
    current_output = s.value;
    return true;
}

function predicate_current_input(stream)
{   var ftor = lookup_functor('$stream', 1);
    var ref = alloc_structure(ftor);
    memory[state.H++] = current_input ^ (TAG_INT << WORD_BITS);
    return unify(stream, ref);
}

function predicate_current_output(stream)
{   var ftor = lookup_functor('$stream', 1);
    var ref = alloc_structure(ftor);
    memory[state.H++] = current_output ^ (TAG_INT << WORD_BITS);
    return unify(stream, ref);
}

function predicate_get_char(stream, c)
{
    var s = {};
    if (!get_stream(stream, s))
        return false;
    return unify(c, lookup_atom(_get_char(s.value)));
}

function predicate_get_code(stream, c)
{
    var s = {};
    if (!get_stream(stream, s))
        return false;
    return unify(c, PL_put_integer(_get_code(s.value)));
}

function predicate_get_byte(stream, c)
{
    var s = {};
    if (!get_stream(stream, s))
        return false;
    return unify(c, PL_put_integer(getb(s.value))); //(getb(s.value) & ((1 << (WORD_BITS-1))-1)) ^ (TAG_INT << WORD_BITS));
}

function predicate_peek_char(stream, c)
{
    var s = {};
    if (!get_stream(stream, s))
        return false;
    return unify(c, lookup_atom(peekch(s.value)));
}

function predicate_peek_code(stream, c)
{
    var s = {};
    if (!get_stream(stream, s))
        return false;
    return unify(c, PL_put_integer(_peek_code(s.value)));
}

function predicate_peek_byte(stream, c)
{
    var s = {};
    if (!get_stream(stream, s))
        return false;
    return unify(c, PL_put_integer(peekb(s.value)));
}

function predicate_put_char(stream, c)
{
    var s = {};
    if (!get_stream(stream, s))
        return false;
    return putch(s.value, atable[VAL(c)]);
}

function predicate_put_code(stream, c)
{
    var s = {};
    if (!get_stream(stream, s))
        return false;
    return putch(s.value, VAL(c));
}

function predicate_put_byte(stream, c)
{
    var s = {};
    if (!get_stream(stream, s))
        return false;
    return putb(s.value, VAL(c));
}

function predicate_flush_output(stream)
{
    var s = {};
    if (!get_stream(stream, s))
        return false;
    if (s.value.write != null)
    {
        return s.value.buffer_size === s.value.write(s.value, 1, s.value.buffer_size, s.value.buffer);
    }
    return permission_error("write", "stream", stream);
}

function predicate_at_end_of_stream(stream)
{
    var s = {};
    if (!get_stream(stream, s))
        return false;
    return (peekch(s.value) !== -1);
}

function predicate_set_stream_position(s, position)
{
    var stream = {};
    if (!get_stream(s, stream))
        return false;
    stream = stream.value;
    if (stream.seek == null)
        return permission_error("seek", "stream", s);
    return stream.seek(stream, VAL(position));
}

/* Actual stream IO */
var streams = [new_stream(null, stdout_write, null, null, null, "")];
function predicate_close(stream)
{
    var s = {};
    if (!get_stream(stream, s))
        return false;
    s = s.value;
    if (s.write != null)
    {
        // Flush output
        // FIXME: If flush fails, then what?
        s.write(s, 1, s.buffer_size, s.buffer);
    }
    if (s.close != null)
    {        
        // FIXME: Ignore s.close(s) if options contains force(true)
        return s.close(s);
    }
    // FIXME: Should be an error
    return false;
}

function get_stream(term, ref)
{
    var fd = {};
    if (!get_stream_fd(term, fd))
        return false;
    ref.value = streams[fd.value];
    return true;
}

function get_stream_fd(term, s)
{
    if (TAG(term) !== TAG_STR)
        return type_error("stream", term);
    let ftor = VAL(memory[VAL(term)]);
    if (atable[ftable[ftor][0]] === "$stream" && ftable_arity(ftor) === 1)
    {
        s.value = VAL(memory[VAL(term)+1]);
        return true;
    }
    return type_error("stream", term);
}

// Streams must all have a buffer to support peeking.
// If the buffer is empty, then fill it via read()
var STREAM_BUFFER_SIZE = 256;

function new_stream(read, write, seek, close, tell, user_data)
{
    return {read: read,
            write: write,
            seek: seek,
            close: close,
            tell: tell,
            data: user_data,
            buffer: [],
            buffer_size: 0};
}

function _get_char(s)
{
    var t = getch(s);
    if (t === -1)
        return "end_of_file";
    else
        return String.fromCharCode(t);
}

function get_raw_char(s)
{
    var t = getch(s);
    if (t === -1)
        return -1;
    else
        return String.fromCharCode(t);
}

function peek_raw_char(s, position)
{
    var t = peekch(s, position);
    if (t === -1)
        return -1;
    else
        return String.fromCharCode(t);
}


function _peek_char(s)
{
    var t = peekch(s);
    if (t === -1)
        return "end_of_file";
    else
        return String.fromCharCode(t);
}

function _get_code(s)
{
    return getch(s);
}

function _peek_code(s)
{
    return peekch(s);
}
// See getch for an explanation of what is going on here
function peekch(s, position)
{
    var b = peekb(s, position);
    var ch;
    if (b === -1)
        return -1;
    // ASCII
    if (b <= 0x7F)
        return b;
    ch = 0;
    var i = 0;
    for (var mask = 0x20; mask !== 0; mask >>=1 )
    {        
        var next = s.buffer[i+1];
        if (next === undefined)
        {
            // This is a problem. We need to buffer more data! But we must also not lose the existing buffer since we are peeking.
            abort("Unicode break in peekch. This is a bug");
        }
        if (next === -1)
            return -1;
        ch = (ch << 6) ^ (next & 0x3f);
        if ((b & mask) === 0)
            break;
        i++;
    }
    ch ^= (b & (0xff >> (i+3))) << (6*(i+1));
    return ch; 
}

function getch(s)
{
    var b = getb(s);
    var ch;
    if (b === -1)
        return -1;
    // ASCII
    if (b <= 0x7F)
        return b;
    ch = 0; 
    // Otherwise we have to crunch the numbers
    var i = 0;
    // The first byte has leading bits 1, then a 1 for every additional byte we need followed by a 0
    // After the 0 is the top 1-5 bits of the final character. This makes it quite confusing.
    for (var mask = 0x20; mask !== 0; mask >>=1 )
    {        
        var next = getb(s);
        if (next === -1)
            return -1;
        ch = (ch << 6) ^ (next & 0x3f);
        if ((b & mask) === 0)
            break;
        i++;
    }
    ch ^= (b & (0xff >> (i+3))) << (6*(i+1));
    return ch;        
}

function putch(s, c)
{
    if (s.buffer_size < 0)
        return io_error("write");
    s.buffer.push(c);
    s.buffer_size++;    
    return true;
}


function putb(s, c)
{
    if (s.buffer_size < 0)
        return io_error("write");
    s.buffer.push(c);
    s.buffer_size++;    
    return true;
}

function getb(s)
{
    if (s.buffer_size === 0)
    {
        s.buffer_size = s.read(s, 1, STREAM_BUFFER_SIZE, s.buffer);
    }
    if (s.buffer_size < 0)
        return s.buffer_size;
    // FIXME: Can this STILL be 0?
    if (s.buffer_size === 0)
        return -1;
    // At this point the buffer has some data in it
    s.buffer_size--;
    return s.buffer.shift();
}

function peekbA(s)
{
    if (s.buffer_size === 0)
    {
        s.buffer_size = s.read(s, 1, STREAM_BUFFER_SIZE, s.buffer);
    }
    if (s.buffer_size < 0)
        return s.buffer_size;
    // FIXME: Can this STILL be 0?
    if (s.buffer_size === 0)
        return -1;
    // At this point the buffer has some data in it
    return s.buffer[0];
}

function peekb(s, position)
{
    let offset = 0;
    if(position) {
        if(position < 1) {
            domain_error('positive_integer', position);
        }
        offset = position - 1;
    }
    if (s.buffer_size >= 0 && s.buffer_size < offset+1)
    {
        let extend = offset+1 - s.buffer_size;
        let newChars = [];
        s.read(s, extend, STREAM_BUFFER_SIZE, newChars);
        s.buffer = s.buffer.concat( newChars);
        s.buffer_size = s.buffer.length;
    }
    if (s.buffer_size < offset+1) // end-of-stream reached.
        return -1;
    // At this point the buffer has some data in it
    return s.buffer[offset];
}

function get_stream_position(stream, property)
{
    if (stream.tell != null)
    {
        var p = stream.tell(stream) - stream.buffer.length;
        var ftor = lookup_functor('position', 1);
        var ref = alloc_structure(ftor);
        memory[state.H++] = p ^ (TAG_INT << WORD_BITS);
        return unify(ref, property);
    }
    return false;
}

var stream_properties = [get_stream_position];

function predicate_stream_property(stream, property)
{
    var s = {};
    if (!get_stream(stream, s))
        return false;
    stream = s.value;
    var index = 0;
    if (state.foreign_retry)
    {
        index = state.foreign_value+1;
    }
    else
    {
        create_choicepoint();        
    }
    update_choicepoint_data(index);
    
    if (index >= stream_properties.length)
    {
        destroy_choicepoint();
        return false;
    }    
    return stream_properties[index](stream, property)
}

function predicate_current_stream(stream)
{
    var index = 0;
    if (state.foreign_retry)
    {
        index = state.foreign_value+1;
    }
    else
    {
        create_choicepoint();        
    }
    while (streams[index] === undefined)
    {
        if (index >= streams.length)
        {
            destroy_choicepoint();
            return false;
        }    
        index++;
    }
    update_choicepoint_data(index);
    var ftor = lookup_functor('$stream', 1);
    var ref = alloc_structure(ftor);
    memory[state.H++] = index ^ (TAG_INT << WORD_BITS);
    return unify(stream, ref);   
}
// File node_files.js
'use strict';
var stream;
var fs;
var has_require = typeof require !== 'undefined';
if(has_require) {
    // noinspection NodeJsCodingAssistanceForCoreModules
    stream = require('stream');
    // noinspection NodeJsCodingAssistanceForCoreModules
    fs = require('fs');
}

// This file is for execution under nodejs.
// The presence of 'require' is used to indicate if the
// current execution environment is NodeJS or not.
//
// Since JavascriptCore and Browser environments do not support
// file IO, the predicate_open function just raises an exception
// in those environments.

function predicate_open (file, mode, stream, options) {
    return engine_error('open/4 is not defined outside of nodejs.');
}

function node_write_file(stream, size, count, buffer) {
    var bytes_written = 0;
    var records_written;
    var file = stream.data;
    if (size === 1) {
        let record = Buffer.from(buffer);
        file.write(record);
        records_written = count;
    } else {
        for (records_written = 0; records_written < count; records_written++) {
            let record = new Buffer(buffer.splice(0, size));
            file.write(record);

        }
    }
    return records_written;
}


// function node_read_file(stream, size, count, buffer) {
//     var bytes_read = 0;
//     var records_read;
//     var file = stream.data;
//     let resultBuffer = file.read(count * size);
//     let position = 0;
//     for (records_read = 0; records_read < count; records_read++) {
//         for (var b = 0; b < size; b++) {
//             var t = resultBuffer[position++];
//             if (t === undefined)
//                 return records_read;
//             buffer[bytes_read++] = t;
//         }
//     }
//     return records_read;
// }

function node_read_file(stream, size, count, buffer) {
    var bytes_read = 0;
    var records_read;
    var fileDescriptor = stream.data;
    var resultBuffer = Buffer.allocUnsafe(count * size);

    resultBuffer.fill(0);

    var readCount = fs.readSync(fileDescriptor, resultBuffer, null, count * size);
    let position = 0;
    for (records_read = 0; records_read < count && position < readCount; records_read++) {
        for (var b = 0; b < size && position < readCount; b++) {
            var t = resultBuffer[position++];
            if (t === undefined)
                return records_read;
            buffer[bytes_read++] = t;
        }
    }

    return records_read;
}

function node_close_file(stream) {
    if(typeof stream.data === 'object' && 'end' in stream.data) {
        stream.data.end();
    } else {
        fs.closeSync(stream.data);
    }
    stream.data = undefined;
    return true;
}

function test_write_to_file() {
    const file = fs.createWriteStream('example.txt');
    const testData = "test data2";
    const x = toUTF8Array(testData);

    node_write_file({data: file}, 1, x.length, x);
}

//    test_write_to_file();

if(has_require) {

    predicate_open = function (file, mode, stream, options) {
        var index = streams.length;

        if (TAG(file) === TAG_REF)
            return instantiation_error(file);
        if (TAG(file) !== TAG_ATM)
            return type_error("file_path", file);
        let fileJS = PL_get_atom_chars(file);

        if (TAG(mode) === TAG_REF)
            return instantiation_error(mode);
        else if (TAG(mode) !== TAG_ATM)
            return type_error("atom", mode);
        let modeJS = PL_get_atom_chars(mode);

        if (modeJS === 'read') {
            //const fileStream = fs.createReadStream(fileJS);
            const fileDescriptor = fs.openSync(fileJS, 'r');
            streams[index] = new_stream(node_read_file, null, null, node_close_file, null, fileDescriptor);

        } else if (atable[VAL(mode)] === 'write') {
            const fileStream = fs.createWriteStream(fileJS);
            streams[index] = new_stream(null, node_write_file, null, node_close_file, null, fileStream);
        } else
            return type_error("io_mode", mode);

        var ftor = lookup_functor('$stream', 1);
        var ref = alloc_structure(ftor);
        memory[state.H++] = index ^ (TAG_INT << WORD_BITS);
        return unify(stream, ref);
    };
}
// else {
//     predicate_open = function (file, mode, stream, options) {
//         return engine_error('open/4 is not defined outside of nodejs.');
//     }
// }

// from https://gist.github.com/lihnux/2aa4a6f5a9170974f6aa

function toUTF8Array(str) {
    let utf8 = [];
    for (let i = 0; i < str.length; i++) {
        let charcode = str.charCodeAt(i);
        if (charcode < 0x80) utf8.push(charcode);
        else if (charcode < 0x800) {
            utf8.push(0xc0 | (charcode >> 6),
                0x80 | (charcode & 0x3f));
        } else if (charcode < 0xd800 || charcode >= 0xe000) {
            utf8.push(0xe0 | (charcode >> 12),
                0x80 | ((charcode >> 6) & 0x3f),
                0x80 | (charcode & 0x3f));
        }
        // surrogate pair
        else {
            i++;
            // UTF-16 encodes 0x10000-0x10FFFF by
            // subtracting 0x10000 and splitting the
            // 20 bits of 0x0-0xFFFFF into two halves
            charcode = 0x10000 + (((charcode & 0x3ff) << 10)
                | (str.charCodeAt(i) & 0x3ff));
            utf8.push(0xf0 | (charcode >> 18),
                0x80 | ((charcode >> 12) & 0x3f),
                0x80 | ((charcode >> 6) & 0x3f),
                0x80 | (charcode & 0x3f));
        }
    }
    return utf8;
}
// File gc.js
"use strict";

let gc_environment = 'browser'; // 'console'
let display_gc = false;
let display_minor_gc = false;

function predicate_gc()
{
    let msgIn = "Before GC, heap is " + state.H;
    //stdout("===" + msgIn + "\n" + '\n');
    gc_main_debug(msgIn);
    // WARNING: This assumes ONLY predicate_gc will mark things!
    total_marked = 0;

    let before;
    let after;

    if(display_gc) {
        // debugging only

        before = [];
        let e = state.E;
        let envsize = state.CP.code[state.CP.offset - 1];
        while (e !== HEAP_SIZE) {
            for (let i = 0; i < envsize; i++) {
                before.push(record_term(memory[e + 2 + i]));
            }
            let envcp = memory[e + 1];
            envsize = envcp.code[envcp.offset - 1];
            e = memory[e];
        }

//        gc_debug('===check stacks before mark');
        check_stacks(false);
    }
    mark();

//    gc_debug('===check stacks after mark');
//    check_stacks(true);

    push_registers();
    //   gc_debug('after push_registers, before sweep_trail');
    sweep_trail();
//    gc_debug('after sweep_trail, before sweep_stack');
    sweep_stack();


//    gc_debug('after sweep_stack, before compact');
    compact();
//    gc_debug('after compact, before pop_registers');
    pop_registers();
//    gc_debug('after pop_registers');
    state.H = total_marked;
    if(display_gc) {
//    gc_debug('===check stacks after compact/pop_registers');
//    check_stacks(false);

        after = [];
        let e = state.E;
        let envsize = state.CP.code[state.CP.offset - 1];
        while (e !== HEAP_SIZE) {
            for (let i = 0; i < envsize; i++) {
                after.push(record_term(memory[e + 2 + i]));
            }
            let envcp = memory[e + 1];
            envsize = envcp.code[envcp.offset - 1];
            e = memory[e];
        }
    }
    if (total_marked !== 0)
    {
    }
    if(display_gc) {
        while (before.length !== 0) {
            let a = before.pop();
            let b = after.pop();
            let at = recall_term(a, {});
            let bt = recall_term(b, {});
            if (!predicate_unify(at, bt)) {
                gc_main_debug("Error: Terms in environment changed during GC!");
                gc_main_debug("at = " + term_to_string(at));
                gc_main_debug("bt = " + term_to_string(bt));
                abort("false");
            }
        }


        // display_gc_log();
    }
    let msgOut = "After GC, heap is " + state.H;
    //stdout( msgOut + '\n');
    gc_main_debug(msgOut);

    return true;
}

function push_registers()
{
    for (var i = 0; i < state.num_of_args; i++)
    {
        memory[state.TR++] = register[i];
    }
}

function pop_registers()
{
    for (var i = state.num_of_args-1; i >= 0; i--)
    {
        register[i] = memory[--state.TR];
    }
}

function sweep_trail()
{
    for (var current = state.TR-1; current >= HEAP_SIZE + STACK_SIZE; current--)
    {
        if (IS_HEAP_PTR(memory[current]))
        {
            into_relocation_chain(VAL(memory[current]), current);
        }
        else
        {
        }
    }
}

function sweep_stack()
{
    sweep_environments(state.E, state.CP.code[state.CP.offset - 1]);
    sweep_choicepoints();
}

function sweep_environments(e, envsize)
{
    while (e !== HEAP_SIZE)
    {
        // Traversing backwards to ensure we do not stop prematurely
        for (var y = envsize-1; y >= 0; y--)
        {
            if (IS_HEAP_PTR(memory[e+2 + y]))
            {
                if ((memory[e+2 + y] & M_BIT) === 0)
                {
                    // we have already swept this chain
                    return;
                }
                else 
                {
                    memory[e+2 + y] &= ~M_BIT;
                    into_relocation_chain(VAL(memory[e+2+y]), e+2+y);
                }
            }
        }
        var envcp = memory[e+1];
        // work out the size of the previous environment, using the CP pointer saved in THIS environment.
        // This is why we had to pass size in to mark_environments()
        envsize = envcp.code[envcp.offset-1];
        e = memory[e];
    }
}

function sweep_choicepoints()
{
    var b = state.B;
    while (b !== 0)
    {
        var cpcp = memory[b + memory[b] + 2];        
        var envsize = cpcp.code[cpcp.offset-1];
        sweep_environments(memory[b + memory[b] + 1], envsize);
        for (var y = 0; y < memory[b]; y++)
        {
            if (IS_HEAP_PTR(memory[b+y+1]))
            {
                memory[b+y+1] &= ~M_BIT;
                into_relocation_chain(VAL(memory[b+y+1]), b+y+1);
            }
        }
        if ((memory[memory[b + memory[b] + 6]] & M_BIT) === 0)
        {
            // The choicepoint has a saved value for H (ie HB) which is not marked
            // Make a fake atom on the heap and change the HB to point to it
            memory[memory[b + memory[b] + 6]] = NIL ^ (M_BIT);
            total_marked++;
        }
        into_relocation_chain(memory[b + memory[b] + 6], b + memory[b] + 6);
        b = memory[b + memory[b] + 3];
    }
}

function mark()
{
    mark_registers();
    mark_environments(state.E, state.CP.code[state.CP.offset - 1]);
    mark_choicepoints();
}

function compact()
{
    var dest;
    var current;
    dest = total_marked - 1;
    gc_debug('before compact upward');
    // Upward
    for (current = state.H-1; current >= 0; current--)
    {
        if ((memory[current] & M_BIT) === M_BIT)
        {
            //gc_debug('compact - before update_relocation_chain('+current+', '+dest+')')
            update_relocation_chain(current, dest);
            if (IS_HEAP_PTR(memory[current]))
            {
                if (VAL(memory[current]) < current)
                {
                    //gc_debug('compact - after update_relocation_chain, before into_relocation_chain('+VAL(memory[current])+', '+current+')')
                    into_relocation_chain(VAL(memory[current]), current);
                }
                else if (VAL(memory[current]) === current)
                {
                    memory[current] = (memory[current] & NV_MASK) ^ dest;
                }
            }
            dest--;
        }
    }

    gc_debug('after compact upward, before downward');
    // Downward
    dest = 0;
    for (current = 0; current < state.H; current++)
    {
        if ((memory[current] & M_BIT) === M_BIT)
        {
            update_relocation_chain(current, dest);
            if (IS_HEAP_PTR(memory[current]) && VAL(memory[current]) > current)
            {
                into_relocation_chain(VAL(memory[current]), dest);

                memory[dest] = VAL(memory[dest]) ^ (TAG(memory[current]) << WORD_BITS);
            }
            else
            {
                memory[dest] = memory[current];
                // clear the GC flags
                memory[dest] &= ~F_BIT;
            }
            memory[dest] &= ~M_BIT;
            dest++;
        }
    }
    gc_debug('after compact downward');
}

function update_relocation_chain(current, dest)
{
    let localDebug = false; //(dest === 20);

    if(localDebug) {
        gc_debug('start update_relocation_chain(' + current + ', ' + dest + ')');
    }

    var j;
    while ((memory[current] & F_BIT) === F_BIT)
    {
        j = VAL(memory[current]);
        if(localDebug) {
            gc_debug('update_relocation_chain - memory[current]=' + memory[current] + ', j=' + j);
            gc_debug('memory[j]=' + memory[j] + ', VAL(memory[j])=' + VAL(memory[j]));
            gc_debug('(memory[current] & (NV_MASK ^ F_BIT))=' + (memory[current] & (NV_MASK ^ F_BIT))
                + ', (VAL(memory[j]) ^ (memory[current] & (NV_MASK ^ F_BIT)))=' + (VAL(memory[j]) ^ (memory[current] & (NV_MASK ^ F_BIT)))
                + ', (memory[j] & F_BIT)=' + (memory[j] & F_BIT));
        }
        memory[current] = (VAL(memory[j]) ^ (memory[current] & (NV_MASK ^ F_BIT))) | (memory[j] & F_BIT);
        if(localDebug) {
            gc_debug('new memory[current]=' + memory[current]);
            gc_debug('dest ^ (memory[j] & NV_MASK)='+(dest ^ (memory[j] & NV_MASK)));
        }
        memory[j] = dest ^ (memory[j] & NV_MASK);
        if(localDebug) {
            gc_debug('new memory[j]=' + memory[j]);
        }
        memory[j] &= ~F_BIT;
        if(localDebug) {
            gc_debug('new new memory[j]= (new memory[j] &= ~F_BIT)' + memory[j]);
        }
    }
}

function into_relocation_chain(j, current)
{
    memory[current] = VAL(memory[j]) ^ (memory[current] & (NV_MASK ^ F_BIT)) | (memory[j] & F_BIT);
    memory[j] = current ^ (memory[j] & NV_MASK);
    memory[j] |= F_BIT;        
}

/**
 * @return {boolean}
 */
function IS_HEAP_PTR(x)
{
    var tag = TAG(x);
    return (tag === TAG_STR || tag === TAG_LST || tag === TAG_REF) && (VAL(x) < HEAP_SIZE);
}

// Mark all the cells reachable from the registers as reachable (ie set their M bits)
function mark_registers()
{
    for (var i = 0; i < state.num_of_args; i++)
    {
        if (IS_HEAP_PTR(register[i]))
        {
            // register refers to the heap. We have to temporarily put this onto the heap since mark_variable
            // expects an address (ie an index into memory[]) and register[i]
            var tmp = state.H;
            if (state.H === HEAP_SIZE)
                abort("Out of heap during GC");
            memory[state.H++] = register[i];
            mark_variable(tmp);
            state.H--; // We can just clean it up now, since mark_variable is not allowed to write to memory[]
        }
    }
}

// Mark all the cells reachable from the environment 'initial'.
// Note that this takes into account LCO: Trimmed cells are ignored.
// If these are actually needed, mark_choicepoints() will find them
function mark_environments(initial, envsize)
{
    var e = initial;
    while (e !== HEAP_SIZE)
    {
        // Traversing backwards to ensure we do not stop prematurely
        for (var y = envsize-1; y >= 0; y--)
        {
            if ((memory[e+2 + y] & M_BIT) === M_BIT)
            {
                // we have already done this chain
                return;
            }
            else if (IS_HEAP_PTR(memory[e+2 + y]))
            {
                // Y-register refers to the heap
                mark_variable(e+2 + y);
            }
            else
            {
            }
        }
        var envcp = memory[e+1];
        // work out the size of the previous environment, using the CP pointer saved in THIS environment.
        // This is why we had to pass size in to mark_environments()
        envsize = envcp.code[envcp.offset-1];
        e = memory[e];
    }
}

function mark_choicepoints()
{
    var b = state.B;
    while (b !== 0)
    {
        var cpcp = memory[b + memory[b] + 2];
        var envsize = cpcp.code[cpcp.offset-1];
        mark_environments(memory[b + memory[b] + 1], envsize);
        for (var y = 0; y < memory[b]; y++)
        {
            if (IS_HEAP_PTR(memory[b+y+1]))
            {
                // Y-register refers to the heap
                mark_variable(b + y + 1);
            }
        }
        b = memory[b + memory[b] + 3];
    }
}

var total_marked = 0;

// start is an address: That is, an index into memory[]. It is NOT a cell, so it does NOT have a tag!
// Also, it must be the address of something which is a pointer. That is, VAL(memory[start]) must be another index into memory[].
function mark_variable(start)
{
    let current = start;
    let next = VAL(memory[current]);
    memory[current] |= F_BIT;
    // mark_variable is always called with something which is either not on the heap
    // or not /really/ on the heap, in the case of register values. Therefore, when we count
    // the first thing, we should increment total_marked to 0, not 1.
    total_marked--;

    while(true) // unwrap goto into while loops
    {
        while (true) // forward
        {
            if ((memory[current] & M_BIT) === M_BIT)
                break; // goto backward
            memory[current] |= M_BIT;
            total_marked++;
            switch(TAG(memory[current]))
            {
            case TAG_REF: // Transformation 1
                if ((memory[next] & F_BIT) === F_BIT)
                {
                    break; // goto backward
                }
                // REVERSE(current, next);
            {
                let temp = VAL(memory[next]);

                memory[next] = (memory[next] & NV_MASK) ^ current;
                current = next;
                next = temp;
            }
                continue; // goto forward
            case TAG_STR: // Transform 2a
            case TAG_LST: // Transform 2b
                if ((memory[next+1] & F_BIT) === F_BIT)
                    break; // goto backward
                // Optimisation: We can skip the structure if we have already marked all its arguments
                // FIXME: Implement

                if (TAG(memory[current]) === TAG_STR)
                {
                    var i;
                    for (i = 0; i < ftable[VAL(memory[next])][1]; i++)
                    {
                        memory[next+1+i] |= F_BIT;
                    }
                    next = next+i;
                }
                else
                {
                    memory[next+1] |= F_BIT;
                    next = next+1;
                }
                //REVERSE(current, next);
            {
                let temp = VAL(memory[next]);
                memory[next] = (memory[next] & NV_MASK) ^ current;
                current = next;
                next = temp;
            }
                continue; // goto forward
            default:
                // All other types: INT, ATM, FLT, etc
                // Transformation 3
                break; // goto backward
            }
            break; // if we get to the end of forward, we must be wanting to go to backward
        }
        while (true) // backward
        {
            if ((memory[current] & F_BIT) !== F_BIT)
            {                
                // current is an internal cell
                // Transformation 4
                //UNDO(current, next);
                var temp = VAL(memory[current]);
                //var tag = TAG(memory[next]);
                memory[current] = (memory[current] & NV_MASK) ^ next;
                next = current;
                current = temp;
                continue; // goto backward
            }
            // current is the head of a chain
            memory[current] &= ~F_BIT;
            if (current === start)
            {
                // current is the head of the chain we started with. Finished!
                return;
            }
            // Otherwise, current is the head of a subchain
            current--; // Transformation 5
            //ADVANCE(current, next);
            {
                let temp = VAL(memory[current + 1]);
                memory[current + 1] = (memory[current + 1] & NV_MASK) ^ next;
                next = VAL(memory[current]);
                memory[current] = (memory[current] & NV_MASK) ^ temp;
            }
            break; // goto forward
        }
    }
}



function gc_test(d)
{
    debugging = d;
    load_state();
    initialize();
    stdout("Loaded " + Object.keys(predicates).length + " predicates");
    stdout("Loaded " + atable.length + " atoms");
    stdout("Loaded " + ftable.length + " functors");
    stdout("Loaded " + code.length + " bytes of code");
    call_directives();

    memory[0] = 0x20000088;
    memory[1] = 0x20000071;
    memory[2] = 0x20000072;
    state.H = 3;
    state.CP.code[state.CP.offset - 1] = 1;
    memory[state.E + 2] = 0x8000000;
    mark_variable(state.E+2);

    compact();
}

function dump_heap()
{
    for (var i = 0; i < state.H; i++)
    {
    }
}

function dump_registers()
{
    for (var i = 0; i < state.num_of_args; i++)
    {
    }
}


function predicate_statistics()
{
    let aggregateDuration = statistics_wam_duration();
    let heapSize = statistics_heap_size();
    let maxHeap = statistics_max_heap();
    let maxStack = statistics_max_stack();
    let stackPortion = maxStack - HEAP_SIZE;

    stdout("WAM duration: " + aggregateDuration + "\n");
    stdout("Current heap: " + heapSize + "\n");
    stdout("Max heap: " + maxHeap + " (words of " + HEAP_SIZE + " limit)\n");
    stdout("Max stack: " + stackPortion + " (words of " + STACK_SIZE + " limit, " + maxStack + " absolute)\n");
    return true;
}

function predicate_wam_duration(duration) {
    var aggregateDuration = statistics_wam_duration();

    return unify_number(aggregateDuration, duration);
}

function statistics_wam_duration() {
    var activeWamDuration = 0;

    if(activeWamStartTime) {
        var now = Date.now();
        activeWamDuration = now - activeWamStartTime;
    }

    return activeWamDuration + wamDuration; // wamDuration may be float due to 0.1 fudge values being added.
}

function predicate_statistics_heap_size(size) {
    var heapSize = statistics_heap_size();

    return unify_number(heapSize, size);
}

function statistics_heap_size() {
    return state.H;
}

function predicate_statistics_max_stack(maxStackPL) {
    var maxStackJS = statistics_max_stack();

    return unify_number(maxStackJS, maxStackPL);
}

function statistics_max_stack() {
    return maxStackSize;
}

function predicate_statistics_max_heap(maxHeapPL) {
    var maxHeapJS = statistics_max_heap();

    return unify_number(maxHeapJS, maxHeapPL);
}

function statistics_max_heap() {
    return maxHeapSize;
}

function unify_number(numberJS, numberPL) {
    if(Number.isInteger(numberJS) && numberJS <= (2**(WORD_BITS-1)-1) && numberJS >= -1*(2**(WORD_BITS-1)-1)) {
        return PL_unify_integer(numberPL, numberJS);
    } else {
        return PL_unify_float(numberPL, numberJS);
    }
}

function gc_check(t)
{
    // noinspection JSBitwiseOperatorUsage
    if (t & M_BIT)
        abort("GC exception: " + hex(t) + " has M_BIT set");
}

function check_stacks(m)
{
    gc_debug("Checking stacks " + m);
    check_environments(state.E, state.CP.code[state.CP.offset - 1], m);
    gc_debug("Stacks OK");
}

function check_environments(initial, envsize, m)
{
    var e = initial;
    while (e !== HEAP_SIZE)
    {
        // Traversing backwards to ensure we do not stop prematurely
        gc_debug("Checking environment " + e);
        for (var y = 0; y < envsize; y++)
        {            
            if (TAG(memory[e+2+y]) === TAG_STR ||
                TAG(memory[e+2+y]) === TAG_LST)
            {
                gc_debug("Checking Y" + y);
                check_term(memory[e+2+y], m);
            }
            else 
            {
                gc_debug("Y" + y + " is not a heap pointer");
            }
            // Otherwise we do not need to check it if it is in the environment
        }
        var envcp = memory[e+1];
        // work out the size of the previous environment, using the CP pointer saved in THIS environment.
        // This is why we had to pass size in to mark_environments()
        envsize = envcp.code[envcp.offset-1];
        e = memory[e];
    }
}

function check_term(t, m)
{
    gc_debug("Checking " + hex(t));
    if (!m)
    {
        gc_debug(" == " + term_to_string(t));
    }
    if ((t & M_BIT) === M_BIT)
    {
        if (!m)
            abort("Term " + hex(t) + " is marked but should not be");
    }
    else if (m)
    {
        abort("Term " + hex(t) + " is not marked but is reachable");
    }
    if ((t & F_BIT) === F_BIT)
    {
        if (!m)
            abort("Term " + hex(t) + " is F but should not be");
    }

    if (TAG(t) === TAG_LST)
    {
        if (VAL(t) > state.H)
            abort("Term " + hex(t) + " exceeds heap: " + state.H);
        check_term(memory[VAL(t)], m);
        check_term(memory[VAL(t)+1], m);
    }
    else if (TAG(t) === TAG_STR)
    {
        if (VAL(t) > state.H)
            abort("Term " + hex(t) + " exceeds heap: " + state.H);        
        if (ftable[VAL(memory[VAL(t)])] === undefined)
            abort("Illegal functor " + VAL(memory[VAL(t)]));
        var arity = ftable[VAL(memory[VAL(t)])][1];
        for (var i = 0; i < arity; i++)
            check_term(memory[VAL(t)+1+i], m);
    }
    // Everything else we assume is OK
}

let gc_log = [];

// gc_log is a circular log of that last 95 gc_debug(msg) calls.

function add_to_gc_log(msg) {
    if(gc_log.length === 96) {
        gc_log.shift();
    } else if(gc_log.length > 96) {
        let shift = gc_log.length - 95;
        gc_log = gc_log.slice(shift);
    }
    gc_log.push(msg);
}

function gc_main_debug(msg) {
    if(display_gc) {
        alert('gc: ' + msg);
//        display_backtrack = true;
    }
//    add_to_gc_log(msg);
}

function gc_debug(msg) {
    if(display_minor_gc) {
        alert('gc: ' + msg);
//        display_backtrack = true;
    }
//    add_to_gc_log(msg);
}

function display_gc_log() {
    // for(let msg of gc_log) {
    //     stdout('gc_debug: ' + msg + '\n');
    // }
}

function dump_environments(initial, envsize, cp)
{
    if(! initial){
        dump_environments(state.E, state.CP.code[state.CP.offset - 1], state.CP);
        return;
    }

    let e = initial;
    let envcp = cp;
    while (e !== HEAP_SIZE)
    {
        let predicate = functor_offset_to_predicate_string(envcp.predicate.key);

        gcWrite('environment at ' + e + ' has ' + envsize + ' slots. predicate=' + predicate + ', cp.predicate.key=' + envcp.predicate.key + ', cp.offset=' + envcp.offset);
        envcp = memory[e+1];
        if(typeof envcp === 'undefined' ) {
            gcWrite('CP is undefined at ' + e+1);
            break;
        } else if(typeof envcp.code === 'undefined' ) {
            gcWrite('CP.code is undefined for envcp ' + envcp + ' at ' + e+1);
            break;
        } else if(typeof envcp.offset === 'undefined' ) {
            gcWrite('CP.offset is undefined for envcp ' + envcp + ' at ' + e+1);
            break;
        }


        // work out the size of the previous environment, using the CP pointer saved in THIS environment.
        // This is why we had to pass size in to mark_environments()
        envsize = envcp.code[envcp.offset-1];
        e = memory[e];
    }
}

function functor_offset_to_predicate_string(ftor) {
    let functorPair = ftable[ftor];
    let predicate;
    if(functorPair) {
        let predicateName = atable[functorPair[0]];
        let predicateArity = functorPair[1];
        predicate = predicateName + '/' + predicateArity;
    }
    return predicate;
}

function dump_choicepoints()
{
    let b = state.B;
    while (b !== 0)
    {
        b = dump_choicepoint(b);
    }
}

function dump_choicepoint(b) {
    let dynamicPortionSize = memory[b];

    let backtrack_cp = memory[b + dynamicPortionSize + CP_CP];
    var backtrack_envsize = backtrack_cp.code[backtrack_cp.offset-1];
    //dump_environments(memory[b + dynamicPortionSize + CP_E], envsize, backtrack_cp);
    let backtrack_predicate = functor_offset_to_predicate_string(backtrack_cp.predicate.key);
    let backtrack_offset = backtrack_cp.offset;

    let choice_cp = memory[b + dynamicPortionSize + CP_Next];
    var choice_envsize = choice_cp.code[choice_cp.offset-1];
    let choice_predicate = functor_offset_to_predicate_string(choice_cp.predicate.key);
    let choice_offset = choice_cp.offset;

    gcWrite('choicepoint at ' + b + ' has ' + dynamicPortionSize + ' dynamic slots. choice predicate=' + choice_predicate + ' (offset ' + choice_offset + '), backtrack predicate=' + backtrack_predicate + ' (offset ' + backtrack_offset + '), backtrack environment=' + memory[b + dynamicPortionSize + CP_E]);

    return memory[b + dynamicPortionSize + CP_B];
}

function gcWrite(msg) {
    if(gc_environment === 'console') {
        print(msg);
    } else if(gc_environment === 'browser') {
        console.log(msg);
    }
}
// File dom.js
'use strict';

/*
predicates to read and modify the javascript DOM for HTML
 */

/** @namespace Map */

var deavCursors = new Map();
var deavCursorCounter = 0;
var desaCursors = new Map();
var desaCursorCounter = 0;


function predicate_set_dom_element_attribute_value(element, attribute, value) {
    if (TAG(element) !== TAG_STR) {
        return instantiation_error(element);
    }

    if (TAG(attribute) !== TAG_ATM) {
        return instantiation_error(attribute);
    }

    if (TAG(value) !== TAG_ATM) {
        return instantiation_error(value);
    }

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        return false;
    }
    var elementJS = elementObject.value;

    var attributeJS = atable[VAL(attribute)];
    var valueJS = atable[VAL(value)];

    if(attributeJS === 'class') {
        elementJS.classList.add(valueJS);
    } else {
        elementJS.setAttribute(attributeJS, valueJS);
    }
    return true;
}

function predicate_remove_dom_element_class(element, value) {
    if (TAG(element) !== TAG_STR) {
        return instantiation_error(element);
    }

    if (TAG(value) !== TAG_ATM) {
        return instantiation_error(value);
    }

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        return false;
    }
    var elementJS = elementObject.value;

    var valueJS = atable[VAL(value)];

    elementJS.classList.remove(valueJS);
    return true;
}

function predicate_replace_dom_element_class(element, oldValue, value) {
    if (TAG(element) !== TAG_STR) {
        return instantiation_error(element);
    }

    if (TAG(oldValue) !== TAG_ATM) {
        return instantiation_error(oldValue);
    }

    if (TAG(value) !== TAG_ATM) {
        return instantiation_error(value);
    }

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        return false;
    }
    var elementJS = elementObject.value;

    var oldValueJS = atable[VAL(oldValue)];
    var valueJS = atable[VAL(value)];

    elementJS.classList.replace(oldValueJS, valueJS);
    return true;
}

function predicate_toggle_dom_element_class(element, value, action) {
    if (TAG(element) !== TAG_STR) {
        return instantiation_error(element);
    }

    if (TAG(value) !== TAG_ATM) {
        return instantiation_error(value);
    }

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        return false;
    }
    var elementJS = elementObject.value;

    var valueJS = atable[VAL(value)];
    var actionJS;
    var flag;

    if(TAG(action) === TAG_REF) {
        flag = elementJS.classList.toggle(valueJS);
        if(flag) {
            actionJS = 'add';
        } else {
            actionJS = 'remove';
        }
        bind(action, lookup_atom(actionJS))
    } else {
        if (TAG(action) !== TAG_ATM) {
            return instantiation_error(action);
        }

        actionJS = atable[VAL(action)];
        if(actionJS === 'add') {
            flag = true;
        } else if(actionJS === 'remove') {
            flag = false;
        } else {
            return domain_error(action);
        }

        elementJS.classList.toggle(valueJS, flag);
    }
    return true;
}

// dom_element_attribute_value(E, A, V)
// Allows any combination of bindings of E, A, and V, including none bound.
// If E is unbound and A and V are bound then predicate_dom_element_attribute_value has two
// strategies for finding E: if A and V are bound and A is 'id', 'name', or 'class'
// then use specific getElementById(V), getElementsByName(V), or getElementsByClass(V) method
// where A is 'id', 'name', or 'class', respectively; otherwise, get all
// elements using document.querySelectorAll('*') and check each one using element.getAttribute(A)=V.
//
// If A is unbound then predicate_dom_element_attribute_value checks each possible E
// using element.getAttributeNames() to generate all values for A for each E. For each A and E value
// the value is checked/retrieved. As above, 'id', 'name',
// and 'class' attributes are handled specially; all other attributes are handled
// using element.getAttribute(A)=V.

function predicate_dom_element_attribute_value(element, attribute, value) {
    var cursor;
    var cursorIDPL;
    var cursorIDJS;

    if (state.foreign_retry) {
        cursorIDPL = state.foreign_value;
        cursorIDJS = atable[VAL(cursorIDPL)];
        cursor = deavCursors.get(cursorIDJS);

    }
    else {
        let container = {};
        if(!setupValues(element, attribute, value, container)) {
            return false;
        }
        cursor = {
            elements: setupElements(element, attribute, value),
            attributes: setupAttributes(element, attribute),
            values: container.value
        };
        cursorIDJS = 'crs' + deavCursorCounter++;
        deavCursors.set(cursorIDJS, cursor);
        cursorIDPL = lookup_atom(cursorIDJS);

        create_choicepoint();
    }

    update_choicepoint_data(cursorIDPL);

     if (cursor.elements && cursor.elements.length > 0) {
        var elementJS = cursor.elements[0];

        var elementPL = create_element_structure(elementJS);

        if (!cursor.attributes) {
            cursor.attributes = setupAttributesFromJSElement(elementJS, attribute);
        }

        if (cursor.attributes && cursor.attributes.length > 0) {
            var attributeJS = cursor.attributes[0];
            var attributePL = lookup_atom(attributeJS);
            if (!cursor.values) {
                cursor.values = setupValuesFromJSElementAndAttribute(elementJS, attributeJS);
            }

            if (cursor.values && cursor.values.length > 0) {
                var valueJS = cursor.values.pop();
                var valuePL = lookup_atom(valueJS);
                return (unify(value, valuePL) &&
                    unify(attribute, attributePL) &&
                    unify(element, elementPL));
            }

            // All values for the current attributeJS have been processed.
            // Set the cursor.values to undefined to force recalculation of values
            // with next attribute.
            // Move to the next attribute by removing attributes[0].

            cursor.values = undefined;
            cursor.attributes = cursor.attributes.slice(1);
            return false; // go to next choice (of attribute)
        }

        // All attributes for current elementJS have been processed.
        // Set the cursor.attributes to undefined to force recalculation of attributes
        // with next element.
        // Move to the next element by removing elements[0].

        cursor.attributes = undefined;
        cursor.elements = cursor.elements.slice(1);
        return false; // go to next choice (of element)
    } else {
        destroy_choicepoint();
        return false;
    }
}

function setupValues(element, attribute, value, container) {
    var values = [];

    var valueJS;

    // if (TAG(value) !== TAG_REF) {
    //     if (TAG(value) !== TAG_ATM) {
    //         instantiation_error(value);
    //     }
    //     valueJS = atable[VAL(value)];
    //     values.push(valueJS);
    // } else
    if (TAG(element) !== TAG_REF && TAG(attribute) !== TAG_REF) {
        if (TAG(element) !== TAG_STR) {
            return instantiation_error(element);
        }

        if (TAG(attribute) !== TAG_ATM) {
            return instantiation_error(attribute);
        }

        var elementObject = {};
        if (!get_element_object(element, elementObject)) {
            container.value = undefined;
            return true;
        }
        var elementJS = elementObject.value;
        var attributeJS = atable[VAL(attribute)];

        if (attributeJS === 'class') {
            values = Array.from(elementJS.classList);
        } else {
            valueJS = elementJS.getAttribute(attributeJS);
            if (valueJS) {
                values.push(valueJS);
            } else {
                values = undefined;
            }
        }
    } else {
        values = undefined;
    }
    container.value = values;
    return true;
}

function setupValuesFromJSElementAndAttribute(elementJS, attributeJS) {
    var values = [];
    var valueJS;

    // if (TAG(value) !== TAG_REF) {
    //     if (TAG(value) !== TAG_ATM) {
    //         instantiation_error(value);
    //     }
    //     valueJS = atable[VAL(value)];
    //     values.push(valueJS);
    // } else
    if (attributeJS === 'class') {
        values = Array.from(elementJS.classList);
    } else {
        valueJS = elementJS.getAttribute(attributeJS);
        if(valueJS) {
            values.push(valueJS);
        } else {
            values = undefined;
        }
    }
    return values;
}

function setupAttributes(element, attribute) {
    var attributes = [];
    if (TAG(attribute) !== TAG_REF) {
        if (TAG(attribute) !== TAG_ATM) {
            instantiation_error(attribute);
        }

        attributes.push(atable[VAL(attribute)]);
    } else if (TAG(element) !== TAG_REF) {
        if (TAG(element) !== TAG_STR) {
            instantiation_error(element);
        }
        var elementObject = {};
        if (!get_element_object(element, elementObject)) {
            return undefined;
        }
        var elementJS = elementObject.value;
        /** @namespace elementJS.getAttributeNames */
        attributes = elementJS.getAttributeNames();
    } else {
        attributes = undefined;
    }
    return attributes;
}

function setupAttributesFromJSElement(elementJS, attribute) {
    var attributes = [];
    if (TAG(attribute) !== TAG_REF) {
        if (TAG(attribute) !== TAG_ATM) {
            instantiation_error(attribute);
        }

        attributes.push(atable[VAL(attribute)]);
    } else {
        /** @namespace elementJS.getAttributeNames */
        attributes = elementJS.getAttributeNames();
    }
    return attributes;
}

function setupElements(element, attribute, value) {
    if (TAG(element) !== TAG_REF) {
        if (TAG(element) !== TAG_STR) {
            instantiation_error(element);
        }
        var elementObject = {};
        if (!get_element_object(element, elementObject)) {
            return undefined;
        }
        var elements = [];
        var elementJS = elementObject.value;
        if(elementJS) {
            elements.push(elementJS);
        }
        return elements;
    } else if (TAG(attribute) !== TAG_REF && TAG(value) !== TAG_REF) {
        if (TAG(attribute) !== TAG_ATM) {
            instantiation_error(attribute);
        }

        if (TAG(value) !== TAG_ATM) {
            instantiation_error(value);
        }

        var attributeJS = atable[VAL(attribute)];
        var valueJS = atable[VAL(value)];

        if (attributeJS === 'id') {
            elements = [];
            var idElementJS = document.getElementById(valueJS);
            if(idElementJS) {
                elements.push(idElementJS);
            }
        } else if (attributeJS === 'name') {
            elements = Array.from(document.getElementsByName(valueJS));
        } else if (attributeJS === 'class') {
            elements = Array.from(document.getElementsByClassName(valueJS));
        } else {
            elements = Array.from(document.querySelectorAll('*'));
        }
        return elements;
    } else {
        return Array.from(document.querySelectorAll('*'));
    }
}

function create_element_structure(elementJS) {
    return create_object_structure(elementJS);
}

function get_element_object(term, ref) {
    return get_object_container(term, ref);

     //(ref.type === 'element') ||  (ref.type === 'htmlelement') ;
}

function string_to_codes(string) {
    string = '' + string; // Ensure that string value is a character string and not a number or undefined.

    if(string.length === 0) {
        return NIL;
    }

    var tmp = state.H ^ (TAG_LST << WORD_BITS);
    for (var i = 0; i < string.length; i++)
    {
        memory[state.H] = string.charCodeAt(i) ^ (TAG_INT << WORD_BITS);
        // If there are no more items we will overwrite the last entry with [] when we exit the loop
        memory[state.H+1] = ((state.H+2) ^ (TAG_LST << WORD_BITS));
        state.H += 2;
    }
    memory[state.H-1] = NIL;
    return tmp;
}

function codes_to_string(codes, container, reportError) {
    var string = '';

    var list = codes;

    while(list !== NIL) {
        if(TAG(list) !== TAG_LST) {
            return reportError && instantiation_error(list);
        }

        var codePL = deref(memory[VAL(list)]);
        if(TAG(codePL) !== TAG_INT) {
            return reportError && instantiation_error(codePL);
        } else {
            string += String.fromCharCode(codePL);
            list = deref(memory[VAL(list) + 1]);
        }
    }
    container.value = string;
    return true;
}

function predicate_alert(term) {
    var termJS = format_term(term, {});
    alert(termJS);
    return true;
}

function predicate_dom_window(windowPL) {
    if(typeof window === 'undefined') {
        return false;
    }
    let objectPL = create_object_structure(window, 'window');
    return unify(objectPL, windowPL);
}

function predicate_create_dom_element(tag, element) {
    if(TAG(tag) !== TAG_ATM) {
        return instantiation_error(tag);
    }

    if(TAG(element) !== TAG_REF) {
        return instantiation_error(element);
    }

    var tagJS = atable[VAL(tag)];
    var elementJS = document.createElement(tagJS);
    var elementPL = create_element_structure(elementJS);

    return unify(element, elementPL);
}

function predicate_create_dom_text_node(text, element) {
    if(TAG(text) !== TAG_LST) {
        return instantiation_error(text);
    }

    if(TAG(element) !== TAG_REF) {
        return instantiation_error(element);
    }

    let container = {};
    if(! codes_to_string(text, container, true)) {
        return false;
    }
    var textJS = container.value;

    var elementJS = document.createTextNode(textJS);
    var elementPL = create_element_structure(elementJS);

    return unify(element, elementPL);

}

function predicate_append_dom_node_child(element, child) {
    if(TAG(child) !== TAG_STR) {
        return instantiation_error(child);
    }

    if(TAG(element) !== TAG_STR) {
        return instantiation_error(element);
    }

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        existence_error("element", element);
    }
    var elementJS = elementObject.value;

    var childObject = {};
    if (!get_element_object(child, childObject)) {
        existence_error("element", child);
    }
    var childJS = childObject.value;

    elementJS.appendChild(childJS);

    return true;

}


function predicate_insert_before_dom_node(parent, element, before) {

    if (TAG(parent) !== TAG_STR) {
        return instantiation_error(parent);
    }

    if (TAG(before) !== TAG_STR) {
        return instantiation_error(before);
    }

    if (TAG(element) !== TAG_STR) {
        return instantiation_error(element);
    }

    var parentObject = {};
    if (!get_element_object(parent, parentObject)) {
        return existence_error("element", parent);
    }
    var parentJS = parentObject.value;

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        return existence_error("element", element);
    }
    var elementJS = elementObject.value;

    var beforeObject = {};
    if (!get_element_object(before, beforeObject)) {
        return existence_error("element", before);
    }
    var beforeJS = beforeObject.value;

    parentJS.insertBefore(beforeJS, elementJS);

    return true;

}

function predicate_dom_select_element(query, element) {
    if (TAG(element) !== TAG_STR && TAG(element) !== TAG_REF) {
        return instantiation_error(element);
    }

    if (TAG(query) !== TAG_LST) {
        return instantiation_error(query);
    }

    let container = {};
    if(!codes_to_string(query, container, true)) {
        return false;
    }
    var queryJS = container.value;

    var elementJS = document.querySelector(queryJS);
    var elementPL = create_element_structure(elementJS);
    return unify(element, elementPL);
}

function predicate_dom_select_all_elements(query, element) {
    var cursor;
    var cursorIDPL;
    var cursorIDJS;

    if (state.foreign_retry) {
        cursorIDPL = state.foreign_value;
        cursorIDJS = atable[VAL(cursorIDPL)];
        cursor = desaCursors.get(cursorIDJS);

    }
    else {
        let container = {};
        if(!setupElementsForSelectAll(query, container)) {
            return false;
        }
        cursor = {
            elements: container.value
        };
        cursorIDJS = 'crs' + desaCursorCounter++;
        desaCursors.set(cursorIDJS, cursor);
        cursorIDPL = lookup_atom(cursorIDJS);

        create_choicepoint();
    }

    update_choicepoint_data(cursorIDPL);

    if (cursor.elements && cursor.elements.length > 0) {
        var elementJS = cursor.elements[0];
        cursor.elements = cursor.elements.slice(1);
        var elementPL = create_element_structure(elementJS);
       return unify(element, elementPL);
    } else {
        destroy_choicepoint();
        return false;
    }
}

function setupElementsForSelectAll(query, container) {

    let queryContainer = {};
    if(!codes_to_string(query, queryContainer, true)) {
        return false;
    }

    container.value = document.querySelectorAll(queryContainer.value);
    return true;
}

// proscriptls_init  generally is only used once in a web page to set up the proscriptls globals.
// Additional calls of Prolog queries should use proscriptls to avoid overwriting the global data,
// particularly the predicates from assertions.
/*
       // noinspection JSUnusedLocalSymbols
        function predicate_flush_stdout()
        {
            return true;
        }
        // noinspection JSUnusedLocalSymbols
        function stdout(msg) {
            alert(msg);
        }

 */

// These functions may be defined in some other file.
// If not then they are defined by proscriptls_init  method.

var predicate_flush_stdout;
var stdout;

// JavaScriptCore proscriptls.js standalone.js  -e "proscriptls_toplevel(true)"

function proscriptls_toplevel(debug) {
    proscriptls_init('toplevel.', debug, true, true);
}

function proscriptls_init(queryJS, debug, displayLoadInfo, displaySucceededMsg) {
    debugging = debug;

    if(! predicate_flush_stdout) {
        predicate_flush_stdout = function() { return true;};
    }

    if(! stdout) {
        stdout = function(msg) {console.log(msg);};
    }

    load_state();

    if(displayLoadInfo) {
        let indexedCount = 0;
        let fullyIndexedCount = 0;
        for(let key of Object.keys(predicates)) {
            let predicate = predicates[key];
            if(typeof predicate.index !== 'undefined') {
                indexedCount++;

                let sequenceCount = 0;
                for(let clauseKey of predicate.clause_keys) {
                    if(clauseKey !== predicate.index) {
                        let clause = predicate.clauses[predicate.clause_keys[clauseKey]];
                        let code = clause.code;
                        if (code[0] === 30 || code[0] === 254) {
                            // trust_me or nop2 instruction
                            // trust_me indicates a sequence of 2 or more clauses,
                            // nop2 indicates a sequence of one clause.
                            sequenceCount++;
                        }
                    }
                }

                if(sequenceCount === 1) {
                    fullyIndexedCount++;
                }
            }
        }
        stdout("Loaded " + Object.keys(predicates).length + " predicates (" + indexedCount + " indexed, " + fullyIndexedCount + " single sequence)\n");
        stdout("Loaded " + atable.length + " atoms\n");
        stdout("Loaded " + ftable.length + " functors\n");
    }

    initialize();

    call_directives('clear_directives');

    danglingPredicates();

    unusedPredicates();

    consult_scripts();

    if(queryJS && queryJS !== '') {
        initialize(); // ensure state is initialized. proscriptls saves and restores state.
        proscriptls(queryJS, displaySucceededMsg);
    }
}

function consult_scripts() {
    // skip this function if it is invoked in JavaScriptCore or nodejs, where
    // 'document' is not a defined global var.
    if(typeof document === 'undefined' || document === undefined) {
        return;
    }

    let scripts = document.getElementsByTagName('SCRIPT');
    // collect script.src URLs to pass to consult/1.
    // for scripts that have inline text use compile_atom/1.

    let srcs = [];
    for(let script of scripts) {
        if(script.type && script.type === 'text/prolog') {
            if (script.src) {
                srcs.push(script.src);
            } else {
                consult_script_text(script.text);
            }
        }
    }

    consult_script_srcs(srcs);
}

function consult_script_text(code_atom) {
//    initialize();
    let atom = lookup_atom(code_atom);
    let ftor = VAL(lookup_functor("wam_compiler:consult_atom", 1));
    allocate_first_frame();
    var pred = predicates[ftor];
    var pi = predicates[ftor].clause_keys[0];
    state.current_predicate = pred;
    code = pred.clauses[pi].code;
    state.P = 0;
    register[0] = atom;
    if (wam())
        debug("Script consulted");
    else
        debug("Failed to consult script");
}

function consult_script_srcs(srcs) {
    // set up consult(srcs)
    // srclist
    // pred = consultPred
    // register[0] = srclist
 //   initialize();

    if(srcs.length === 0) {
        return;
    }

    var srcList = state.H ^ (TAG_LST << WORD_BITS);
    for (var i = 0; i < srcs.length; i++)
    {
        memory[state.H] = lookup_atom(srcs[i]);
        // If there are no more items we will overwrite the last entry with [] when we exit the loop
        memory[state.H+1] = ((state.H+2) ^ (TAG_LST << WORD_BITS));
        state.H += 2;
    }
    memory[state.H-1] = NIL;

    let ftor = VAL(lookup_functor("wam_compiler:consult", 1));
    allocate_first_frame();
    var pred = predicates[ftor];
    var pi = predicates[ftor].clause_keys[0];
    state.current_predicate = pred;
    code = pred.clauses[pi].code;
    state.P = 0;
    register[0] = srcList;
    if (wam())
        debug("Script srcs consulted");
    else
        debug("Failed to consult script srcs");
}

function call_directives(mode) {

    let system_predicates = (! system || system.length === 0)
        ? undefined
        : system.map((V) => {return "'" + atable[ftable[V][0]] + "'"}).join(", ");

    let initialization_predicates = (! initialization || initialization.length === 0)
        ? undefined
        : initialization.map((V) => {return "'" + atable[ftable[V][0]] + "'"}).join(", ");

    let extended_query = "";

    if(system_predicates){
        if(initialization_predicates){
            extended_query = system_predicates + ", " + initialization_predicates;
        } else {
            extended_query = system_predicates;
        }
    } else if(initialization_predicates){
        extended_query = initialization_predicates;
    }

    if(extended_query !== "") {
        proscriptls(extended_query);
    }

    if(mode === 'clear_directives' ) {
        for(let predicateID of system) {
            delete predicates[predicateID];
        }
        system = [];
    }
}

// proscriptls calls the given query using the current predicates definitions.
// All other global runtime data is saved and restored.
// This allows the asserta/assertz clauses to persist across calls of proscriptls.

function proscriptls(queryJS, displaySucceededMsg) {
    if(state.wamYielded) {
        // delay until wamYielded is false.
        gcWrite('waiting on wamYielded: proscriptls(' + queryJS + ',' + displaySucceededMsg + ')');
        setTimeout(proscriptls, 10, queryJS, displaySucceededMsg);
        return;
    }

    let saved_state;
    let saved_registers;
    let saved_code;
    let saved_memory;
    let restore_environment = false;
    if(state.running) {
        saved_state = copy_state(state);
        saved_registers = copy_registers(register);
        saved_code = code;
        saved_memory = copy_memory(memory);
        restore_environment = true;
    }
    initialize();
    allocate_first_frame();
    // call_atom(query, Bindings)
    // ignore the Bindings for now (may be useful later)
    var ftor = VAL(lookup_functor("wam_compiler:call_atom", 2));
    var pred = predicates[ftor];
    var pi = predicates[ftor].clause_keys[0];
    state.current_predicate = pred;
    code = pred.clauses[pi].code;
    register[0] = lookup_atom(queryJS);
    register[1] = alloc_var();
    try
    {
        if (!wam())
        {
            if (exception == null)
                stdout("Failed " + queryJS + "\n");
            else
                stdout("Uncaught exception: " + term_to_string(recall_term(exception, {})) +"\n");
        } else if(displaySucceededMsg) {
            stdout("Succeeded " + queryJS + "\n");
        }
    } catch (anything)
    {
        if(console && console.log) {
            console.log(anything);
        }
        debug("Error. " + anything);
    }

    if(restore_environment) {
        register = copy_registers(saved_registers);
        state = copy_state(saved_state);
        code = saved_code;
        memory = copy_memory(saved_memory);
    }
}

function proscriptls_apply(goalArguments, module, goal) {
    // goal = '[Tx-X,Ty-Y,...] ^ G' where G is an expression referencing X, Y, ...
    // goalArguments is an array [a,b, ...] where each item is applied to the corresonding
    // entry in [X, Y, ...].The combined expression is:
    //     ParamGoal = Arguments ^ BoundGoal, call(BoundGoal)
    // proscriptls();
    // However, the goalArguments must be converted to their Prolog syntactic representation.
    // How this is done depends on the expected type, which is encoded in the goal's argument T specification.

    // goal = "[a-B, c-D]^ foo(B, D)" -> ["[a-B, c-D]", "a-B, c-D"]
    // for each argument[i], create formatted term F[i] of prolog transform(type[i], argument[i].
    // new goal = "F[i] = A[i], ..., G"
    let typedArgumentStrings = goal.match(/^ *\[(.*)] *\^/);

    let goalReconstituted;
    if(typedArgumentStrings) {
        let typedArgumentPrefix = typedArgumentStrings[0];
        // 'goal' may be a complex expression with a primary operator precedence greater than that of ':'.
        // Because of this, the goalString is 'module : (P)' instead of just 'module : P'.
        let goalString = module + ' : (' + goal.substring(typedArgumentPrefix.length) + ')';

        let typedArgumentString = typedArgumentStrings[1];
        let typedArguments = typedArgumentString.trim().split(",");

        let unificationExpressions = [];
        let limit = Math.min(typedArguments.length, goalArguments.length);

        for (let ofst = 0; ofst < limit; ofst++) {
            let typedArgumentString = typedArguments[ofst];
            let items = typedArgumentString.trim().split('-');
            let type = items[0].trim();
            let variable = items[1].trim();
            let argument = goalArguments[ofst];
            let resultContainer = {};
            if (convert_result(argument, {type: type}, module, resultContainer)) {
                let argumentPL = resultContainer.value;
                let argumentReconstituted = format_term(argumentPL, {quoted: true});
                unificationExpressions.push(variable + " = " + argumentReconstituted);
            }
        }

        let argumentUnificationsPrefix = unificationExpressions.join(", ");
        goalReconstituted = argumentUnificationsPrefix + ", " + goalString;
    } else {
        goalReconstituted = module + ": (" + goal + ")";
    }

    proscriptls(goalReconstituted);
}

function debug(msg) {
    if(debugging) {
        alert(msg);
    }
}

let logKey = 'proscriptls_log';

function logToLocalStorage(msg) {
    let log = window.localStorage.getItem(logKey);
    if(log) {
        log += msg + '\n';
    } else {
        log = msg + '\n';
    }
    window.localStorage.setItem(logKey, log);
}

function predicate_clear_local_storage_log() {
    window.localStorage.removeItem(logKey);

    current = -1;
    let ringCurrentKey = logKey + ':current';
    window.localStorage.setItem(ringCurrentKey, '' + current);

    let ofst = 0;
    while(true) {
        let ringKey = logKey + ':' + ofst++;
        if(typeof window.localStorage.getItem(ringKey) === 'undefined' ||
            window.localStorage.getItem(ringKey) === null) {
            break;
        } else {
             window.localStorage.removeItem(ringKey);
        }
    }

    return true;
}

let current = -1;

function log_ring(msg) {
    current = (current+1) % prolog_flag_values.wam_log_size;
    let ringKey = logKey + ':' + current;
    let ringCurrentKey = logKey + ':current';
    window.localStorage.setItem(ringKey, msg);
    window.localStorage.setItem(ringCurrentKey, '' + current);
}

function dumpWriteLogRing() {
    let ringCurrentKey = logKey + ':current';
    let currentLogString = window.localStorage.getItem(ringCurrentKey);
    let currentLog = Number.parseInt(currentLogString);
    // find log ring size for the currently recorded log entries.
    // This might be different than the current global value for log_ring_size.
    let ofst = 0;
    while(true) {
        let ringKey = logKey + ':' + ofst++;
        if(typeof window.localStorage.getItem(ringKey) === 'undefined' ||
            window.localStorage.getItem(ringKey) === null) {
            break;
        }
    }

    let ringSize = ofst-1;
    let unadjustedLimit = currentLog + ringSize;
    for(let unadjustedOfst = currentLog;unadjustedOfst < unadjustedLimit;unadjustedOfst++) {
        let adjustedOfst = unadjustedOfst % ringSize;
        let ringKey = logKey + ':' + adjustedOfst;
        let msg = window.localStorage.getItem(ringKey);
        dumpWrite(msg);
    }
}
// File debugger.js
"use strict";

var input_buffer = [];

function predicate_get_terminal_char(c) {
    let char = get_terminal_char();
    if(char) {
        return unify(c, lookup_atom(char));
    } else {
        return false;
    }
}

function get_terminal_char() {
    return input_buffer.shift();
}

var trace_retry = 'false';

function predicate_trace_set_retry(value) {
    if(TAG(value) === TAG_INT) {
        trace_retry = VAL(value);
    } else if(TAG(value) === TAG_ATM) {
        trace_retry = atable[VAL(value)];
    }
    // stdout('Set retry: ' + trace_retry + '\n');
    return true;
}

function predicate_trace_retry_value(value) {
    //stdout('Retry: ' + trace_retry + '\n');
    if(typeof trace_retry === 'number') {
        return unify(value, PL_put_integer(trace_retry));
    } else {
        return unify(value, lookup_atom(trace_retry));
    }
}

function predicate_trace_set_prompt(value) {
    state.trace_prompt = atable[VAL(value)];
    return true;
}
// File decode_instruction.js
"use strict";

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
            instructionSize = 2;
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
                let decoding = decode_switch_table_sequence('constant',codePosition + 2, code);
                size = decoding.size;
                table = 'seq(' + decoding.string + ')';
            } else if(T === 1) {
                let decoding = decode_switch_table_hash('constant',codePosition + 2, code);
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
                let decoding = decode_switch_table_sequence('structure',codePosition + 2, code);
                size = decoding.size;
                table = 'seq(' + decoding.string + ')';
            } else if(T === 1) {
                let decoding = decode_switch_table_hash('structure',codePosition + 2, code);
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

function decode_switch_table_sequence(dataType, codePosition, code) {
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

function decode_switch_table_hash(dataType, codePosition, code) {
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
            let decoding = decode_switch_table_sequence(dataType, BAX, code);
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
// File object.js
'use strict';

var idsToObjects = new Map();
var idsToTypes = new Map();
var objectsToIDs = new Map();
var goalFunctions = new Map();

var dotrCursors = new Map();
var dotrCursorCounter = 0;

var dtpaCursors = new Map();
var dtpaCursorCounter = 0;

var dtpCursors = new Map();
var dtpCursorCounter = 0;

var dtmCursors = new Map();
var dtmCursorCounter = 0;

// create_object_structure interns a Javascript object by creating
// a unique key string for that object and storing the relationship
// between that key and the object in two global static Map objects.
// It constructs a one-argument Prolog structure with a distinctive functor
// and the key as the argument to identify that Javascript object.
// Finally, it returns the internal Prolog reference (index in
// the global memory array of the root of the structure term)
// to the newly allocated structure.
//
// get_object_id_container takes a reference to a Prolog structure
// term as created by create_object_structure and an 'idContainer'
// Javascript object as input,
// sets idContainer.value to the key used internally that identifies a Javascript object,
// and returns 'true'.
//
// get_object_container takes an object-identifying Prolog structure
// term (as above) and a 'container' Javascript object as input,
// sets container.value to the identified Javascript object, and
// returns 'true'.

// functor for an object that is an instance of Foo is typically '$foo'.
function create_object_structure(objectJS, specifiedTypeJS) {
    var objectMapID = lookup_object(objectJS, specifiedTypeJS);
    var lookupObject = lookup_atom(objectMapID);
    // '$obj'(lookupObject)
    var ftor = lookup_functor('$obj', 1);
    var objectPL = alloc_structure(ftor);
    memory[state.H++] = lookupObject;
    return objectPL;
}

function lookup_object(objectJS, specifiedTypeJS) {
    var objectMapID = objectsToIDs.get(objectJS);
    if (objectMapID) {
        return objectMapID;
    }

    objectMapID = 'obj' + objectsToIDs.size;
    objectsToIDs.set(objectJS, objectMapID);
    idsToObjects.set(objectMapID, objectJS);
    let objectType = specifiedTypeJS ? specifiedTypeJS : object_type_check(objectJS);
    idsToTypes.set(objectMapID, objectType);
    return objectMapID;
}

// For objects of type Foo the 'type' = 'foo' and functor = '$foo'.
function get_object_container(term, container) {
    let objectIDContainer = {};
    if (!get_object_id_container(term, objectIDContainer)) {
        return false;
    }
    container.value = idsToObjects.get(objectIDContainer.value);
    container.type = idsToTypes.get(objectIDContainer.value);
    if(! container.value) {
        return domain_error("no such object", term);
    }
    return true;
}

function release_object(term) {
    let objectIDContainer = {};
    if (!get_object_id_container(term, objectIDContainer)) {
        return false;
    }
    idsToObjects.delete(objectIDContainer.value);
    idsToTypes.delete(objectIDContainer.value);
    return true;
}

// For objects of type Foo the 'type' = 'foo' and functor = '$foo'.
function get_object_id_container(term, idContainer) {
    if (TAG(term) !== TAG_STR)
        return type_error('obj', term);
    var ftor = VAL(memory[VAL(term)]);
    if (atable[ftable[ftor][0]] === '$obj' && ftable_arity(ftor) === 1) {
        var arg = deref(memory[VAL(term) + 1]);
        if (TAG(arg) !== TAG_ATM)
            return type_error("obj_arg");
        idContainer.value = atable[VAL(arg)];
        return true;
    }
    return type_error(type, term);
}

var parentMap = new Map([
    ['eventtarget', []],
    ['window', ['eventtarget', 'windowlocalstorage', 'windowsessionstorage', 'windoworworkerglobalscope']],
    ['node', ['eventtarget']],
    ['parentnode', []], // ParentNode is a mixin, there is no constructor for it.
    ['document', ['node', 'parentnode']],
    ['htmldocument', ['document']],
    ['documentfragment', ['node', 'parentnode']],
    ['element', ['node', 'parentnode']],
    ['htmlelement', ['element']],
    ['htmlcanvaselement', ['htmlelement']],
    ['event', []],
    ['cssstyledeclaration', []],
    ['cssrule', []],
    ['canvasrenderingcontext2d', []],
    ['blob', []],
    ['imagedata', []],
    ['uint8clampedarray', []],
    ['canvasgradient', []],
    ['canvaspattern', []],
    ['htmlimageelement', ['htmlelement']],
    ['htmlinputelement', ['htmlelement']],
    ['htmltextareaelement', ['htmlelement']],
    ['htmlselectelement', ['htmlelement']],
    ['htmlformelement', ['htmlelement']],
    ['htmloptionelement', ['htmlelement']],
    ['path2d', []],
    ['uievent', ['event']],
    ['mouseevent', ['uievent']],
    ['textmetrics', []],
    ['validitystate', []],
    ['file', ['blob']],
    ['elementcreationoptions', []],
    ['location', []],
    ['history', []],
    ['customelementregistrgy',[]],
    ['barprop', []],
    ['navigator', ['navigatorid','navigatorlanguage', 'navigatoronline', 'navigatorcontentutils', 'navigatorcookies']],
    ['navigatorid', []], // mixin
    ['navigatorlanguage', []], // mixin
    ['navigatoronline', []], // mixin
    ['navigatorcontentutils', []], // mixin
    ['navigatorcookies', []], // mixin
    ['blob', []],
    ['storage', []],
    ['windowlocalstorage',[]], // mixin
    ['windowsessionstorage',[]] // mixin
]);

var childMap = new Map();

function calculate_inheritance_children() {
    for(let objectType of parentMap.keys()) {
        let parents = parentMap.get(objectType);

        for(let ofst = 0;ofst < parents.length;ofst++) {
            let parent = parents[ofst];
            let children = childMap.get(parent);
            if(children) {
                children.push(objectType);
            } else {
                childMap.set(parent, [objectType]);
            }
        }
    }
}

calculate_inheritance_children();

// constructorMap[obj.constructor] is object type.
// This map does not include mixin Web API Interfaces.

var constructorMap = {
    "ImageData" : "imagedata",
    "Uint8ClampedArray" : 'uint8clampedarray',
    "CanvasGradient" : 'canvasgradient',
    "CanvasPattern" : 'canvaspattern',
    "HTMLImageElement" : 'htmlimageelement',
    "HTMLTextAreaElement" : 'htmltextareaelement',
    "HTMLInputElement" : 'htmlinputelement',
    "HTMLSelectElement" : 'htmlselectelement',
    "HTMLFormElement" : 'htmlformelement',
    "HTMLOptionElement" : 'htmloptionelement',
    "Path2D" : 'path2d',
    "UIEvent" : 'uievent',
    "MouseEvent" : 'mouseevent',
    "TextMetrics" : 'textmetrics',
    "ValidityState" : 'validitystate',
    "File": 'file',
    "ElementCreationOptions": 'elementcreationoptions',
    "DocumentFragment": 'documentfragment',
    "Location": 'location',
    "History" : 'history',
    "CustomElementRegistry" : 'customelementregistrgy',
    "BarProp" : 'barprop',
    "Navigator" : 'navigator',
    "Blob" : 'blob',
    "Storage" : 'storage'
};

var distinctivePropertyMap = {
    node:'nodeType',
    element:'id',
    htmlelement:'title',
    event:'eventPhase',
    cssrule: 'parentStyleSheet'
};

var distinctiveMethodMap = {
    eventtarget: 'addEventListener',
    document: 'getElementById',
    cssstyledeclaration:'getPropertyPriority',
    htmlcanvaselement:'getContext',
    canvasrenderingcontext2d: 'getImageData'
};

// [createImageData, [number, number], object]
// [MethodName, ArgTypes, ReturnType] or [MethodName, ArgTypes] if no return.
//
// ['createImageData',{name:'createImageData',arguments:[{type:'number'},{type:'number'}],returns:{type:'object'}}]

function convert_method_spec(specTerm, resultContainer) {
    if (TAG(specTerm) !== TAG_LST) {
        return type_error("list", specTerm);
    }

    let methodNamePL = deref(memory[VAL(specTerm)]);

    let methodNameContainer = {};
    let methodNameJS;
    if (getAtomPropertyValue(methodNamePL, methodNameContainer, true)) {
        methodNameJS = methodNameContainer.value;
    } else {
        return false;
    }

    let result = {};
    result.name = methodNameJS;

    let specTermTailPL = deref(memory[VAL(specTerm) + 1]);
    let argTypesContainer = {};
    let typeTermsListPL = deref(memory[VAL(specTermTailPL)]); // head of specTermTailPL = [type1, type2, ...]
    if(! convert_type_terms(typeTermsListPL, argTypesContainer)) {
        return false;
    }
    result.arguments = argTypesContainer.value;

    let specTermTailTailPL = deref(memory[VAL(specTermTailPL) + 1]); // tail of specTermTailPL
    if(specTermTailTailPL !== NIL) {
        let returnTypePL = deref(memory[VAL(specTermTailTailPL)]);
        let returnContainer = {};
        if (convert_type_term(returnTypePL, returnContainer)) {
            result.returns = returnContainer.value;
        } else {
            return false;
        }
    }
    resultContainer.value = result;
    return true;
}

function convert_type_terms(list, container) {
    if(list === NIL) {
        container.value = [];
        return true;
    }

    if(TAG(list) !== TAG_LST) {
        return type_error('list', list);
    }

    let argList = list;
    let argTypesJS = [];
    while (argList !== NIL) {
        if (TAG(argList) !== TAG_LST) {
            return type_error("list", argList);
        }
        let argTypePL = deref(memory[VAL(argList)]);
        let argTypeContainer = {};
        if(!convert_type_term(argTypePL, argTypeContainer)) {
            return false;
        }
        argTypesJS.push(argTypeContainer.value);
        argList = deref(memory[VAL(argList) + 1]);
    }
    container.value = argTypesJS;
    return true;
}

// atom or array(atom)
// Type = {type: atom} or {type: {array: atom}}
function convert_type_term(typePL, container) {
    let result = {};
    if(TAG(typePL) === TAG_ATM) {
        let typeContainer = {};
        if(! getAtomPropertyValue(typePL, typeContainer, true)) {
            return false;
        }
        result.type = typeContainer.value;
    } else if(TAG(typePL) === TAG_STR) {
        let functorPL = ftable[VAL(memory[VAL(typePL)])][0];
        let functor = atable[functorPL];
        if(functor !== 'array_type') {
            return domain_error('type array_type', functorPL);
        }
        let arity = ftable[VAL(memory[VAL(typePL)])][1];
        if(arity !== 1) {
            return representation_error('type array_type arity 1', typePL);
        }
        let subContainer = {};
        if(! convert_type_term(deref(memory[VAL(typePL) + 1]), subContainer, true)) {
            return false;
        }
        let extendedType = {};
        extendedType.arrayType = subContainer.value.type;
        result.type = extendedType;
    } else {
        return type_error('atom or structure', typePL);
    }

    container.value = result;
    return true;
}


// [integer, length]
// [Property, DataType, settable] or [Property, DataType] if not settable.
//
// SimpleProperty('integer', 'length')

function convert_property_spec(specTerm, resultContainer) {
    if(TAG(specTerm) === TAG_ATM) {
        return getAtomPropertyValue(specTerm, resultContainer, true);
    }

    if (TAG(specTerm) !== TAG_LST) {
        return type_error("list", specTerm);
    }

    let propertyPL = deref(memory[VAL(specTerm)]);
    if(TAG(propertyPL) !== TAG_ATM) {
        return type_error("atom", propertyPL);
    }

    let propertyContainer = {};
    let propertyJS;
    if (getAtomPropertyValue(propertyPL, propertyContainer, true)) {
        propertyJS = propertyContainer.value;
    } else {
        return false;
    }

    let specTermTailPL = deref(memory[VAL(specTerm) + 1]);
    let typePL = deref(memory[VAL(specTermTailPL)]);
    if(TAG(typePL) !== TAG_ATM) {
        return type_error("atom", typePL);
    }
    let typeContainer = {};
    let typeJS;
    if (getAtomPropertyValue(typePL, typeContainer, true)) {
        typeJS = typeContainer.value;
    } else {
        return false;
    }

    let specTermTailTailPL = deref(memory[VAL(specTermTailPL) + 1]);
    let settableFlag = false;
    if(specTermTailTailPL !== NIL) {
        let settablePL = deref(memory[VAL(specTermTailTailPL)]);
        if (TAG(settablePL) !== TAG_ATM) {
            return type_error("atom", typePL);
        }
        let settableContainer = {};
        let settableJS;
        if (getAtomPropertyValue(settablePL, settableContainer, true)) {
            settableJS = settableContainer.value;
            if (settableJS !== 'settable' && settableJS !== 'not_settable') {
                return domain_error('settable or not_settable', settablePL);
            }
            settableFlag = (settableJS === 'settable');
        } else {
            return false;
        }
    }

    resultContainer.value = SimpleProperty(typeJS, propertyJS, settableFlag);
    return true;
}

function getInterfaceItemSpec(typeJS, itemType, itemName) {
    let itemsMember;
    if(itemType === 'method') {
        itemsMember = 'methods';
    } else if(itemType === 'property') {
        itemsMember = 'properties';
    } else {
        throw 'internal error: invalid interface item type = ' + itemType + '. Must be either "method" or "property".';
    }

    let stack = [typeJS];
    while (stack.length > 0) {
        let testType = stack.shift(0);
        let specs = webInterfaces.get(testType);
        if (specs ) {
            if(specs[itemsMember]) {
                let spec = specs[itemsMember].get(itemName);
                if (spec) {
                    return spec;
                }
            }
            // methodName not found. put parentMap.get(testType) on the
            // bottom of the stack (for breadth-first search of
            // parents)

            let parents = parentMap.get(testType);
            if (parents) {
                for (let parent of parents) {
                    stack.push(parent);
                }
            }
        }
    }
    return undefined;
}

function object_type_check(object, candidates) {
    let constructorType = object.constructor && object.constructor.name && constructorMap[object.constructor.name];
    if(constructorType) {
        return constructorType;
    }

    if(! candidates) {
        candidates = [];
        for(let candidate of parentMap.keys()) {
            let parents = parentMap.get(candidate);
            if(parents.length === 0) {
                candidates.push(candidate);
            }
        }
    }

    for (let ofst = 0;ofst < candidates.length;ofst++) {
        let candidate = candidates[ofst];
        let checkProperty = distinctivePropertyMap[candidate];
        let checkMethod = distinctiveMethodMap[candidate];
        if ((checkProperty && typeof object[checkProperty] !== 'undefined')
            || (checkMethod && typeof object[checkMethod] === 'function')) {
            if (! childMap.get(candidate)) {
                return candidate;
            } else {
                let childType = object_type_check(object, childMap.get(candidate));
                if(childType) {
                    return childType;
                } else {
                    return candidate;
                }
            }
        }
    }
    return undefined;
}

function predicate_dom_object_type(object, type) {
    if(TAG(object) === TAG_REF) {
        return instantiation_error(object);
    }

    let objectContainer = {};
    if (!get_object_container(object, objectContainer)) {
        return false;
    }
    let typeJS = objectContainer.type;
    return unify(type, lookup_atom(typeJS));
}

/**
 * The type parameter is either a Type or ModuleName : Type. If ModuleName is not specified it is inferred to be 'user'.
 * The Type term is the constructor name and arguments (if any).
 * An argument may be a goal_functor type, in which case the ModuleName (inferred or explicit)
 * is needed to determine what module holds the predicate(s) of the goal_functor.
 * @param type
 * @param object
 * @param spec
 * @returns {boolean}
 */
function predicate_dom_create_object(type, object, spec) {
    if(TAG(type) !== TAG_ATM && TAG(type) !== TAG_STR) {
        return type_error('atom or structure', type);
    }

    let typeJS;
    let moduleName;
    let arity;
    let methodAddress;

    if (TAG(type) === TAG_ATM) {
        typeJS = atable[VAL(type)];
        arity = 0;
        moduleName = 'user';
    } else  {
        let functor = atable[ftable[VAL(memory[VAL(type)])][0]];
        let localArity = ftable[VAL(memory[VAL(type)])][1];
        if (functor === ':' && localArity === 2) {
            // ModuleName : MethodStructure
            moduleName = atable[VAL(deref(memory[VAL(type) + 1]))];
            let subStructure = deref(memory[VAL(type) + 2]);
            if(TAG(subStructure) === TAG_ATM) {
                typeJS = atable[VAL(subStructure)];
                arity = 0;
            } else {
                methodAddress = VAL(subStructure);
                typeJS = atable[ftable[VAL(memory[methodAddress])][0]];
                arity = ftable[VAL(memory[methodAddress])][1];
            }
        } else {
            moduleName = 'user';
            methodAddress = VAL(type);
            typeJS = atable[ftable[VAL(memory[methodAddress])][0]];
            arity = ftable[VAL(memory[methodAddress])][1];
        }
    }

    let structureArguments = [];

    for(let ofst = 0; ofst < arity;ofst++) {
        let argument = deref(memory[methodAddress + ofst + 1]);
        structureArguments.push(argument);
    }

    if(TAG(object) !== TAG_REF) {
        return instantiation_error(object);
    }

    let constructorName;
    for(let entry of Object.entries(constructorMap)) {
        if(entry[1] === typeJS) {
            constructorName = entry[0];
            break;
        }
    }

    if(! constructorName) {
        constructorName = typeJS;
    }

    let objectJS;
    if(structureArguments.length > 0) {
        let argTypesContainer = {};
        if(! convert_type_terms(spec, argTypesContainer)) {
            return false;
        }
        let specArguments = argTypesContainer.value;

        let applyArguments = [];
        for (var i = 0; i < arity; i++) {
            let applyArgumentContainer = {};
            if (convert_method_argument(structureArguments[i], specArguments[i], moduleName, applyArgumentContainer)) {
                applyArguments.push(applyArgumentContainer.value);
            } else {
                return false;
            }
        }

        objectJS = newCall(window[constructorName], applyArguments);
     } else {
        objectJS = new window[constructorName]();
    }

    let derivedConstructorName = objectJS.constructor.name;
    let derivedTypeJS = constructorMap[derivedConstructorName];

    let recordedTypeJS = (derivedTypeJS) ? derivedTypeJS : typeJS;

    let objectPL = create_object_structure(objectJS, recordedTypeJS);
    return unify(object, objectPL);
}

/*
The newCall function is from
    https://stackoverflow.com/a/8843181/302650 ,
an answer to
    https://stackoverflow.com/questions/1606797/use-of-apply-with-new-operator-is-this-possible


 */
function newCall(Cls, structureArguments) {
    // The first of the bindArguments is the first argument to the bind() function. From bind() doc:
    // [The first argument is the] value to be passed as the 'this' parameter to the target function when the bound function is called.
    // This first argument is null because the 'new' operator overrides the 'this' parameter.

    let bindArguments = [null].concat(structureArguments);
    return new (Function.prototype.bind.apply(Cls, bindArguments));
    // or even
    // return new (Cls.bind.apply(Cls, arguments));
    // if you know that Cls.bind has not been overwritten
}

function predicate_dom_release_object(object) {
    if (TAG(object) === TAG_REF) {
        return instantiation_error(object);
    }

    return release_object(object);
}

function predicate_dom_type_reference(type, name, standard, mdn) {
    var cursor;
    var cursorIDPL;
    var cursorIDJS;

    if (state.foreign_retry) {
        cursorIDPL = state.foreign_value;
        cursorIDJS = atable[VAL(cursorIDPL)];
        cursor = dotrCursors.get(cursorIDJS);

    }
    else {
        let container = {};
        if(!setupInterfaceSpecificationsForType(type, container)) {
            return false;
        }
        cursor = {
            types: container.value
        };
        cursorIDJS = 'crs' + dotrCursorCounter++;
        dotrCursors.set(cursorIDJS, cursor);
        cursorIDPL = lookup_atom(cursorIDJS);

        create_choicepoint();
    }

    update_choicepoint_data(cursorIDPL);

    if (cursor.types && cursor.types.length > 0) {
        let typeJS = cursor.types[0];
        cursor.types = cursor.types.slice(1); // set cursor.types to next type for retry.
        let spec = webInterfaces.get(typeJS);
        if(spec) {
            let reference = spec.reference;
            if(reference) {
                let nameTest = reference.name;
                let standardTest = reference.standard;
                let mdnTest = reference.mdn;
                if(nameTest && standardTest && mdnTest) {
                    return unify(type, PL_put_atom_chars(typeJS)) &&
                        unify(name, PL_put_atom_chars(nameTest)) &&
                        unify(standard, PL_put_atom_chars(standardTest)) &&
                        unify(mdn, PL_put_atom_chars(mdnTest));
                } else {
                    return engine_error('Web API Interface type ' + typeJS + ' has an incomplete specification reference section : ' + JSON.stringify(spec));
                }
            } else {
                return engine_error('Web API Interface type ' + typeJS + ' has specification without a "reference" section : ' + JSON.stringify(spec));
            }
        } else {
            return domain_error('web api interface type', typeJS);
        }
     } else {
        destroy_choicepoint();
        return false;
    }
}

function setupInterfaceSpecificationsForType(type, container) {
    if (TAG(type) !== TAG_REF) {
        let typeJS = PL_get_atom_chars(type);
        container.value = [typeJS];
    } else {
        container.value = Array.from(webInterfaces.keys());
    }

    return true;
}

function predicate_dom_type_parent(objectType, parentType) {
    // The function uses fail/retry to implement a doubly-nested loop: the outer loop
    // iterates over the Web Interface API object types, the inner loop iterates over
    // the parent types defined for the current object type of the outer loop.

    var cursor;
    var cursorIDPL;
    var cursorIDJS;

    if (state.foreign_retry) {
        cursorIDPL = state.foreign_value;
        cursorIDJS = atable[VAL(cursorIDPL)];
        cursor = dtpaCursors.get(cursorIDJS);

    }
    else if(TAG(objectType) !== TAG_REF && TAG(parentType) !== TAG_REF) {
        return deterministic_dom_type_parent(objectType, parentType);
    }
    else {
        cursor = {};
        let container = {};
        if(!setupInterfaceSpecificationsForType(objectType, container)) {
            return false;
        }
        cursor.objectTypes = container.value;
        cursorIDJS = 'crs' + dtpaCursorCounter++;
        dtpaCursors.set(cursorIDJS, cursor);
        cursorIDPL = lookup_atom(cursorIDJS);

        create_choicepoint();
    }

    update_choicepoint_data(cursorIDPL);

    if (cursor.objectTypes && cursor.objectTypes.length > 0) {
        let objectTypeJS = cursor.objectTypes[0];
        if (!cursor.parents) {
            if (!setupCursorForParentTypes(parentMap.get(objectTypeJS), parentType, cursor)) {
                destroy_choicepoint();
                return false;
            }
        }

        if (cursor.parents && cursor.parents.length > 0) {
            let parentTypeJS = cursor.parents[0];
            cursor.parents = cursor.parents.slice(1);

            return unify(objectType, PL_put_atom_chars(objectTypeJS)) &&
                unify(parentType, PL_put_atom_chars(parentTypeJS));
        } else {
            // All parents for current type have been processed.
            // Set the cursor.parents to undefined to force recalculation of parents
            // with next type.

            cursor.parents = undefined;

            // set cursor.objectTypes to next objectType for retry.
            cursor.objectTypes = cursor.objectTypes.slice(1);
            return false;
        }
    } else {
        destroy_choicepoint();
        return false;
    }

}

function deterministic_dom_type_parent(objectType, parentType) {
    if(TAG(objectType) === TAG_REF) {
        return instantiation_error(objectType);
    } if (TAG(objectType) !== TAG_ATM) {
        return type_error('atom', objectType);
    }

    let objectTypeJS = PL_get_atom_chars(objectType);

    if(TAG(parentType) === TAG_REF) {
        return instantiation_error(parentType);
    } if (TAG(parentType) !== TAG_ATM) {
        return type_error('atom', parentType);
    }

    let parentTypeJS = PL_get_atom_chars(parentType);
    let parents = parentMap.get(objectTypeJS);

    return parents && parents.includes(parentTypeJS);
}

function setupCursorForParentTypes(parents, parentType, cursor) {
    if (! parents) {
        cursor.parents = [];
    } else if (TAG(parentType) !== TAG_REF) {
        if (TAG(parentType) !== TAG_ATM) {
            return type_error('atom', parentType);
        }
        let parentTypeJS = PL_get_atom_chars(parentType);
        if (parents.includes(parentTypeJS)) {
            cursor.parents = [parentTypeJS];
        } else {
            cursor.parents = [];
        }
    } else {
        cursor.parents = parents;
    }

    return true;

}
// Find the related values for a WebInterface API object type, a property name implemented for that type, the Javascript Web API function name used
// to implement that property, and the type of value for that property.
//
// All binding patterns are supported: objectType REF or ATM, propertyName REF or ATM, jsName REF or ATM, and valueType REF or ATM.

function predicate_dom_type_property(objectType, propertyName, jsName, valueType) {
    // The function uses fail/retry to implement a doubly-nested loop: the outer loop
    // iterates over the Web Interface API object types, the inner loop iterates over
    // the properties defined for the current object type of the outer loop.
    // A pair of values for objectType and propertyName uniquely identifies values
    // for jsName and valueType.

    var cursor;
    var cursorIDPL;
    var cursorIDJS;

    if (state.foreign_retry) {
        cursorIDPL = state.foreign_value;
        cursorIDJS = atable[VAL(cursorIDPL)];
        cursor = dtpCursors.get(cursorIDJS);

    }
    else if(TAG(objectType) !== TAG_REF && TAG(propertyName) !== TAG_REF) {
        return deterministic_dom_type_property(objectType, propertyName, jsName, valueType);
    }
    else {
        cursor = {};
        if(! setupCursorForTypes(objectType, jsName, valueType, cursor)) {
            return false;
        }
        cursorIDJS = 'crs' + dtpCursorCounter++;
        dtpCursors.set(cursorIDJS, cursor);
        cursorIDPL = lookup_atom(cursorIDJS);

        create_choicepoint();
    }

    update_choicepoint_data(cursorIDPL);

    if (cursor.objectTypes && cursor.objectTypes.length > 0) {
        let objectTypeJS = cursor.objectTypes[0];
        let spec = webInterfaces.get(objectTypeJS);
        if(spec) {
            let properties = spec.properties;
            if (! cursor.propertyNames) {
                if(! setupCursorForPropertyNames(properties, propertyName, cursor)) {
                    destroy_choicepoint();
                    return false;
                }
            }

            if(cursor.propertyNames && cursor.propertyNames.length > 0) {
                let propertyNameJS = cursor.propertyNames[0];
                cursor.propertyNames = cursor.propertyNames.slice(1);
                let propertySpec = properties.get(propertyNameJS);
                if(propertySpec) {
                    if (! cursor.implementingName || propertySpec.name === cursor.implementingName) {
                        let implementingName = propertySpec.name;
                        if (! cursor.propertyType || propertySpec.type === cursor.propertyType) {
                            let propertyType = propertySpec.type;
                            let propertyTypePL = method_property_type_to_term(propertyType);

                            return unify(objectType, PL_put_atom_chars(objectTypeJS)) &&
                                unify(propertyName, PL_put_atom_chars(propertyNameJS)) &&
                                unify(jsName, PL_put_atom_chars(implementingName)) &&
                                unify(valueType, propertyTypePL);
                        } else {
                            return false;
                        }
                    } else {
                        return false;
                    }
                } else {
                    destroy_choicepoint();
                    return domain_error(objectTypeJS  + ' property', PL_put_atom_chars(propertyNameJS))
                }
            } else {
                // All propertyNames for current type have been processed.
                // Set the cursor.propertyNames to undefined to force recalculation of propertyNames
                // with next type.

                cursor.propertyNames = undefined;

                // set cursor.objectTypes to next objectType for retry.
                cursor.objectTypes = cursor.objectTypes.slice(1);
                return false;
            }
         } else {
            destroy_choicepoint();
            return domain_error('web api interface type', PL_put_atom_chars(objectTypeJS));
        }
    } else {
        destroy_choicepoint();
        return false;
    }
}

function deterministic_dom_type_property(objectType, propertyName, jsName, valueType) {
    if(TAG(objectType) === TAG_REF) {
        return instantiation_error(objectType);
    } if (TAG(objectType) !== TAG_ATM) {
        return type_error('atom', objectType);
    }

    let objectTypeJS = PL_get_atom_chars(objectType);

    if(TAG(propertyName) === TAG_REF) {
        return instantiation_error(propertyName);
    } if (TAG(propertyName) !== TAG_ATM) {
        return type_error('atom', propertyName);
    }

    let propertyNameJS = PL_get_atom_chars(propertyName);

    let spec = webInterfaces.get(objectTypeJS);
    if(spec) {
        let properties = spec.properties;
        if(properties) {
            let propertySpec = properties.get(propertyNameJS);
            if(propertySpec) {
                let implementingName = propertySpec.name;
                let propertyType = propertySpec.type;
                return unify(jsName, PL_put_atom_chars(implementingName)) &&
                    unify(valueType, PL_put_atom_chars(propertyType));
            } else {
                return false;
            }
        } else {
            return engine_error('Web API Interface type ' + objectTypeJS + ' has specification without a "properties" section : ' + JSON.stringify(spec));
        }
    } else {
        return domain_error('web api interface type', objectTypeJS);
    }
}

function setupCursorForTypes(objectType, jsName, valueType, cursor) {
    let container = {};
    if(!setupInterfaceSpecificationsForType(objectType, container)) {
        return false;
    }
    cursor.objectTypes = container.value;

    if(TAG(jsName) !== TAG_REF) {
        if(TAG(jsName) !== TAG_ATM) {
            return type_error('atom', jsName);
        }
        cursor.implementingName = PL_get_atom_chars(jsName);
    }

    if(TAG(valueType) !== TAG_REF) {
        if(TAG(valueType) !== TAG_ATM) {
            return type_error('atom', valueType);
        }
        cursor.propertyType = PL_get_atom_chars(valueType);
    }

    return true;
}

function setupCursorForPropertyNames(properties, propertyName, cursor) {
    if (! properties) {
        cursor.propertyNames = [];
    } else if (TAG(propertyName) !== TAG_REF) {
        if (TAG(propertyName) !== TAG_ATM) {
            return type_error('atom', propertyName);
        }
        let propertyNameJS = PL_get_atom_chars(propertyName);
        if (properties.get(propertyNameJS)) {
            cursor.propertyNames = [propertyNameJS];
        } else {
            cursor.propertyNames = [];
        }
    } else {
        cursor.propertyNames = Array.from(properties.keys());
    }

    return true;
}

// Find the related values for a WebInterface API object type, a method name implemented for that type, the Javascript Web API function name used
// to implement that method, the types of arguments for that method, and the result type (possibly undefined).
//
// All binding patterns are supported: objectType REF or ATM, methodName REF or ATM, implementationName REF or ATM, argumentTypes REF or LST,
// and resultType REF or ATM.

function predicate_dom_type_method(objectType, methodName, implementationName, argumentTypes, resultType) {
    // The function uses fail/retry to implement a doubly-nested loop: the outer loop
    // iterates over the Web Interface API objectTypes, the inner loop iterates over
    // the methodNames defined for the current objectType of the outer loop.
    // A pair of values for objectType and methodName uniquely identifies values
    // for implementationName, argumentTypes, and resultTYpe.

    var cursor;
    var cursorIDPL;
    var cursorIDJS;

    if (state.foreign_retry) {
        cursorIDPL = state.foreign_value;
        cursorIDJS = atable[VAL(cursorIDPL)];
        cursor = dtmCursors.get(cursorIDJS);

    }
    else if(TAG(objectType) !== TAG_REF && TAG(methodName) !== TAG_REF) {
        return deterministic_dom_type_method(objectType, methodName, implementationName, argumentTypes, resultType);
    }
    else {
        cursor = {};
        if(! setupMethodCursorForTypes(objectType, cursor)) {
            return false;
        }
        cursorIDJS = 'crs' + dtmCursorCounter++;
        dtmCursors.set(cursorIDJS, cursor);
        cursorIDPL = lookup_atom(cursorIDJS);

        create_choicepoint();
    }

    update_choicepoint_data(cursorIDPL);

    if (cursor.objectTypes && cursor.objectTypes.length > 0) {
        let objectTypeJS = cursor.objectTypes[0];
        let spec = webInterfaces.get(objectTypeJS);
        if(spec) {
            let methods = spec.methods;
            if (! cursor.methodNames) {
                if(! setupCursorForMethodNames(methods, methodName, cursor)) {
                    destroy_choicepoint();
                    return false;
                }
            }

            if(cursor.methodNames && cursor.methodNames.length > 0) {
                let methodNameJS = cursor.methodNames[0];
                cursor.methodNames = cursor.methodNames.slice(1);
                let methodSpec = methods.get(methodNameJS);
                if(methodSpec) {
                    let implementingNameJS = methodSpec.name;
                    let argumentTypesJS = methodSpec.arguments;
                    let argumentTypesPL = method_property_types_to_list(argumentTypesJS);
                    let resultTypeJS = methodSpec.returns ? methodSpec.returns.type : 'void';

                    return unify(objectType, PL_put_atom_chars(objectTypeJS)) &&
                        unify(methodName, PL_put_atom_chars(methodNameJS)) &&
                        unify(implementationName, PL_put_atom_chars(implementingNameJS)) &&
                        unify(argumentTypes, argumentTypesPL) &&
                        unify(resultType, method_property_type_to_term(resultTypeJS));
                } else {
                    destroy_choicepoint();
                    return domain_error(objectTypeJS  + ' method', PL_put_atom_chars(methodNameJS))
                }
            } else {
                // All methodNames for current type have been processed.
                // Set the cursor.methodNames to undefined to force recalculation of methodNames
                // with next type.

                cursor.methodNames = undefined;

                // set cursor.objectTypes to next objectType for retry.
                cursor.objectTypes = cursor.objectTypes.slice(1);
                return false;
            }
        } else {
            destroy_choicepoint();
            return domain_error('web api interface type', PL_put_atom_chars(objectTypeJS));
        }
    } else {
        destroy_choicepoint();
        return false;
    }
}

function deterministic_dom_type_method(objectType, methodName, implementationName, argumentTypes, resultType) {
    if(TAG(objectType) === TAG_REF) {
        return instantiation_error(objectType);
    } if (TAG(objectType) !== TAG_ATM) {
        return type_error('atom', objectType);
    }

    let objectTypeJS = PL_get_atom_chars(objectType);

    if(TAG(methodName) === TAG_REF) {
        return instantiation_error(methodName);
    } if (TAG(methodName) !== TAG_ATM) {
        return type_error('atom', methodName);
    }

    let methodNameJS = PL_get_atom_chars(methodName);

    let spec = webInterfaces.get(objectTypeJS);
    if(spec) {
        let methods = spec.methods;
        if(methods) {
            let methodSpec = methods.get(methodNameJS);
            if(methodSpec) {
                let implementingNameJS = methodSpec.name;
                let argumentTypesJS = methodSpec.arguments;
                let argumentTypesPL = method_property_types_to_list(argumentTypesJS);
                let resultTypeJS = methodSpec.returns ? methodSpec.returns.type : 'void';

                return unify(objectType, PL_put_atom_chars(objectTypeJS)) &&
                    unify(methodName, PL_put_atom_chars(methodNameJS)) &&
                    unify(implementationName, PL_put_atom_chars(implementingNameJS)) &&
                    unify(argumentTypes, argumentTypesPL) &&
                    unify(resultType, method_property_type_to_term(resultTypeJS));
            } else {
                return false;
            }
        } else {
            return engine_error('Web API Interface type ' + objectTypeJS + ' has specification without a "methods" section : ' + JSON.stringify(spec));
        }
    } else {
        return domain_error('web api interface type', objectTypeJS);
    }
}

function method_property_types_to_list(typesJS) {
    if(typesJS.length === 0) {
        return NIL;
    }

    let items = [];
    for(let typeJS of typesJS) {
        items.push(method_property_type_to_term(typeJS.type));
    }

    let typesPL;

    let tmp = state.H ^ (TAG_LST << WORD_BITS);
    for (let i = 0; i < items.length; i++)
    {
        memory[state.H] = items[i];
        // If there are no more items we will overwrite the last entry with [] when we exit the loop
        memory[state.H+1] = ((state.H+2) ^ (TAG_LST << WORD_BITS));
        state.H += 2;
    }
    memory[state.H-1] = NIL;
    return tmp;

}
function method_property_type_to_term(typeJS) {
    let typePL;
    if(typeof typeJS === 'object' && typeJS.length) // list of strings
    {
        typePL = strings_to_atom_list(typeJS);
    } else if(typeof typeJS === 'object' && typeJS.arrayType)
    {
        typePL = PL_put_atom_chars('array:' + typeJS.arrayType);
    } else {
        typePL = PL_put_atom_chars(typeJS);
    }
    return typePL;
}


function setupMethodCursorForTypes(objectType, cursor) {
    let container = {};
    if (!setupInterfaceSpecificationsForType(objectType, container)) {
        return false;
    }
    cursor.objectTypes = container.value;
    return true;
}


function setupCursorForMethodNames(methods, methodName, cursor) {
    if (! methods) {
        cursor.methodNames = [];
    } else if (TAG(methodName) !== TAG_REF) {
        if (TAG(methodName) !== TAG_ATM) {
            return type_error('atom', methodName);
        }
        let methodNameJS = PL_get_atom_chars(methodName);
        if (methods.get(methodNameJS)) {
            cursor.methodNames = [methodNameJS];
        } else {
            cursor.methodNames = [];
        }
    } else {
        cursor.methodNames = Array.from(methods.keys());
    }

    return true;
}
// File web_interfaces.js
"use strict";

/*
W3C Web API specifies 'APIs', 'webInterfaces', and 'mixins'.
(It also uses the term 'object type' to refer to
these webInterfaces. Javascript (through ECMAScript 2020) does not define 'interface'
as a language concept - it continues to rely on 'duck typing' (if it walks like
a duck, sounds like a duck, then it is a duck). So the Web API webInterfaces appear
to be an informal concept.)

An interface is a collection of properties, methods, and events. It may inherit properties,
methods, and/or events from a 'parent' interface and any number of mixins.
The properties and methods of an interface are implemented by a Javascript object.
An interface may correspond to an object type, or it may only be used as part of the
definition of an object type. For instance, the Navigator interface does not 'inherit'
any properties or methods from other webInterfaces but it does implement properties and
methods defined by other webInterfaces such as NavigatorID. The NavigatorID interface is not used
as an object type: i.e. there are no Javascript objects in the W3C Web API
implementations that are created solely from the NavigatorID interface.
[I do not know why the Web API documentation does not consider an object
that implements the Navigator interface (and thus is of the Navigator
object type) to not also be of the NavigatorID interface and object type. Perhaps because
it is not completely described by the NavigatorID interface?]

A mixin is similar to an interface but "...you can't create an object of type [mixin]".
It appears that a mixin is the same as the non-object-type webInterfaces mentioned above
(such as NavigatorID), but the WebAPI documentation does not make that connection. An
example mixin is WindowEventHandlers:
"[it] describes the event handlers common to several webInterfaces like Window,
or HTMLBodyElement and HTMLFrameSetElement. Each of these webInterfaces can
implement additional specific event handlers."

An API is a collection of webInterfaces, a collection of properties for
other webInterfaces, and a collection of methods for other webInterfaces.
More precisely, API X has webInterfaces [I1, ..., In],
properties [Ip1.P1,...,Ip.Pj],
and methods [Im.M1(...), ..., Im.Mk(...)], where:
  * Ia != Ib for all 1 <= a <= n, 1 <= b <= n, and a != b;
  * Ipx != Ia for all 1 <= x <= j and 1 <= a <= n;
  * Imx != Ia for all 1 <= x <= k and 1 <= a <= n.

The ProscriptLS implementation provides explicit access to selected properties and methods
of various WebAPI objects and Javascript runtime objects.
Currently the supported WebAPI objects are Node, Element, and HTMLElement.
The Javascript runtime object is Promise.

The properties are handled in the Javascript supporting ProscriptLS
in a systematic fashion using a propertyMap to map
a property name to a property specification. The property specification defines
the name and type properties and the objects(valueJS), objectValues(objectJS),
and setValue(property, objectJS, value) methods.

The name property is the same name as is used in the propertyMap key.

The type property is the type of the return value for the property. The
propertyValueToJS(type, valueJS) function translates a Javascript
property value to its Prolog representation. The defined types are:
'atom', 'number', 'string', 'object'. The 'object' type translates
a Javascript object to a Prolog representation by looking up the
object in a map. If the object is already in the map then it returns
the identifier stored in the map. If it is not already known then it
generates a unique identifier for that object and stores it in the map.

The objects(valueJS) method returns an Array of the objects that have
the 'name' property with value 'valueJS'. This may return 'undefined'
if no mapping from property value to object is available.

The objectValues(objectJS) method returns an Array of Javascript
values for the 'name' property of object 'objectJS'.

The setValue(property, objectJS, value) method sets the 'property'
of 'objectJS' to 'value'. This may set 'exception' to domain_error if
the property is read-only.

A property that is a single value that can be accessed
by "elementJS[propertyName]" is defined using specification
return by the SimpleProperty(type, propertyName, settable) method.

Other specification-returning methods are used for properties
that do not meet the constraints of the SimpleProperty method.

*/

function TagProperty() {
    var that = {};
    that.name = "tag";
    that.type = 'atom';
    that.objects = function(valueJS) {
        return Array.from(document.getElementsByTagName(valueJS));
    };
    that.elementValuesFunction = function(elementJS) {
        var values = [];
        values.push(elementJS.tagName);
        return values;
    };
    // noinspection JSUnusedLocalSymbols
    that.setValue = function(property, elementJS, value) {
        domain_error(property);
    };
    return that;
}

function ChildNodeProperty() {
    var that = {};
    that.name = "childNode";
    that.type = 'object';
    that.objects = function(valueJS) {
        var objects = [];
        objects.push(valueJS.parentNode);
        return objects;
    };
    that.elementValuesFunction = function(objectJS) {
        /** @namespace elementJS.children */
        return [...objectJS.childNodes];// This is the spread operator. It creates an array from the NodeList of 'childNodes'.
    };
    // noinspection JSUnusedLocalSymbols
    that.setValue = function(property, elementJS, value) {
        domain_error(property);
    };
    return that;
}

function SimpleChildNodeProperty(propertyName) {
    var that = {};
    that.name = "firstChild";
    that.type = 'object';
    that.objects = function(valueJS) {
        var objects = [];
        objects.push(valueJS.parentElement);
        return objects;
    };
    that.elementValuesFunction = function(elementJS) {
        var objects = [];
        let propertyValue = elementJS[propertyName];
        if(typeof propertyValue !== 'undefined' && propertyValue !== null) {
            objects.push(propertyValue);
        }
        return objects;
    };
    // noinspection JSUnusedLocalSymbols
    that.setValue = function(property, elementJS, value) {
        domain_error(property);
    };
    return that;
}

function ChildProperty() {
    var that = {};
    that.name = "child";
    that.type = 'object';
    that.objects = function(valueJS) {
        var objects = [];
        objects.push(valueJS.parentElement);
        return objects;
    };
    that.elementValuesFunction = function(elementJS) {
        /** @namespace elementJS.children */
        return [...elementJS.children];// This is the spread operator. It creates an array from the HTMLCollection of 'children'.
    };
    // noinspection JSUnusedLocalSymbols
    that.setValue = function(property, elementJS, value) {
        domain_error(property);
    };
    return that;
}

function SimpleChildProperty(propertyName) {
    var that = {};
    that.name = propertyName;
    that.type = 'object';
    that.objects = function(valueJS) {
        var objects = [];
        objects.push(valueJS.parentElement);
        return objects;
    };
    that.elementValuesFunction = function(elementJS) {
        var objects = [];
        let value = elementJS[propertyName];
        if(typeof value !== 'undefined' && value !== null) {
            objects.push(value);
        }
        return objects;
    };
    // noinspection JSUnusedLocalSymbols
    that.setValue = function(property, elementJS, value) {
        domain_error(property);
    };
    return that;
}

/**
 * This prolog 'class' property uses the classList and className HTML Element properties.
 */

function ClassProperty() {
    var that = {};
    that.name = "class";
    that.type = 'atom';
    that.objects = function(valueJS) {
        return document.getElementsByClassName(valueJS);
    };
    that.elementValuesFunction = function(elementJS) {
        /** @namespace elementJS.classList */
        return [...elementJS.classList];// This is the spread operator. It creates an array from the DOMTokenList of 'classList'.
    };

    /**
     * Set the 'className' property to the given class or classes.
     * If the 'value' is a single atom then set the className to the string for that atom.
     * If the 'value' is a list of atoms then set the className to the
     * string that is the strings for those atoms separated by spaces.
     * @param property
     * @param elementJS
     * @param value
     */
    that.setValue = function(property, elementJS, value) {
        var valueJS;
        if (TAG(element) === TAG_ATM) {
            let container = {};
            if(getAtomPropertyValue(value, container)) {
                valueJS = container.value;
            } else {
                // leave valueJS undefined.
            }
        } else if (TAG(element) === TAG_LST) {
            valueJS = getClassListPropertyValue(value);
        }
        elementJS.className = valueJS;
    };
    return that;
}

function SimpleProperty(type, propertyName, settable) {
    var that = {};
    that.name = propertyName;
    that.type = type;
    // noinspection JSUnusedLocalSymbols
    that.objects = function(valueJS) {
        return Array.from(document.querySelectorAll('*')); // return all objects for later filtering by unification
    };
    that.elementValuesFunction = function(elementJS) {
        var values = [];
        let value = elementJS[propertyName];
        if(typeof value !== 'undefined' && value !== null) {
            if(typeof value === 'object' && value.constructor.name === 'NodeList') {
                values = Array.from(value);
            } else if(typeof value === 'object' && value.constructor.name === 'FileList') {
                values = Array.from(value);
            } else if(typeof value === 'object' && value.constructor.name === 'HTMLOptionsCollection') {
                values = Array.from(value);
            } else if(typeof value === 'object' && value.constructor.name === 'HTMLCollection') {
                values = Array.from(value);
            } else {
                values.push(value);
            }
        }
        return values;
    };
    that.setValue = function(property, elementJS, value) {
        if(settable) {
            let container = {};
            if(propertyValueToJS(type, value, container, true)) {
                elementJS[propertyName] = container.value;
                return true;
            } else {
                return false;
            }
        } else {
            return domain_error(property);
        }
    };
    return that;
}

var webInterfaces = new Map();

var eventTargetMethodSpecs = new Map([
    ['addEventListener',{name:'addEventListener',arguments:[{type:'string'},{type:'goal_function'}]}],
    ['removeEventListener',{name:'removeEventListener',arguments:[{type:'string'},{type:'goal_function'}]}],
    ['dispatchEvent',{name:'dispatchEvent',arguments:[{type:'event'}],returns:{type:'boolean'}}]
]);

webInterfaces.set('eventtarget',
    {
        name: 'eventtarget',
        methods: eventTargetMethodSpecs,
        reference: {name:'EventTarget',
            standard:'https://www.w3.org/TR/2018/WD-dom41-20180201/#interface-eventtarget',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/EventTarget'
        }
    });

var nodeInterfaceProperties = new Map([
    // baseURI
    // baseURIObject
    ['childNode', ChildNodeProperty()], // adapted from childNodes
    ['firstChild', SimpleChildNodeProperty('firstChild')],
    // isConnected
    ['lastChild', SimpleChildProperty('lastChild')],
    //['namespaceURI', SimpleProperty('string', 'namespaceURI')], // deprecated in Node interface: moved to Element.
    ['nextSibling', SimpleChildProperty('nextSibling')],
    ['nodeName', SimpleProperty('atom','nodeName')],
    ['nodeType', SimpleProperty('number','nodeType')],
    ['nodeValue', SimpleProperty('string','nodeValue', true)],
    // outerText
    ['ownerDocument', SimpleChildProperty('ownerDocument')],
    ['parentElement', SimpleChildProperty('parentElement')],
    ['parentNode', SimpleChildProperty('parentNode')],
    ['previousSibling', SimpleChildProperty('previousSibling')],
    ['textContent', SimpleProperty('string', 'textContent', true)]
]);

var nodeMethodSpecs = new Map([
    ['cloneNode',{name:'cloneNode',arguments:[{type:'boolean'}],returns:{type:'object'}}],
    ['compareDocumentPosition',{name:'compareDocumentPosition',arguments:[{type:'object'}],returns:{type:'number'}}],
    ['contains',{name:'contains',arguments:[{type:'object'}],returns:{type:'boolean'}}],
    ['isEqualNode',{name:'isEqualNode',arguments:[{type:'object'}],returns:{type:'boolean'}}],
    ['normalize',{name:'normalize',arguments:[]}],
    ['removeChild',{name:'removeChild',arguments:[{type:'object'}]}]
]);

webInterfaces.set('node',
    {
        name: 'node',
        parent: ['eventtarget'],
        properties:nodeInterfaceProperties,
        methods:nodeMethodSpecs,
        reference: {name:'Node',
            standard:'https://www.w3.org/TR/2018/WD-dom41-20180201/#node',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/Node'
        }
    });
/*
interface ParentNode {
  [SameObject] readonly attribute HTMLCollection children;
  readonly attribute Element? firstElementChild;
  readonly attribute Element? lastElementChild;
  readonly attribute unsigned long childElementCount;

  [CEReactions, Unscopable] void prepend((Node or DOMString)... nodes);
  [CEReactions, Unscopable] void append((Node or DOMString)... nodes);

  Element? querySelector(DOMString selectors);
  [NewObject] NodeList querySelectorAll(DOMString selectors);
};

 */
var parentNodeInterfaceProperties = new Map([
    ['child', ChildProperty()], // adapted from children
    ['childElementCount', SimpleProperty('number','childElementCount')],
    ['firstElementChild', SimpleChildProperty('firstElementChild')],
    ['lastElementChild', SimpleChildProperty('lastElementChild')]
]);

var parentNodeMethodSpecs = new Map([
    ['prepend',{name:'prepend',arguments:[{type: {arrayType:['object','string']}}]}], // list of Node or DOMString
    ['append',{name:'append',arguments:[{type:{arrayType:['object','string']}}]}], // list of Node or DOMString
    ['querySelector',{name:'querySelector',arguments:[{type:'string'}],returns:{type:'object'}}], // Element
    ['querySelectorAll',{name:'querySelectorAll',arguments:[{type:'string'}],returns:{type:'object', multiple:true}}], // NodeList
]);

webInterfaces.set('parentnode',
    {
        name: 'parentnode',
        parent: [],
        properties:parentNodeInterfaceProperties,
        methods:parentNodeMethodSpecs,
        reference: {name:'ParentNode',
            standard:'https://www.w3.org/TR/2018/WD-dom41-20180201/#parentnode',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/ParentNode'
        }
    });

var documentInterfaceProperties = new Map([
    ['URL', SimpleProperty('string','URL')],
    ['documentURI', SimpleProperty('string','documentURI')],
    ['origin', SimpleProperty('string','origin')],
    ['compatMode', SimpleProperty('atom','compatMode')],
    ['characterSet', SimpleProperty('atom','characterSet')],
    ['contentType', SimpleProperty('atom','contentType')],
    ['docType', SimpleProperty('object','docType')], // DocumentType
    ['documentElement', SimpleProperty('object','documentElement')] // Element
]);

var documentMethodSpecs = new Map([
    ['getElementsByTagName',{name:'getElementsByTagName',arguments:[{type:'string'}],returns:{type:'object'}}],
    ['getElementsByTagNameNS',{name:'getElementsByTagNameNS',arguments:[{type:'string'},{type:'string'}],returns:{type:'object'}}],
    ['createElement',{name:'createElement',arguments:[{type:'string'},{type:'object'}],returns:{type:'object'}}], // input ElementCreationOptions, output Element
    ['createElementNS',{name:'createElementNS',arguments:[{type:'string'},{type:'string'},{type:'object'}],returns:{type:'object'}}], // input ElementCreationOptions, output Element
    ['createDocumentFragment',{name:'createDocumentFragment',arguments:[],returns:{type:'object'}}], // DocumentFragment
    ['createTextNode',{name:'createTextNode',arguments:[{type:'string'}],returns:{type:'object'}}], // Text
    ['createCDATASection',{name:'createCDATASection',arguments:[{type:'string'}],returns:{type:'object'}}], // CDATASection
    ['createComment',{name:'createComment',arguments:[{type:'string'}],returns:{type:'object'}}], // Comment
    ['createProcessingInstruction',{name:'createProcessingInstruction',arguments:[{type:'string'},{type:'string'}],returns:{type:'object'}}], // ProcessingInstruction
    ['importNode',{name:'importNode',arguments:[{type:'object'},{type:'boolean'}],returns:{type:'object'}}], // input Node, output Node
    ['adoptNode',{name:'adoptNode',arguments:[{type:'object'}],returns:{type:'object'}}], // input Node, output Node
    ['createAttribute',{name:'createAttribute',arguments:[{type:'string'}],returns:{type:'object'}}], // Attr
    ['createAttributeNS',{name:'createAttributeNS',arguments:[{type:'string'},{type:'string'}],returns:{type:'object'}}], // Attr
    ['createEvent',{name:'createEvent',arguments:[{type:'string'}],returns:{type:'object'}}], // Event
    ['createRange',{name:'createRange',arguments:[{type:'string'}],returns:{type:'object'}}], // Range
    ['createNodeIterator',{name:'createNodeIterator',arguments:[{type:'object'},{type:'number'},{type:'object'}],returns:{type:'object'}}], // input Node, NodeFilter, output NodeIterator
    ['createTreeWalker',{name:'createTreeWalker',arguments:[{type:'object'},{type:'number'},{type:'object'}],returns:{type:'object'}}], // input Node, NodeFilter, output TreeWalker
]);

webInterfaces.set('document',
    {
        name: 'document',
        parent: ['node'],
        properties:documentInterfaceProperties,
        methods:documentMethodSpecs,
        reference: {name:'Document',
            standard:'https://www.w3.org/TR/2018/WD-dom41-20180201/#document',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/Document'
        }
    });

var documentFragmentInterfaceProperties = new Map( [
    ['is', SimpleProperty('atom', 'is')]

]);

var documentFragmentMethodSpecs = new Map([
]);

webInterfaces.set('documentfragment',
    {name: 'documentfragment',
        parent: ['node'],
        properties:documentFragmentInterfaceProperties,
        methods:documentFragmentMethodSpecs,
        reference: {name:'DocumentFragment',
            standard:'https://www.w3.org/TR/2018/WD-dom41-20180201/#documentfragment',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/DocumentFragment'
        }
    });

var windowInterfaceProperties = new Map([
    // window
    // self
    ['document', SimpleProperty('object','document')], // Document
    ['name', SimpleProperty('atom','name')],
    ['location', SimpleProperty('object','location')], // Location
    ['history', SimpleProperty('object','history')], // History
    ['customElements', SimpleProperty('object','customElements')], // CustomElementRegistry
    ['locationbar', SimpleProperty('object','locationbar')], // BarProp
    ['menubar', SimpleProperty('object','menubar')], // BarProp
    ['personalbar', SimpleProperty('object','personalbar')], // BarProp
    ['scrollbars', SimpleProperty('object','scrollbars')], // BarProp
    ['statusbar', SimpleProperty('object','statusbar')], // BarProp
    ['toolbar', SimpleProperty('object','toolbar')], // BarProp
    ['status', SimpleProperty('atom','status')],
    ['closed', SimpleProperty('boolean','closed')],
    // frames
    ['length', SimpleProperty('number','length')],
    // top
    // opener
    // parent
    ['frameElement', SimpleProperty('object','frameElement')], // Element
    ['navigator', SimpleProperty('object', 'navigator')], // Navigator
    ['applicationCache', SimpleProperty('object', 'applicationCache')] // ApplicationCache
]);

var windowMethodSpecs = new Map([
    ['close',{name:'close',arguments:[]}],
    ['stop',{name:'stop',arguments:[]}],
    ['focus',{name:'focus',arguments:[]}],
    ['blur',{name:'blur',arguments:[]}],
    //   WindowProxy? open(optional USVString url = "about:blank", optional DOMString target = "_blank", optional [TreatNullAs=EmptyString] DOMString features = "");
    ['open', {name: 'open',arguments:[{type:'string'},{type:'string'},{type:'string'}],returns:{type:'object'}}], // WindowProxy
    ['alert',{name:'alert',arguments:[{type:'string'}]}],
    ['confirm',{name:'confirm',arguments:[{type:'string'}],returns:{type:'boolean'}}],
    ['prompt',{name:'prompt',arguments:[{type:'string'}],returns:{type:'string'}}],
    ['print',{name:'print',arguments:[]}],
//    void postMessage(any message, USVString targetOrigin, optional sequence<object> transfer = []);
//void postMessage(any message, optional WindowPostMessageOptions options);
    ['postMessageOrigin',{name:'postMessage',arguments:[{type:'string'},{type:'string'},{type:{arrayType:'object'}}]}],
    ['postMessage',{name:'postMessage',arguments:[{type:'string'}, {type:'object'}]}] // WindowPostMessageOptions
]);


//Window includes GlobalEventHandlers;
//Window includes WindowEventHandlers;
//Window includes WindowLocalStorage;
//Window includes WindowSessionStorage;

webInterfaces.set('window',
    {
        name: 'window',
        parent: ['eventtarget'],
        properties:windowInterfaceProperties,
        methods:windowMethodSpecs,
        reference: {name:'Window',
            standard:'https://html.spec.whatwg.org/multipage/window-object.html',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/Window'
        }
    });


var elementInterfaceProperties = new Map([
    ['accessKey', SimpleProperty('atom','accessKey', true)],
    // attributes: available using dom_element_attribute_value
    ['class', ClassProperty()], // adapted from classList, className
    ['clientHeight', SimpleProperty('number','clientHeight')],
    ['clientLeft', SimpleProperty('number','clientLeft')],
    ['clientTop', SimpleProperty('number','clientTop')],
    ['clientWidth', SimpleProperty('number','clientWidth')],
    // currentStyle: available using dom_element_attribute_value?
    ['id', SimpleProperty('atom','id', true)],
    ['innerHTML', SimpleProperty('string', 'innerHTML', true)],
    // name: available using dom_element_attribute_value
    ['namespaceURI', SimpleProperty('string', 'namespaceURI')],
    ['nextElementSibling', SimpleChildProperty('nextElementSibling')],
    ['previousElementSibling', SimpleChildProperty('previousElementSibling')],
    // runtimeStyle: available using dom_element_attribute_value?
    ['scrollHeight', SimpleProperty('number','scrollHeight')],
    ['scrollLeft', SimpleProperty('number','scrollLeft')],
    // scrollLeftMax: available using dom_element_attribute_value?
    ['scrollTop', SimpleProperty('number','scrollTop')],
    // scrollTopMax: available using dom_element_attribute_value?
    ['scrollWidth', SimpleProperty('number','scrollWidth')],
    ['tag', TagProperty()] // for tagName
]);

var elementMethodSpecs = new Map([
    ['getBoundingClientRect',{name:'getBoundingClientRect',arguments:[],returns:{type:'dom_rect'}}],
    ['insertAdjacentElement',{name:'insertAdjacentElement',arguments:[{type:'position'},{type:'object'}]}],
    ['insertAdjacentHTML',{name:'insertAdjacentHTML',arguments:[{type:'position'},{type:'string_codes'}]}],
    ['insertAdjacentText',{name:'insertAdjacentText',arguments:[{type:'position'},{type:'string_codes'}]}],
    ['scrollIntoView',{name:'scrollIntoView',arguments:[{type:['boolean', 'options']}]}]
]);

webInterfaces.set('element',
    {
        name: 'element',
        parent: ['node'],
        properties:elementInterfaceProperties,
        methods: elementMethodSpecs,
        reference: {name:'Element',
            standard:'https://www.w3.org/TR/2018/WD-dom41-20180201/#element',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/Element'
        }
    });

var htmlElementInterfaceProperties = new Map([
    ['contentEditable', SimpleProperty('atom','contentEditable', true)],
    // contextMenu, deprecated
    // dataset
    ['dir', SimpleProperty('atom','dir', true)], // rtl, ltr, auto
    // hidden
    ['innerText', SimpleProperty('string', 'innerText', true)],
    ['lang', SimpleProperty('atom','lang', true)], // ISO 639-1 Language Codes: en, de, ja, ...
    // nonce, experimental
    ['offsetHeight', SimpleProperty('number','offsetHeight')],
    ['offsetLeft', SimpleProperty('number','offsetLeft')],
    ['offsetParent', SimpleProperty('object','offsetParent')],
    ['offsetTop', SimpleProperty('number','offsetTop')],
    ['offsetWidth', SimpleProperty('number','offsetWidth')],
    // onabort, experimental
    // onanimationcancel
    // onanimationend
    // onanimationiteration, experimental
    // onauxclick, experimental
    // onblur
    // oncancel
    // oncanplay
    // oncanplaythrough
    // onchange
    // onclick
    // onclose, experimental
    // oncontextmenu
    // oncopy, experimental
    // oncuechange
    // oncut, experimental
    // ondblclick
    // ondurationchange
    // onended
    // onerror
    // onfocus
    // ongotpointercapture
    // oninput
    // oninvalid
    // onkeydown
    // onkeypress
    // onkeyup
    // onload
    // onloadeddata
    // onloadedmetadata
    // onloadend
    // onloadstart
    // onlostpointercapture
    // onmousedown
    // onmouseenter
    // onmouseleave
    // onmousemove
    // onmouseout
    // onmouseover
    // onmouseup
    // onpaste, experimental
    // onplay
    // onpointercancel
    // onpointerdown
    // onpointerenter
    // onpointerleave
    // onpointermove
    // onpointerout
    // onpointerover
    // onpointerup
    // onreset
    // onresize
    // onscroll
    // onselect
    // onselectionchange, experimental
    // onselectstart, experimental
    // onsubmit
    // ontouchcancel, experimental
    // ontouchstart, experimental
    // ontransitioncancel
    // ontransitionend
    // onwheel
    // outerText
    ['style', SimpleProperty('object', 'style', true)], // object has a CSSStyleDeclaration interface.
    ['tabIndex', SimpleProperty('number','tabIndex')],
    ['title', SimpleProperty('string', 'title', true)]
]);

var htmlElementMethodSpecs = new Map([
    ['blur',{name:'blur',arguments:[]}],
    ['click',{name:'click',arguments:[]}],
    ['focus',{name:'focus',arguments:[]}]
]);

webInterfaces.set('htmlelement',
    {name: 'htmlelement',
        parent: ['element'],
        properties:htmlElementInterfaceProperties,
        methods:htmlElementMethodSpecs,
        reference: {name:'HTMLElement',
            standard:'https://www.w3.org/TR/2017/REC-html52-20171214/dom.html#htmlelement',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/HTMLElement'
        }
    });

var htmlCanvasElementInterfaceProperties = new Map( [
    ['height', SimpleProperty('number', 'height', true)],
    ['width', SimpleProperty('number', 'width', true)]
]);

var htmlCanvasElementMethodSpecs = new Map([
    ['getContext',{name:'getContext',arguments:[{type:'string'}],returns:{type:'object'}}], // arg is '2d' or 'webgl'. return is CanvasRenderingContext2D or WebGLRenderingContext
    ['toBlob',{name:'toBlob',arguments:[{type:'goal_function'},{type:'string'},{type:'float'}]}],
    ['toDataURL',{name:'toDataURL',arguments:[{type:'string'},{type:'float'}],returns:{type:'string_codes'}}], // 2nd arg is between 0 and 1. Result is a data URL.
    ['removeProperty',{name:'removeProperty',arguments:[{type:'string'}],returns:{type:'atom'}}],
    ['setProperty',{name:'setProperty',arguments:[{type:'string'},{type:'string'},{type:'string'}]}]
]);

webInterfaces.set('htmlcanvaselement',
    {name: 'htmlcanvaselement',
        properties:htmlCanvasElementInterfaceProperties,
        methods:htmlCanvasElementMethodSpecs,
        reference: {name:'HTMLCanvasElement',
            standard:'https://www.w3.org/TR/html5/semantics-scripting.html#the-canvas-element',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/HTMLCanvasElement'
        }
    });

var htmlTextAreaElementInterfaceProperties = new Map( [
 //   ['autocomplete', SimpleProperty('atom', 'autocomplete', true)], // experimental, according to mozilla
    ['autofocus', SimpleProperty('boolean', 'autofocus', true)],
    ['cols', SimpleProperty('number', 'cols', true)], // not Input
    ['dirName', SimpleProperty('atom', 'dirName', true)],
    ['disabled', SimpleProperty('boolean', 'disabled', true)],
    ['form', SimpleProperty('object', 'form')],
//    ['inputMode', SimpleProperty('atom', 'inputMode', true)], // experimental, according to mozilla
    ['maxLength', SimpleProperty('number', 'maxLength', true)],
    ['minLength', SimpleProperty('number', 'minLength', true)],
    ['name', SimpleProperty('atom', 'name', true)],
    ['placeholder', SimpleProperty('atom', 'placeholder', true)],
    ['readOnly', SimpleProperty('boolean', 'readOnly', true)],
    ['required', SimpleProperty('boolean', 'required', true)],
    ['rows', SimpleProperty('number', 'rows', true)], // not Input
    ['wrap', SimpleProperty('atom', 'wrap', true)], // not Input
    ['type', SimpleProperty('atom', 'type', true)],
    ['defaultValue', SimpleProperty('atom', 'defaultValue', true)],
    ['value', SimpleProperty('atom', 'value', true)],
    ['textLength', SimpleProperty('number', 'textLength')], // not Input
    ['willValidate', SimpleProperty('boolean', 'willValidate')],
    ['validity', SimpleProperty('object', 'validity')], // ValidityState
    ['validationMessage', SimpleProperty('atom', 'validationMessage')],
    ['labels', SimpleProperty('object', 'labels')], // NodeList returned item-by-item as objects
    ['selectionStart', SimpleProperty('number', 'selectionStart', true)],
    ['selectionEnd', SimpleProperty('number', 'selectionEnd', true)],
    ['selectionDirection', SimpleProperty('atom', 'selectionDirection', true)]
]);

var htmlTextAreaElementMethodSpecs = new Map([
    ['checkValidity',{name:'checkValidity',arguments:[],returns:{type:'boolean'}}],
    ['reportValidity',{name:'reportValidity',arguments:[],returns:{type:'boolean'}}],
    ['setCustomValidity',{name:'setCustomValidity',arguments:[{type:'string'}]}],
    ['select',{name:'select',arguments:[]}],
    ['setRangeText',{name:'setRangeText',arguments:[{type:'string'},{type:'number'},{type:'number'},{type:'string'}]}],
    ['setSelectionRange',{name:'setSelectionRange',arguments:[{type:'number'},{type:'number'},{type:'string'}]}]
]);

webInterfaces.set('htmltextareaelement',
    {name: 'htmltextareaelement',
        properties:htmlTextAreaElementInterfaceProperties,
        methods:htmlTextAreaElementMethodSpecs,
        reference: {name:'HTMLTextAreaElement',
            standard:'https://www.w3.org/TR/2017/REC-html52-20171214/sec-forms.html#htmltextareaelement',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/HTMLTextAreaElement'
        }
    });

var htmlInputElementInterfaceProperties = new Map( [
    ['accept', SimpleProperty('atom', 'accept', true)],
    ['alt', SimpleProperty('string', 'alt', true)],
    ['autocomplete', SimpleProperty('atom', 'autocomplete', true)],
    ['autofocus', SimpleProperty('boolean', 'autofocus', true)],
    ['defaultChecked', SimpleProperty('boolean', 'defaultChecked', true)],
    ['checked', SimpleProperty('boolean', 'checked', true)],
    ['dirName', SimpleProperty('atom', 'dirName', true)],
    ['disabled', SimpleProperty('boolean', 'disabled', true)],
    ['form', SimpleProperty('object', 'form')],
    ['files', SimpleProperty('object', 'files')],
    ['formAction', SimpleProperty('atom', 'formAction', true)],
    ['formEnctype', SimpleProperty('atom', 'formEnctype', true)],
    ['formMethod', SimpleProperty('atom', 'formMethod', true)],
    ['formNoValidate', SimpleProperty('boolean', 'formNoValidate', true)],
    ['formTarget', SimpleProperty('atom', 'formTarget', true)],
    ['height', SimpleProperty('number', 'height', true)],
    ['indeterminate', SimpleProperty('boolean', 'indeterminate', true)],
    //['inputMode', SimpleProperty('atom', 'inputMode', true)], // not supported, but is in html 5.1 standard
    //['list', SimpleProperty('object', 'list')], // not widely supported, but is in html 5.1 standard
    ['max', SimpleProperty('atom', 'max', true)],
    ['maxLength', SimpleProperty('number', 'maxLength', true)],
    ['min', SimpleProperty('atom', 'min', true)],
    ['minLength', SimpleProperty('number', 'minLength', true)],
    ['multiple', SimpleProperty('boolean', 'multiple', true)],
    ['name', SimpleProperty('atom', 'name', true)],
    ['pattern', SimpleProperty('atom', 'pattern', true)],
    ['placeholder', SimpleProperty('atom', 'placeholder', true)],
    ['readOnly', SimpleProperty('boolean', 'readOnly', true)],
    ['required', SimpleProperty('boolean', 'required', true)],
    ['size', SimpleProperty('number', 'size', true)],
    ['src', SimpleProperty('atom', 'src', true)],
    ['step', SimpleProperty('atom', 'step', true)],
    ['type', SimpleProperty('atom', 'type', true)],
    ['defaultValue', SimpleProperty('atom', 'defaultValue', true)],
    ['value', SimpleProperty('atom', 'value', true)],
    //['valueAsDate', SimpleProperty('object', 'valueAsDate', true)], // only applies to type=date and type=date is not supported in Safari.
    ['valueAsNumber', SimpleProperty('number', 'valueAsNumber', true)],
    ['width', SimpleProperty('number', 'width', true)],
    ['willValidate', SimpleProperty('boolean', 'willValidate')],
    ['validity', SimpleProperty('object', 'validity')], // ValidityState
    ['validationMessage', SimpleProperty('atom', 'validationMessage')],
    ['labels', SimpleProperty('object', 'labels')], // NodeList
    ['selectionStart', SimpleProperty('number', 'selectionStart', true)],
    ['selectionEnd', SimpleProperty('number', 'selectionEnd', true)],
    ['selectionDirection', SimpleProperty('atom', 'selectionDirection', true)]
]);

var htmlInputElementMethodSpecs = new Map([
    ['stepUp',{name:'stepUp',arguments:[{type:'number'}]}],
    ['stepDown',{name:'stepDown',arguments:[{type:'number'}]}],
    ['checkValidity',{name:'checkValidity',arguments:[],returns:{type:'boolean'}}],
    ['reportValidity',{name:'reportValidity',arguments:[],returns:{type:'boolean'}}],
    ['setCustomValidity',{name:'setCustomValidity',arguments:[{type:'string'}]}],
    ['select',{name:'select',arguments:[]}],
    ['setRangeText',{name:'setRangeText',arguments:[{type:'string'},{type:'number'},{type:'number'},{type:'string'}]}],
    ['setSelectionRange',{name:'setSelectionRange',arguments:[{type:'number'},{type:'number'},{type:'string'}]}]
]);

webInterfaces.set('htmlinputelement',
    {name: 'htmlinputelement',
        properties:htmlInputElementInterfaceProperties,
        methods:htmlInputElementMethodSpecs,
        reference: {name:'HTMLInputElement',
            standard:'https://www.w3.org/TR/2017/REC-html52-20171214/sec-forms.html#htmlinputelement',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/HTMLInputElement'
        }
    });

var htmlSelectElementInterfaceProperties = new Map( [
    ['autofocus', SimpleProperty('boolean', 'autofocus', true)],
    ['disabled', SimpleProperty('boolean', 'disabled', true)],
    ['form', SimpleProperty('object', 'form')],
    ['labels', SimpleProperty('object', 'labels')], // NodeList returned item-by-item as objects
    ['length', SimpleProperty('number', 'length', true)],
    ['multiple', SimpleProperty('boolean', 'multiple', true)],
    ['name', SimpleProperty('atom', 'name', true)],
    ['options', SimpleProperty('object', 'options')],
    ['required', SimpleProperty('boolean', 'required', true)],
    ['selectedIndex', SimpleProperty('number', 'selectedIndex', true)],
    ['selectedOptions', SimpleProperty('object', 'selectedOptions')],
    ['size', SimpleProperty('number', 'size', true)],
    ['type', SimpleProperty('atom', 'type', true)],
    ['validity', SimpleProperty('object', 'validity')], // ValidityState
    ['validationMessage', SimpleProperty('atom', 'validationMessage')],
    ['value', SimpleProperty('atom', 'value', true)],
    ['willValidate', SimpleProperty('boolean', 'willValidate')]
 ]);

var htmlSelectElementMethodSpecs = new Map([
    ['add',{name:'add',arguments:[{type:'object'},{type:['object','number']}]}],
    ['checkValidity',{name:'checkValidity',arguments:[],returns:{type:'boolean'}}],
    ['item',{name:'item',arguments:[{type:'number'}],returns:{type:'object'}}],
    ['namedItem',{name:'namedItem',arguments:[{type:'string'}],returns:{type:'object'}}],
    ['remove',{name:'remove',arguments:[{type:'number'}]}],
    ['reportValidity',{name:'reportValidity',arguments:[],returns:{type:'boolean'}}],
    ['setCustomValidity',{name:'setCustomValidity',arguments:[{type:'string'}]}]
]);

webInterfaces.set('htmlselectelement',
    {name: 'htmlselectelement',
        properties:htmlSelectElementInterfaceProperties,
        methods:htmlSelectElementMethodSpecs,
        reference: {name:'HTMLSelectElement',
            standard:'https://www.w3.org/TR/2017/REC-html52-20171214/sec-forms.html#htmlselectelement',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/HTMLSelectElement'
        }
    });

var htmlOptionElementInterfaceProperties = new Map( [
    ['defaultSelected', SimpleProperty('boolean', 'defaultSelected', true)],
    ['disabled', SimpleProperty('boolean', 'disabled', true)],
    ['form', SimpleProperty('object', 'form')],
    ['index', SimpleProperty('number', 'index')],
    ['label', SimpleProperty('string', 'label', true)],
    ['selected', SimpleProperty('boolean', 'selected', true)],
    ['text', SimpleProperty('string', 'text', true)],
    ['value', SimpleProperty('atom', 'value', true)],
]);

var htmlOptionElementMethodSpecs = new Map([
]);

webInterfaces.set('htmloptionelement',
    {name: 'htmloptionelement',
        properties:htmlOptionElementInterfaceProperties,
        methods:htmlOptionElementMethodSpecs,
        reference: {name:'HTMLOptionElement',
            standard:'https://www.w3.org/TR/2017/REC-html52-20171214/sec-forms.html#htmloptionelement',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/HTMLOptionElement'
        }
    });


var htmlFormElementInterfaceProperties = new Map( [
]);

var htmlFormElementMethodSpecs = new Map([
]);

webInterfaces.set('htmlformelement',
    {name: 'htmlformelement',
        properties:htmlFormElementInterfaceProperties,
        methods:htmlFormElementMethodSpecs,
        reference: {name:'HTMLFormElement',
            standard:'https://www.w3.org/TR/2017/REC-html52-20171214/sec-forms.html#htmlformelement',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/HTMLFormElement'
        }
    });

var eventInterfaceProperties = new Map( [
    ['bubbles', SimpleProperty('boolean', 'bubbles')],
    ['cancelable', SimpleProperty('boolean', 'cancelable')],
    ['cancelBubble', SimpleProperty('boolean', 'cancelBubble', true)],
    ['composed', SimpleProperty('boolean', 'composed')],
    ['currentTarget', SimpleProperty('object', 'currentTarget')],
    ['defaultPrevented', SimpleProperty('boolean', 'defaultPrevented')],
    ['eventPhase', SimpleProperty('number', 'eventPhase')],
    ['returnValue', SimpleProperty('boolean', 'returnValue', true)],
    ['target', SimpleProperty('object', 'target')],
    ['timeStamp', SimpleProperty('number', 'timeStamp')],
    ['type', SimpleProperty('string', 'type')],
    ['isTrusted', SimpleProperty('boolean', 'isTrusted')]
]);

var eventMethodSpecs = new Map([
    ['composedPath',{name:'composedPath',arguments:[],returns:{type:'object'}}],
    ['preventDefault',{name:'preventDefault',arguments:[]}],
    ['stopImmediatePropagation',{name:'stopImmediatePropagation',arguments:[]}],
    ['stopPropagation',{name:'stopPropagation',arguments:[]}],
    ['setProperty',{name:'setProperty',arguments:[{type:'string'},{type:'string'},{type:'string'}]}]
]);

webInterfaces.set('event',
    {name: 'event',
        properties:eventInterfaceProperties,
        methods:eventMethodSpecs,
        reference: {name:'Event',
            standard:'https://www.w3.org/TR/2018/WD-dom41-20180201/#interface-event',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/Event'
        }
    });

var cssStyleDeclarationInterfaceProperties = new Map( [
    ['cssText', SimpleProperty('string', 'cssText', true)], // documented as Attribute, but not listed as Property.
    ['length', SimpleProperty('number', 'length')],
    ['parentRule', SimpleProperty('object', 'parentRule')] // object has a CSSRule interface.
]);


var cssStyleDeclarationMethodSpecs = new Map([
    ['getPropertyPriority',{name:'getPropertyPriority',arguments:[{type:'string'}],returns:{type:'string'}}],
    ['getPropertyValue',{name:'getPropertyValue',arguments:[{type:'string'}],returns:{type:'string'}}],
    ['item',{name:'item',arguments:[{type:'integer'}],returns:{type:'atom'}}],
    ['removeProperty',{name:'removeProperty',arguments:[{type:'string'}],returns:{type:'string'}}],
    ['setProperty',{name:'setProperty',arguments:[{type:'string'},{type:'string'},{type:'string'}]}]
]);

webInterfaces.set('cssstyledeclaration',
    {name: 'cssstyledeclaration',
        properties:cssStyleDeclarationInterfaceProperties,
        methods:cssStyleDeclarationMethodSpecs,
        reference: {name:'CSSStyleDeclaration',
            standard:'https://www.w3.org/TR/cssom-1/#cssstyledeclaration',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/CSSStyleDeclaration'
        }
    });

var cssRuleInterfaceProperties = new Map( [
    ['cssText', SimpleProperty('string', 'cssText', true)],
    ['parentStyleSheet', SimpleProperty('object', 'parentStyleSheet')],
    ['parentRule', SimpleProperty('object', 'parentRule')], // documented as Attribute, but not listed as Property.
    ['type', SimpleProperty('number', 'type')], // documented as Attribute, but not listed as Property.
]);

var cssRuleMethodSpecs = new Map([]);

webInterfaces.set('cssrule',
    {name: 'cssrule',
        properties:cssRuleInterfaceProperties,
        methods:cssRuleMethodSpecs,
        reference: {name:'CSSRule',
            standard:'https://www.w3.org/TR/cssom-1/#cssrule',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/CSSRule'
        }
    });

var canvasRenderingContext2DInterfaceProperties = new Map( [
    ['canvas', SimpleProperty('object', 'canvas')],
    ['fillStyle', SimpleProperty(['atom','object'], 'fillStyle', true)], // fillStyle is a string naming a color, a CanvasGradient object, or a CanvasPattern object.
    ['font', SimpleProperty('atom', 'font', true)],
    ['globalAlpha', SimpleProperty('number', 'globalAlpha', true)],
    ['globalCompositeOperation', SimpleProperty('atom', 'globalCompositeOperation', true)],
    ['imageSmoothingEnabled', SimpleProperty('atom', 'imageSmoothingEnabled', true)],
    ['lineCap', SimpleProperty('atom', 'lineCap', true)],
    ['lineDashOffset', SimpleProperty('number', 'lineDashOffset', true)],
    ['lineJoin', SimpleProperty('atom', 'lineJoin', true)],
    ['lineWidth', SimpleProperty('number', 'lineWidth', true)],
    ['miterLimit', SimpleProperty('number', 'miterLimit', true)],
    ['shadowBlur', SimpleProperty('number', 'shadowBlur', true)],
    ['shadowColor', SimpleProperty('atom', 'shadowColor', true)],
    ['shadowOffsetX', SimpleProperty('number', 'shadowOffsetX', true)],
    ['shadowOffsetY', SimpleProperty('number', 'shadowOffsetY', true)],
    ['strokeStyle', SimpleProperty(['atom','object'], 'strokeStyle', true)], // strokeStyle is a string naming a color, a CanvasGradient object, or a CanvasPattern object.
    ['textAlign', SimpleProperty('atom', 'textAlign', true)],
    ['textBaseline', SimpleProperty('atom', 'textBaseline', true)]
]);

var canvasRenderingContext2DMethodSpecs = new Map([
    ['arc',{name:'arc',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'boolean'}]}],
    ['arcTo',{name:'arcTo',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['beginPath',{name:'beginPath',arguments:[]}],
    ['bezierCurveTo',{name:'bezierCurveTo',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['clearRect',{name:'clearRect',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['clip',{name:'clip',arguments:[{type:'string'}]}],
    ['clipPath',{name:'clip',arguments:[{type:'object'},{type:'string'}]}], // object is Path2D
    ['closePath',{name:'closePath',arguments:[]}],
    ['createImageData',{name:'createImageData',arguments:[{type:'number'},{type:'number'}],returns:{type:'object'}}],
    ['createImageDataCopy',{name:'createImageData',arguments:[{type:'object'}],returns:{type:'object'}}], // object is ImageData
    ['createLinearGradient',{name:'createLinearGradient',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'}],returns:{type:'object'}}],
    ['createPattern',{name:'createPattern',arguments:[{type:'object'},{type:'string'}],returns:{type:'object'}}], // input object is CanvasImageSource, returns CanvasPattern
    ['createRadialGradient',{name:'createRadialGradient',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'}],returns:{type:'object'}}], // returns CanvasGradient
    ['drawFocusIfNeeded',{name:'drawFocusIfNeeded',arguments:[{type:'object'}]}], // object is Element
    ['drawFocusIfNeededPath',{name:'drawFocusIfNeeded',arguments:[{type:'object'},{type:'object'}]}], // object is Path2D
    ['drawImage',{name:'drawImage',arguments:[{type:'object'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['ellipse',{name:'ellipse',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'boolean'}]}],
    ['fill',{name:'fill',arguments:[{type:'string'}]}],
    ['fillPath',{name:'fill',arguments:[{type:'object'},{type:'string'}]}], // object is Path2D
    ['fillRect',{name:'fillRect',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['fillText',{name:'fillText',arguments:[{type:'string'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['getImageData',{name:'getImageData',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'}],returns:{type:'object'}}],
    ['getLineDash',{name:'getLineDash',arguments:[],returns:{type:{arrayType:'number'}}}],
    ['isPointInPath',{name:'isPointInPath',arguments:[{type:'number'},{type:'number'},{type:'string'}],returns:{type:'boolean'}}],
    ['isPointInPathX',{name:'isPointInPath',arguments:[{type:'object'},{type:'number'},{type:'number'},{type:'string'}],returns:{type:'boolean'}}],
    ['isPointInStroke',{name:'isPointInStroke',arguments:[{type:'number'},{type:'number'}],returns:{type:'boolean'}}],
    ['isPointInStrokePath',{name:'isPointInStroke',arguments:[{type:'object'},{type:'number'},{type:'number'}],returns:{type:'boolean'}}],
    ['lineTo',{name:'lineTo',arguments:[{type:'number'},{type:'number'}]}],
    ['measureText',{name:'measureText',arguments:[{type:'string_codes'}],returns:{type:'object'}}], // returns TextMetrics
    ['moveTo',{name:'moveTo',arguments:[{type:'number'},{type:'number'}]}],
    ['putImageData',{name:'putImageData',arguments:[{type:'object'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['quadraticCurveTo',{name:'quadraticCurveTo',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['rect',{name:'rect',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['restore',{name:'restore',arguments:[]}],
    ['rotate',{name:'rotate',arguments:[{type:'number'}]}],
    ['save',{name:'save',arguments:[]}],
    ['scale',{name:'scale',arguments:[{type:'number'},{type:'number'}]}],
    ['setLineDash',{name:'setLineDash',arguments:[{type:{arrayType:'number'}}]}],
    ['setTransform',{name:'setTransform',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['stroke',{name:'stroke',arguments:[{type:'object'}]}],
    ['strokeRect',{name:'strokeRect',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['strokeText',{name:'strokeText',arguments:[{type:'string'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['transform',{name:'transform',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['translate',{name:'translate',arguments:[{type:'number'},{type:'number'}]}]
]);

webInterfaces.set('canvasrenderingcontext2d',
    {name: 'canvasrenderingcontext2d',
        properties:canvasRenderingContext2DInterfaceProperties,
        methods:canvasRenderingContext2DMethodSpecs,
        reference: {name:'CanvasRenderingContext2D',
            standard:'https://www.w3.org/TR/2dcontext/#canvasrenderingcontext2d',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/CanvasRenderingContext2D'
        }
    });

var blobInterfaceProperties = new Map( [
    ['size', SimpleProperty('number', 'size', true)],
    ['type', SimpleProperty('atom', 'type')],
]);

var blobMethodSpecs = new Map([
    ['slice',{name:'slice',arguments:[{type:'number'},{type:'number'},{type:'string'}],returns:{type:'object'}}]
]);

webInterfaces.set('blob',
    {name: 'blob',
        properties:blobInterfaceProperties,
        methods:blobMethodSpecs,
        reference: {name:'Blob',
            standard:'https://www.w3.org/TR/FileAPI/#dfn-Blob',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/Blob'
        }
    });

var imageDataInterfaceProperties = new Map( [
    ['data', SimpleProperty('object', 'data')]
]);

var imageDataMethodSpecs = new Map([
]);

webInterfaces.set('imagedata',
    {name: 'imagedata',
        properties:imageDataInterfaceProperties,
        methods:imageDataMethodSpecs,
        reference: {name:'ImageData',
            standard:'https://www.w3.org/TR/2dcontext/#imagedata',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/ImageData'
        }
    });

var uint8ClampedArrayInterfaceProperties = new Map( [
    ['length', SimpleProperty('integer', 'length')]
]);

var uint8ClampedArrayMethodSpecs = new Map([
    ['set', {name:'set',arguments:[{type:{arrayType:'integer'}},{type:'integer'}]}]
]);

webInterfaces.set('uint8clampedarray',
    {name: 'uint8clampedarray',
        properties:uint8ClampedArrayInterfaceProperties,
        methods:uint8ClampedArrayMethodSpecs,
        reference: {name:'Uint8ClampedArray',
            standard:'https://www.ecma-international.org/ecma-262/6.0/#table-49',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/Uint8ClampedArray'
        }
    });

var canvasGradientInterfaceProperties = new Map( [
]);

var canvasGradientMethodSpecs = new Map([
    ['addColorStop', {name:'addColorStop',arguments:[{type:'number'},{type:'string'}]}]
]);

webInterfaces.set('canvasgradient',
    {name: 'canvasgradient',
        properties:canvasGradientInterfaceProperties,
        methods:canvasGradientMethodSpecs,
        reference: {name:'CanvasGradient',
            standard:'https://www.w3.org/TR/2dcontext/#canvasrenderingcontext2d',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/CanvasGradient'
        }
    });

var canvasPatternInterfaceProperties = new Map( [
]);

var canvasPatternMethodSpecs = new Map([
]);

webInterfaces.set('canvaspattern',
    {name: 'canvaspattern',
        properties:canvasPatternInterfaceProperties,
        methods:canvasPatternMethodSpecs,
        reference: {name:'CanvasPattern',
            standard:'https://www.w3.org/TR/2dcontext/#canvasrenderingcontext2d',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/CanvasPattern'
        }
    });

var htmlImageElementInterfaceProperties = new Map( [
    ['src', SimpleProperty('string', 'src', true)]
]);

var htmlImageElementMethodSpecs = new Map([
]);

webInterfaces.set('htmlimageelement',
    {name: 'htmlimageelement',
        properties:htmlImageElementInterfaceProperties,
        methods:htmlImageElementMethodSpecs,
        reference: {name:'HTMLImageElement',
            standard:'https://www.w3.org/TR/2017/REC-html52-20171214/semantics-embedded-content.html#htmlimageelement',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/HTMLImageElement'
        }
    });

var path2DInterfaceProperties = new Map( [
]);

var path2DMethodSpecs = new Map([
    ['arc',{name:'arc',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'boolean'}]}],
    ['ellipse',{name:'ellipse',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'boolean'}]}]
]);

webInterfaces.set('path2d',
    {name: 'path2d',
        properties:path2DInterfaceProperties,
        methods:path2DMethodSpecs,
        reference: {name:'Path2D',
            standard:'https://html.spec.whatwg.org/multipage/canvas.html#dom-path2d',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/Path2D'
        }
    });

var uiEventInterfaceProperties = new Map( [
]);

var uiEventMethodSpecs = new Map([
]);

webInterfaces.set('uievent',
    {name: 'uievent',
        properties:uiEventInterfaceProperties,
        methods:uiEventMethodSpecs,
        reference: {name:'UIEvent',
            standard:'https://www.w3.org/TR/2019/WD-uievents-20190530/#idl-uievent',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/UIEvent'
        }
    });

var mouseEventInterfaceProperties = new Map( [
    ['clientX', SimpleProperty('number', 'clientX')],
    ['clientY', SimpleProperty('number', 'clientY')],
    ['pageX', SimpleProperty('number', 'pageX')],
    ['pageY', SimpleProperty('number', 'pageY')],
    ['altKey', SimpleProperty('boolean', 'altKey')],
    ['button', SimpleProperty('number', 'button')],
    ['buttons', SimpleProperty('number', 'buttons')],
    ['ctrlKey', SimpleProperty('boolean', 'ctrlKey')],
    ['metaKey', SimpleProperty('boolean', 'metaKey')],
    ['movementX', SimpleProperty('number', 'movementX')],
    ['movementY', SimpleProperty('number', 'movementY')],
    ['mozInputSource', SimpleProperty('atom', 'mozInputSource')],
    ['offsetX', SimpleProperty('number', 'offsetX')],
    ['offsetY', SimpleProperty('number', 'offsetY')],
    ['region', SimpleProperty('atom', 'region')],
    ['relatedTarget', SimpleProperty('object', 'relatedTarget')],
    ['screenX', SimpleProperty('number', 'screenX')],
    ['screenY', SimpleProperty('number', 'screenY')],
    ['shiftKey', SimpleProperty('boolean', 'shiftKey')]
]);

var mouseEventMethodSpecs = new Map([
    ['getModifierState',{name:'getModifierState',arguments:[{type:'string'}],returns:{type:'boolean'}}],
]);

webInterfaces.set('mouseevent',
    {name: 'mouseevent',
        properties:mouseEventInterfaceProperties,
        methods:mouseEventMethodSpecs,
        reference: {name:'MouseEvent',
            standard:'https://www.w3.org/TR/2019/WD-uievents-20190530/#idl-mouseevent',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/MouseEvent'
        }
    });

var textMetricsInterfaceProperties = new Map( [
    ['width', SimpleProperty('number', 'width')],
    ['actualBoundingBoxLeft', SimpleProperty('number', 'actualBoundingBoxLeft')],
    ['actualBoundingBoxRight', SimpleProperty('number', 'actualBoundingBoxRight')]

]);

var textMetricsMethodSpecs = new Map([
]);

webInterfaces.set('textmetrics',
    {name: 'textmetrics',
        properties:textMetricsInterfaceProperties,
        methods:textMetricsMethodSpecs,
        reference: {name:'TextMetrics',
            standard:'https://www.w3.org/TR/2dcontext/#textmetrics',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/TextMetrics'
        }
    });

var validityStateInterfaceProperties = new Map( [
    ['badInput', SimpleProperty('boolean', 'badInput')],
    ['customError', SimpleProperty('boolean', 'customError')],
    ['patternMismatch', SimpleProperty('boolean', 'patternMismatch')],
    ['rangeOverflow', SimpleProperty('boolean', 'rangeOverflow')],
    ['rangeUnderflow', SimpleProperty('boolean', 'rangeUnderflow')],
    ['stepMismatch', SimpleProperty('boolean', 'stepMismatch')],
    ['tooLong', SimpleProperty('boolean', 'tooLong')],
    ['tooShort', SimpleProperty('boolean', 'tooShort')],
    ['typeMismatch', SimpleProperty('boolean', 'typeMismatch')],
    ['valid', SimpleProperty('boolean', 'valid')],
    ['valueMissing', SimpleProperty('boolean', 'valueMissing')]
]);

var validityStateMethodSpecs = new Map([
]);

webInterfaces.set('validitystate',
    {name: 'validitystate',
        properties:validityStateInterfaceProperties,
        methods:validityStateMethodSpecs,
        reference: {name:'ValidityState',
            standard:'https://www.w3.org/TR/2017/REC-html52-20171214/sec-forms.html#validitystate',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/ValidityState'
        }
    });

var fileInterfaceProperties = new Map( [
    ['lastModified', SimpleProperty('number', 'lastModified')],
    ['name', SimpleProperty('atom', 'name')]

]);

var fileMethodSpecs = new Map([
]);

webInterfaces.set('file',
    {name: 'file',
        properties:fileInterfaceProperties,
        methods:fileMethodSpecs,
        reference: {name:'File',
            standard:'https://www.w3.org/TR/2019/WD-FileAPI-20190531/#dfn-file',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/File'
        }
    });

var elementCreationOptionsInterfaceProperties = new Map( [
    ['is', SimpleProperty('atom', 'is')]

]);

var elementCreationOptionsMethodSpecs = new Map([
]);

webInterfaces.set('elementcreationoptions',
    {name: 'elementcreationoptions',
        properties:elementCreationOptionsInterfaceProperties,
        methods:elementCreationOptionsMethodSpecs,
        reference: {name:'ElementCreationOptions',
            standard:'https://www.w3.org/TR/2018/WD-dom41-20180201/#dictdef-elementcreationoptions',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/Document/createElement'
        }
    });

var locationInterfaceProperties = new Map( [
    ['href', SimpleProperty('atom', 'href')],
    ['origin', SimpleProperty('atom', 'origin')],
    ['protocol', SimpleProperty('atom', 'protocol')],
    ['host', SimpleProperty('atom', 'host')],
    ['hostname', SimpleProperty('atom', 'hostname')],
    ['port', SimpleProperty('atom', 'port')],
    ['pathname', SimpleProperty('atom', 'pathname')],
    ['search', SimpleProperty('atom', 'search')],
    ['hash', SimpleProperty('atom', 'hash')]
]);

var locationMethodSpecs = new Map([
    ['assign', {name:'assign',arguments:[{type:'string'}]}],
    ['replace', {name:'replace',arguments:[{type:'string'}]}],
    ['reload', {name:'reload',arguments:[]}]
]);

webInterfaces.set('location',
    {name: 'location',
        properties:locationInterfaceProperties,
        methods:locationMethodSpecs,
        reference: {name:'Location',
            standard:'https://html.spec.whatwg.org/multipage/history.html#location',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/Window/location'
        }
    });

var historyInterfaceProperties = new Map( [
    ['length', SimpleProperty('number', 'length')],
    ['scrollRestoration', SimpleProperty('atom', 'scrollRestoration', true)],
    ['state', SimpleProperty('atom', 'state')]
]);

var historyMethodSpecs = new Map([
    ['go', {name:'go',arguments:[{type:'number'}]}],
    ['back', {name:'back',arguments:[]}],
    ['forward', {name:'forward',arguments:[]}],
    ['pushState', {name:'pushState',arguments:[{type:'string'},{type:'string'}, {type:'string'}]}],
    ['replaceState', {name:'replaceState',arguments:[{type:'string'},{type:'string'}, {type:'string'}]}]
]);

webInterfaces.set('history',
    {name: 'history',
        properties:historyInterfaceProperties,
        methods:historyMethodSpecs,
        reference: {name:'History',
            standard:'https://html.spec.whatwg.org/multipage/history.html#history',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/Window/history'
        }
    });

var customElementRegistryInterfaceProperties = new Map( [
]);

var customElementRegistryMethodSpecs = new Map([
    ['define', {name:'define',arguments:[{type:'string'},{type:'object'},{type:'object'}]}], //CustomElementConstructor, ElementDefinitionOptions
    ['get', {name:'get', arguments:[{type:'string'}],returns:{type:'object'}}], // returned object is a constructor function (with internal method [[Constructor]])
    ['upgrade', {name:'upgrade', arguments:[{type:'object'}]}],
    ['whenDefined', {name:'whenDefined', arguments:[{type:'string'}],returns:{type:'object'}}] // returns Promise object
]);

webInterfaces.set('customelementregistry',
    {name: 'customelementregistry',
        properties:customElementRegistryInterfaceProperties,
        methods:customElementRegistryMethodSpecs,
        reference: {name:'CustomElementRegistry',
            standard:'https://html.spec.whatwg.org/multipage/custom-elements.html#customelementregistry',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/CustomElementRegistry'
        }
    });

var barPropInterfaceProperties = new Map( [
    ['visible', SimpleProperty('boolean', 'visible')]

]);

var barPropMethodSpecs = new Map([
]);

webInterfaces.set('barprop',
    {name: 'barprop',
        properties:barPropInterfaceProperties,
        methods:barPropMethodSpecs,
        reference: {name:'BarProp',
            standard:'https://html.spec.whatwg.org/multipage/window-object.html#barprop',
            mdn:'none'
        }
    });

// Navigator properties and methods are defined entirely in
// mixins such as NavigatorID
var navigatorInterfaceProperties = new Map( [
]);

var navigatorMethodSpecs = new Map([
]);

webInterfaces.set('navigator',
    {name: 'navigator',
        properties:navigatorInterfaceProperties,
        methods:navigatorMethodSpecs,
        reference: {name:'Navigator',
            standard:'https://html.spec.whatwg.org/multipage/system-state.html#navigator',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/Navigator'
        }
    });

var navigatorIDInterfaceProperties = new Map( [
    ['userAgent', SimpleProperty('atom', 'userAgent')]
]);

var navigatorIDMethodSpecs = new Map([
]);

webInterfaces.set('navigatorid',
    {name: 'navigatorid',
        properties:navigatorIDInterfaceProperties,
        methods:navigatorIDMethodSpecs,
        reference: {name:'NavigatorID',
            standard:'https://html.spec.whatwg.org/multipage/system-state.html#navigatorid',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/NavigatorID'
        }
    });

var navigatorLanguageInterfaceProperties = new Map( [
    ['language', SimpleProperty('atom', 'language')],
    ['languages', SimpleProperty('atom', 'languages')] //  should be array(atom), or have SimpleProperty handle array objects.
]);

var navigatorLanguageMethodSpecs = new Map([
]);

webInterfaces.set('navigatorlanguage',
    {name: 'navigatorlanguage',
        properties:navigatorLanguageInterfaceProperties,
        methods:navigatorLanguageMethodSpecs,
        reference: {name:'NavigatorLanguage',
            standard:'https://html.spec.whatwg.org/multipage/system-state.html#navigatorlanguage',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/NavigatorLanguage'
        }
    });

var navigatorOnLineInterfaceProperties = new Map( [
    ['onLine', SimpleProperty('boolean', 'onLine')]
]);

var navigatorOnLineMethodSpecs = new Map([
]);

webInterfaces.set('navigatoronline',
    {name: 'navigatoronline',
        properties:navigatorOnLineInterfaceProperties,
        methods:navigatorOnLineMethodSpecs,
        reference: {name:'NavigatorOnLine',
            standard:'https://html.spec.whatwg.org/multipage/system-state.html#navigatoronline',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/NavigatorOnLine'
        }
    });

var navigatorContentUtilsInterfaceProperties = new Map( [
]);

var navigatorContentUtilsMethodSpecs = new Map([
    ['registerProtocolHandler', {name:'registerProtocolHandler',arguments:[{type:'string'},{type:'string'},{type:'string'}]}],
    ['unregisterProtocolHandler', {name:'unregisterProtocolHandler',arguments:[{type:'string'},{type:'string'}]}]
]);

webInterfaces.set('navigatorcontentutils',
    {name: 'navigatorcontentutils',
        properties:navigatorContentUtilsInterfaceProperties,
        methods:navigatorContentUtilsMethodSpecs,
        reference: {name:'NavigatorContentUtils',
            standard:'https://html.spec.whatwg.org/multipage/system-state.html#NavigatorContentUtils',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/NavigatorContentUtils'
        }
    });

var navigatorCookiesInterfaceProperties = new Map( [
    ['cookieEnabled', SimpleProperty('boolean', 'cookieEnabled')]
]);

var navigatorCookiesMethodSpecs = new Map([
]);

webInterfaces.set('navigatorcookies',
    {name: 'navigatorcookies',
        properties:navigatorCookiesInterfaceProperties,
        methods:navigatorCookiesMethodSpecs,
        reference: {name:'NavigatorCookies',
            standard:'https://html.spec.whatwg.org/multipage/system-state.html#navigatorcookies',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/NavigatorCookies'
        }
    });

var storageInterfaceProperties = new Map( [
    ['length', SimpleProperty('number', 'length')],
]);

var storageMethodSpecs = new Map([
    ['key', {name:'key',arguments:[{type:'number'}],returns:{type:'atom'}}],
    ['getItem', {name:'getItem',arguments:[{type:'string'}],returns:{type:'atom'}}],
    ['setItem', {name:'setItem',arguments:[{type:'string'}, {type:'string'}]}],
    ['removeItem', {name:'removeItem',arguments:[{type:'string'}]}],
    ['clear', {name:'clear',arguments:[]}]
]);

webInterfaces.set('storage',
    {name: 'storage',
        properties:storageInterfaceProperties,
        methods:storageMethodSpecs,
        reference: {name:'Storage',
            standard:'https://html.spec.whatwg.org/multipage/webstorage.html#storage-2',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/Storage'
        }
    });

var windowLocalStorageInterfaceProperties = new Map( [
    ['localStorage', SimpleProperty('object', 'localStorage')],
]);

var windowLocalStorageMethodSpecs = new Map([

]);

webInterfaces.set('windowlocalstorage',
    {name: 'windowlocalstorage',
        properties:windowLocalStorageInterfaceProperties,
        methods:windowLocalStorageMethodSpecs,
        reference: {name:'WindowLocalStorage',
            standard:'https://html.spec.whatwg.org/multipage/webstorage.html#windowlocalstorage',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API/Local_storage'
        }
    });

var windowSessionStorageInterfaceProperties = new Map( [
    ['sessionStorage', SimpleProperty('object', 'sessionStorage')],
]);

var windowSessionStorageMethodSpecs = new Map([

]);

webInterfaces.set('windowsessionstorage',
    {name: 'windowsessionstorage',
        properties:windowSessionStorageInterfaceProperties,
        methods:windowSessionStorageMethodSpecs,
        reference: {name:'WindowSessionStorage',
            standard:'https://html.spec.whatwg.org/multipage/webstorage.html#windowsessionstorage',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/Web_Storage_API/Using_the_Web_Storage_API'
        }
    });


var windowOrWorkerGlobalScopeInterfaceProperties = new Map( [
    ['caches', SimpleProperty('object', 'caches')], // CacheStorage
    ['crossOriginIsolated', SimpleProperty('boolean', 'crossOriginIsolated')],
    ['indexedDB', SimpleProperty('object', 'indexedDB')], // IDBFactory
    ['isSecureContext', SimpleProperty('boolean', 'isSecureContext')],
    ['origin', SimpleProperty('atom', 'origin')]
]);

var windowOrWorkerGlobalScopeMethodSpecs = new Map([
    ['btoa', {name:'btoa',arguments:[{type:'string'}],returns:{type:'string'}}],
    ['atob', {name:'atob',arguments:[{type:'string'}],returns:{type:'string'}}],
    ['clearInterval', {name: 'clearInterval',arguments:[{type:'number'}]}],
    ['fetch', {name: 'fetch',arguments:[{type:['string','object']},{type:'object'}],returns:{type:'object'}}], // options object is HTMLOptionsCollection?, returns is Promise
    ['queueMicrotask', {name: 'queueMicrotask',arguments:[{type:'goal_function'}]}],
    ['setInterval', {name: 'setInterval',arguments:[{type:'goal_function'},{type:'number'}],returns:{type:'number'}}],
    ['setTimeout', {name: 'setTimeout',arguments:[{type:'goal_function'},{type:'number'}],returns:{type:'number'}}]
]);

webInterfaces.set('windoworworkerglobalscope',
    {name: 'windoworworkerglobalscope',
        properties:windowOrWorkerGlobalScopeInterfaceProperties,
        methods:windowOrWorkerGlobalScopeMethodSpecs,
        reference: {name:'windowOrWorkerGlobalScope',
            standard:'https://html.spec.whatwg.org/multipage/webappapis.html#windoworworkerglobalscope-mixin',
            mdn:'https://developer.mozilla.org/en-US/docs/Web/API/WindowOrWorkerGlobalScope'
        }
    });

// File object_property.js
"use strict";

var dopCursors = new Map();
var dopCursorCounter = 0;

// dom_object_property(Type, Object, Property, Value) binds Value to all values of Property of Object of Type.
// Property must be a ground value (i.e. not TAG(property) === TAG_REF)
// At least two of Type, Object, and Value must be ground.
function predicate_dom_object_property(type, object, property, value) {
    var cursor;
    var cursorIDPL;
    var cursorIDJS;

    if (state.foreign_retry) {
        cursorIDPL = state.foreign_value;
        cursorIDJS = atable[VAL(cursorIDPL)];
        cursor = dopCursors.get(cursorIDJS);

    }
    else {
        let container = {};
        if(!setupObjectsForPropertyValue(type, object, property, value, container)) {
            return false;
        }
        cursor = {
            objects: container.value,
            property_values: setupPropertyValues(type, object, property, value)
        };
        cursorIDJS = 'crs' + dopCursorCounter++;
        dopCursors.set(cursorIDJS, cursor);
        cursorIDPL = lookup_atom(cursorIDJS);

        create_choicepoint();
    }

    update_choicepoint_data(cursorIDPL);

    if (cursor.objects && cursor.objects.length > 0) {
        var elementJS = cursor.objects[0].value;
        var typeJS = cursor.objects[0].type;
        var elementPL = create_object_structure(elementJS, typeJS);

        if (!cursor.property_values) {
            cursor.property_values = setupPropertyValuesFromJSElement(typeJS, elementJS, property, value);
        }

        if (cursor.property_values && cursor.property_values.length > 0) {
            var valueJS = cursor.property_values.pop();
            let container = {};
            if(propertyValueToPL(typeJS, property, valueJS, container)) {
                return unify(value, container.value) &&
                    unify(object, elementPL);
            } else {
                return false;
            }
         }

        // All classNames for current elementJS have been processed.
        // Set the cursor.tags to undefined to force recalculation of tags
        // with next element.
        // Move to the next element by removing objects[0].

        cursor.property_values = undefined;
        cursor.objects = cursor.objects.slice(1);
        return false; // go to next choice (of element)
    } else {
        destroy_choicepoint();
        return false;
    }
}


function setupObjectsForPropertyValue(type, object, property, value, container) {
    if (TAG(property) === TAG_REF) {
        return instantiation_error(property);
    } else {
        var propertyJS = atable[VAL(property)];
    }

    if (TAG(object) !== TAG_REF) {
        if (TAG(object) !== TAG_STR) {
            return instantiation_error(object);
        }
        var objectContainer = {};
        if (!get_object_container(object, objectContainer)) {
            container.value = undefined;
        } else {
            var objectContainers = [];
            if(TAG(type) === TAG_LST) {
                // override the 'type' already assigned to objectContainer.type, if any,
                // with the type specification (e.g. SimpleProperty('foo', 'number', true)).
                let typeContainer = {};
                if (convert_property_spec(type, typeContainer)) {
                    objectContainer.type = typeContainer.value;
                } else {
                    return false;
                }
            }
            objectContainers.push(objectContainer);
            container.value = objectContainers;
        }
     } else if (TAG(value) !== TAG_REF && TAG(type) !== TAG_REF) {
        let typeContainer = {};
        if(convert_property_spec(type, typeContainer)) {
            let typeJS = typeContainer.value;
            return setupObjectsForBoundPropertyValue(typeJS, propertyJS, value, container);
        } else {
            return false;
        }
    } else if (TAG(type) === TAG_ATM && atable[VAL(type)] === 'element') {
        container.value = objectsToContainers(document.querySelectorAll('*'), 'element');
    } else {
        container.value = undefined;
    }

    return true;
}

function objectsToContainers(objects, typeJS) {
    let objectContainers = [];

    for(let object of objects) {
        objectContainers.push({value:object, type:typeJS});
    }
    return objectContainers;
}

function setupPropertyValuesFromJSElement(typeJS, objectJS, property, value) {
    if (TAG(property) === TAG_REF) {
        instantiation_error(property);
    } else {
        var propertyJS = atable[VAL(property)];
    }
    var values;
    if (TAG(value) !== TAG_REF ) {
        values = setupPropertyValuesFromBoundValue(typeJS, property, propertyJS, value);
    } else {
        values = setupPropertyValuesFromJSElement1(typeJS, objectJS, property, propertyJS);
    }
    return values;
}

function setupPropertyValues(type, object, property, value) {
    let typeJS;
    if(TAG(type) !== TAG_REF) {
        let typeContainer = {};
        if (convert_property_spec(type, typeContainer)) {
            typeJS = typeContainer.value;
        }
    }

    if (TAG(property) === TAG_REF) {
        instantiation_error(property);
    } else {
        var propertyJS = atable[VAL(property)];
    }
    var values;
    if (TAG(value) !== TAG_REF && typeJS) {
        values = setupPropertyValuesFromBoundValue(typeJS, property, propertyJS, value);
    } else if (TAG(object) !== TAG_REF) {
        if (TAG(object) !== TAG_STR) {
            instantiation_error(object);
        }
        var objectContainer = {};
        if (!get_object_container(object, objectContainer)) {
            return undefined;
        }
        let objectJS = objectContainer.value;
        let objectTypeJS = objectContainer.type;

        if(! typeJS) {
            typeJS = objectTypeJS;
        } else if(typeof typeJS === 'string' && typeJS !== objectTypeJS) {
            throw 'incompatible object types. Param type is ' + typeJS + ' and object ' + JSON.stringify(objectJS) + ' is tagged with type ' + objectTypeJS;
        }
        values = setupPropertyValuesFromJSElement1(typeJS, objectJS, property, propertyJS);
    } else {
        values = undefined;
    }
    return values;
}

function propertyValueToJS(type, value, container, reportError) {
    if(! container) {
        let valueJS;
        let container = {};
        if (propertyValueToJS(type, value, container, false)) {
            valueJS = container.value;
        } else {
            let formatted = format_term(value, {quoted: true});
            throw 'unable to convert Prolog value "' + formatted + '" to Javascript value of type "' + type + '".';
        }
        return valueJS;
    }

    if(typeof type === 'object') {
        // union of types
        for(let subtype of type) {
            if(propertyValueToJS(subtype, value, container, false)) {
                return true;
            }
        }

        return reportError && type_error('union: ' + type, value);
    } else if(type === 'atom') {
        return getAtomPropertyValue(value, container, reportError);
    } else if(type === 'boolean') {
        return getBooleanPropertyValue(value, container, reportError);
    } else if(type === 'number') {
        return getNumberPropertyValue(value, container, reportError);
    } else if(type === 'string') {
        return getStringPropertyValue(value, container, reportError);
    } else if(type === 'object') {
        return getObjectPropertyValue(value, container, reportError);
    } else {
        return reportError && domain_error(type);
    }
}

function getAtomPropertyValue(value, container, reportError) {
    if(TAG(value) !== TAG_ATM) {
        return reportError && type_error('atom', value);
    }
    container.value = atable[VAL(value)];
    return true;
}

function getBooleanPropertyValue(value, container, reportError) {
    if(TAG(value) !== TAG_ATM) {
        return reportError && type_error('atom', value);
    }

    var valueJS = atable[VAL(value)];

    if(valueJS !== 'true' && valueJS !== 'false') {
        return reportError && domain_error('boolean atom', value);
    }
    container.value = (valueJS === 'true');
    return true;
}

/**
 * Convert a prolog list of atoms to a space separated string of tokens.
 * @param value
 * @returns {string}
 */
function getClassListPropertyValue(value) {
    if(TAG(value) !== TAG_LST) {
        instantiation_error(value);
    }

    var string = '';
    var list = value;
    while(list !== NIL) {
        if(TAG(list) !== TAG_LST) {
            instantiation_error(list);
        }

        var atomPL = deref(memory[VAL(list)]);
        if(TAG(atomPL) !== TAG_ATM) {
            instantiation_error(atomPL);
        } else {
            if(string !== '') {
                string += ' ';
            }
            string += atable[VAL(value)];
            list = deref(memory[VAL(list) + 1]);
        }
    }

    return string;
}

function getIntegerPropertyValue(value, container, reportError) {
    if(TAG(value) !== TAG_INT) {
        return reportError && type_error('integer', value);
    }

    container.value = PL_get_integer(value);
    return true;
}

function getFloatPropertyValue(value, container, reportError) {
    if(TAG(value) !== TAG_FLT) {
        return reportError && type_error('float', value);
    }

    container.value = floats[VAL(value)];
    return true;
}

function getNumberPropertyValue(value, container, reportError) {

    if(getIntegerPropertyValue(value, container, false)) {
        return true;
    } else if( getFloatPropertyValue(value, container, false)) {
        return true;
    }

    return reportError && type_error('number', value);
}

function getStringPropertyValue(value, container, reportError) {
     return codes_to_string(value, container, reportError);
}

function getObjectPropertyValue(value, container) {
    return get_object_container(value, container);
}

function propertyValueToPL(typeJS, property, valueJS, container, reportError) {
    let propertyJS = PL_get_atom_chars(property);
    let propertySpec = getPropertySpecification(typeJS, propertyJS);
    if (propertySpec) {
        return propertyValueToPLUtil(propertySpec.type, property, valueJS, container, reportError);
    } else {
        return reportError && domain_error(property);
    }
}

function propertyValueToPLUtil(typeJS, property, valueJS, container, reportError) {
    let resultPL;
    if (typeof typeJS === 'object') {
        for (let subtype of typeJS) {
            if (propertyValueToPLUtil(subtype, property, valueJS, container, false)) {
                return true;
            }
        }

        return reportError && type_error(typeJS, lookup_atom(valueJS));
    } else if (typeJS === 'atom') {
        return getAtomPLPropertyValue(valueJS, container, reportError);
    } else if (typeJS === 'boolean') {
        if(typeof valueJS === 'boolean') {
            resultPL = lookup_atom(valueJS.toString());
        } else {
            return reportError && type_error(typeJS, lookup_atom(valueJS));
        }
    } else if (typeJS === 'number') {
        return getNumberPLPropertyValue(valueJS, container, reportError);
    } else if (typeJS === 'integer') {
        return getIntegerPLPropertyValue(valueJS, container, reportError);
    } else if (typeJS === 'float') {
        return getFloatPLPropertyValue(valueJS, container, reportError);
    } else if (typeJS === 'string') {
        resultPL = getStringPLPropertyValue(valueJS);
    } else if (typeJS === 'object') {
        resultPL = getObjectPLPropertyValue(valueJS);
    } else {
        return reportError && domain_error('data type', lookup_atom(typeJS));
    }

    container.value = resultPL;
    return true;
}

function getAtomPLPropertyValue(valueJS, container, reportError) {
    let localValue = valueJS;
    if(typeof localValue === 'number') {
        localValue = localValue.toString();
    } else if(typeof localValue === 'boolean') {
        localValue = localValue.toString();
    } else if(typeof localValue !== 'string') {
        return reportError && type_error('string', lookup_atom(JSON.stringify(valueJS)));
    }

    container.value = lookup_atom(localValue);
    return true;
}

// A number can be either an integer or a float. Javascript is agnostic.
// Prolog represents these differently.

function getNumberPLPropertyValue(valueJS, container, reportError) {
    if(getIntegerPLPropertyValue(valueJS, container, false)
        || getFloatPLPropertyValue(valueJS, container, false)) {
        return true;
    } else {
        return reportError && type_error('number', lookup_atom(JSON.stringify(valueJS)));
    }
}

function getIntegerPLPropertyValue(valueJS, container, reportError) {
    if(! container) {
        let localContainer = {};
        if( getIntegerPLPropertyValue(valueJS, localContainer, false)) {
            return localContainer.value;
        } else {
            throw 'Invalid integer ' + JSON.stringify(valueJS) + '.';
        }
    }
    else if(isInteger(valueJS)) {
        container.value = (valueJS & ((1 << WORD_BITS) - 1)) ^ (TAG_INT << WORD_BITS); // or ((1 << (WORD_BITS-1))-1)  from predicate_get_code (and others) in stream.js?
        return true;
    } else {
        return reportError && type_error('integer', lookup_atom(JSON.stringify(valueJS)));
    }
}

function getFloatPLPropertyValue(valueJS, container, reportError) {
    if(! container) {
        let localContainer = {};
        if( getFloatPLPropertyValue(valueJS, localContainer, false)) {
            return localContainer.value;
        } else {
            throw 'Invalid float ' + JSON.stringify(valueJS) + '.';
        }
    }
    else if(isFloat(valueJS)) {
        container.value = lookup_float(valueJS);
        return true;
    } else {
        return reportError && type_error('float', lookup_atom(JSON.stringify(valueJS)));
    }
}

// maybe this should just be Number.isInteger
function isInteger(value) {
    if(typeof(value) === 'number') {
        return value === ~~value;
    }
}

// there does not appear to be direct Number.isFloat.
//
function isFloat(value) {
    if(typeof(value) === 'number') {
        return value !== ~~value;
    }
}

function getStringPLPropertyValue(valueJS) {
    return string_to_codes(valueJS);
}

function getObjectPLPropertyValue(valueJS) {
    return create_object_structure(valueJS);
}

// typeJS may be a propertySpec. In this case it short-circuits the lookup.
function setupObjectsForBoundPropertyValue(typeJS, propertyJS, value, container) {
    let propertySpec = getPropertySpecification(typeJS, propertyJS);
    if (propertySpec) {
        let valueJS = propertyValueToJS(propertySpec.type, value);
        container.value = objectsToContainers(propertySpec.objects(valueJS), typeJS);
        return true;
    } else {
        return domain_error(propertyJS);
    }
}

function setupPropertyValuesFromBoundValue(typeJS, property, propertyJS, value) {
    let propertySpec = getPropertySpecification(typeJS, propertyJS);
    if (propertySpec) {
        let values = [];
        let valueJS = propertyValueToJS(propertySpec.type, value);
        values.push(valueJS);
        return values;
    } else {
        domain_error(property);
    }
}

function setupPropertyValuesFromJSElement1(typeJS, elementJS, property, propertyJS) {
    let propertySpec = getPropertySpecification(typeJS, propertyJS);
    if (propertySpec) {
        return propertySpec.elementValuesFunction(elementJS);
    } else {
        domain_error(property);
    }
}

// Walk up the interface type hierarchy until propertyJS is
// found. Use a breadth-first search of parents in case
// there are multiple parents for a particular interface.

function getPropertySpecification(typeJS, propertyJS) {
    return (typeof typeJS === 'object') ? typeJS : getInterfaceItemSpec(typeJS, 'property', propertyJS);
}

function predicate_set_dom_object_property(object, property, value, specTerm) {
    if (TAG(object) !== TAG_STR) {
        instantiation_error(object);
    }

    let objectContainer = {};
    if (!get_object_container(object, objectContainer)) {
        return false;
    }
    let elementJS = objectContainer.value;
    let typeJS = objectContainer.type;

    if(specTerm) {
        let specContainer = {};
        if(!convert_property_spec(specTerm, specContainer)) {
            return false;
        }
        typeJS = specContainer.value;
    }

    if (TAG(property) === TAG_REF) {
        instantiation_error(property);
    } else {
        var propertyJS = atable[VAL(property)];
    }

    let propertySpec = getPropertySpecification(typeJS, propertyJS);
    if (propertySpec) {
        propertySpec.setValue(property, elementJS, value);
    } else {
        domain_error(property);
    }
    return true;
}
// File object_method.js
"use strict";

/*
The general approach to providing access to Javascript objects
through Prolog predicates is described in object_property.js.

This file implements access to Javascript object methods.


 */

// The method argument may be qualified with a module name or
// an argument of the method structure may be qualified.
// object_method(Element, add_event_listener(click, 'foo:bar'(thing))).
// or
// object_method(Element, foo : add_event_listener(click, bar(thing))).
// In the second case, the 'foo' module name is used in conjunction with
// the meta-argument type definition for 'add_event_listener' second argument
// (of '0') to determine
// that 'bar(thing)' should be qualified as 'foo:bar'(thing).
// If the foo module had imported bar/1 from the quux module, then this
// qualification would be 'quux:bar'(thing).
//
// method:EventAgent.addEventListener, arguments:[string, goal_function]
// object_method_no_return(objectJS, EventAgent.addEventListener, [eventJS, handlerFunction]);
var domCursors = new Map();
var domCursorCounter = 0;

function predicate_dom_object_method(object, methodStructure, specTerm) {
    var cursor;
    var cursorIDPL;
    var cursorIDJS;
    if (state.foreign_retry) {
        cursorIDPL = state.foreign_value;
        cursorIDJS = atable[VAL(cursorIDPL)];
        cursor = domCursors.get(cursorIDJS);

    }
    else {
        if (TAG(object) !== TAG_STR) {
            return instantiation_error(object);
        }

        if (TAG(methodStructure) !== TAG_STR && TAG(methodStructure) !== TAG_ATM) {
            return instantiation_error(methodStructure);
        }

        if (specTerm && TAG(specTerm) !== TAG_LST) {
            return instantiation_error(specTerm);
        }

        var objectContainer = {};
        if (!get_object_container(object, objectContainer)) {
            return false;
        }
        let objectType = objectContainer.type;
        var objectJS = objectContainer.value;

        let methodName;
        let arity;
        let methodAddress;
        let moduleName;
        if (TAG(methodStructure) === TAG_ATM) {
            methodName = atable[VAL(methodStructure)];
            arity = 0;
            moduleName = 'user';
        } else  {
            let functor = atable[ftable[VAL(memory[VAL(methodStructure)])][0]];
            let localArity = ftable[VAL(memory[VAL(methodStructure)])][1];
            if (functor === ':' && localArity === 2) {
                // ModuleName : MethodStructure
                moduleName = atable[VAL(deref(memory[VAL(methodStructure) + 1]))];
                let subStructure = deref(memory[VAL(methodStructure) + 2]);
                if(TAG(subStructure) === TAG_ATM) {
                    methodName = atable[VAL(subStructure)];
                    arity = 0;
                } else {
                    methodAddress = VAL(subStructure);
                    methodName = atable[ftable[VAL(memory[methodAddress])][0]];
                    arity = ftable[VAL(memory[methodAddress])][1];
                }
            } else {
                moduleName = 'user';
                methodAddress = VAL(methodStructure);
                methodName = atable[ftable[VAL(memory[methodAddress])][0]];
                arity = ftable[VAL(memory[methodAddress])][1];
            }
        }

        let spec;
        if (specTerm) {
            let specContainer = {};
            if (!convert_method_spec(specTerm, specContainer)) {
                return false;
            }
            spec = specContainer.value;
        } else {
            spec = getInterfaceItemSpec(objectType, 'method', methodName);
        }

        if (!spec) {
            return domain_error('method for ' + objectType, lookup_atom(methodName));
        }
        if (spec.returns && spec.returns.type !== 'boolean') {
            arity--; // the last argument to the methodStructure is for the return value.
        }

        let specArguments = spec.arguments;
        let applyArguments = [];
        for (var i = 0; i < arity; i++) {
            let specArgument = specArguments[i];
            let applyArgumentContainer = {};
            if (convert_method_argument(deref(memory[methodAddress + i + 1]), specArgument, moduleName, applyArgumentContainer)) {
                applyArguments.push(applyArgumentContainer.value);
            } else {
                return false;
            }
        }

        if (spec.returns) {
            let resultJS = object_method_return(objectJS, spec.name, applyArguments);
            if(typeof resultJS === 'undefined' || resultJS === null) {
                return false;
            } else  if(spec.returns.multiple) {
                let values = [];

                if (typeof resultJS === 'object' && resultJS.constructor.name === 'NodeList') {
                    values = Array.from(resultJS);
                } else if (typeof resultJS === 'object' && resultJS.constructor.name === 'FileList') {
                    values = Array.from(resultJS);
                } else if (typeof resultJS === 'object' && resultJS.constructor.name === 'HTMLOptionsCollection') {
                    values = Array.from(resultJS);
                } else if (typeof resultJS === 'object' && resultJS.constructor.name === 'HTMLCollection') {
                    values = Array.from(resultJS);
                } else {
                    values.push(resultJS);
                }

                cursor = {values: values, spec: spec, module: moduleName, methodAddress: methodAddress, arity: arity};
                cursorIDJS = 'crs' + domCursorCounter++;
                domCursors.set(cursorIDJS, cursor);
                cursorIDPL = lookup_atom(cursorIDJS);

                create_choicepoint();
            } else {
                let resultContainer = {};
                if (convert_result(resultJS, spec.returns, moduleName, resultContainer)) {
                    let resultPL = resultContainer.value;
                    if (spec.returns.type === 'boolean') {
                        return resultPL;
                    } else {
                        return unify(resultPL, deref(memory[methodAddress + arity + 1]));
                    }
                } else {
                    return false;
                }
            }
        } else {
            object_method_no_return(objectJS, spec.name, applyArguments);
            return true;
        }
    }

    update_choicepoint_data(cursorIDPL);

    if (cursor.values.length > 0) {
        let resultJS = cursor.values.pop();
        let resultContainer = {};
        if (convert_result(resultJS, cursor.spec.returns, cursor.module, resultContainer)) {
            let resultPL = resultContainer.value;
            if (cursor.spec.returns.type === 'boolean') {
                return resultPL;
            } else {
                return unify(resultPL, deref(memory[cursor.methodAddress + cursor.arity + 1]));
            }
        } else {
            return false;
        }
    }

    destroy_choicepoint();
    return false;
}

function convert_method_argument(term, spec, module, resultContainer, reportError) {
    if(TAG(term) === TAG_REF) {
        return instantiation_error(term);
    }

    let arg;
    if(typeof spec.type === 'object') {
        if(spec.type.arrayType) {
            // [X1, X2, ...] for array of arrayType items
            if(TAG(term) === TAG_LST) {
                let arrayContainer = {};
                if(terms_to_array(term, spec.type.arrayType, module, arrayContainer, reportError)) {
                    arg = arrayContainer.value;
                } else {
                    return false;
                }
            } else {
                return reportError && type_error('list for array of ' + JSON.stringify(spec.type.arrayType), term);
            }
        } else {
            // union of multiple types
            for (let subtype of spec.type) {
                if (convert_method_argument(term, {type: subtype}, module, resultContainer, false)) {
                    return true;
                }
            }
            return type_error('union: ' + spec.type, term);
        }
    } else if(spec.type === 'string') {
        if (TAG(term) === TAG_ATM) {
            arg = PL_atom_chars(term);
        } else {
            arg = format_term(term, {quoted:true});
        }
    } else if(spec.type === 'string_codes') {
        let container = {};
        if (!codes_to_string(term, container, reportError)) {
            return false;
        }
        arg = container.value;
    } else if(spec.type === 'integer') {
        if(TAG(term) === TAG_STR){
            let evaluationContainer = {};
            if(!evaluate_expression(term, evaluationContainer)) {
                return false;
            }
            arg = evaluationContainer.value;
        } else {
            let container = {};
            if (getIntegerPropertyValue(term, container, reportError)) {
                arg = container.value;
            } else {
                return false;
            }
        }
    } else if(spec.type === 'number') {
        if(TAG(term) === TAG_STR){
            let evaluationContainer = {};
            if(!evaluate_expression(term, evaluationContainer)) {
                return false;
            }
            arg = evaluationContainer.value;
        } else {
            let container = {};
            if (getNumberPropertyValue(term, container, reportError)) {
                arg = container.value;
            } else {
                return false;
            }
        }
    } else if(spec.type === 'boolean') {
        if (TAG(term) === TAG_ATM) {
            let value = PL_atom_chars(term);
            if(value === 'true') {
                arg = true;
            } else if(value === 'false') {
                arg = false
            } else {
                return reportError && domain_error('boolean', term);
            }
        } else {
            return reportError && type_error('atom', term);
        }
    } else if(spec.type === 'position') {
        if (TAG(term) === TAG_ATM) {
            arg = PL_atom_chars(term);
            if(["afterbegin", "afterend", "beforebegin", "beforeend"].indexOf(arg.toLowerCase()) === -1) {
                return reportError && domain_error("not_valid_insert_adjacent_mode", term);
            }
        } else  {
            return reportError && type_error('atom', term);
        }
    } else if(spec.type === 'goal_function') {
        // the goal may be specified with one or more arguments using '^': Type-X ^ foo(X)
        let goal;
        if (TAG(term) === TAG_ATM) {
            goal = PL_atom_chars(term);
        } else if (TAG(term) === TAG_STR) {
            // create a 'goal' string with canonical naming of variables.
            let copyTerm = copy_term(term);
            if(! predicate_numbervars(copyTerm, PL_put_integer(1), alloc_var())) {
                return false;
            }
            goal = format_term(copyTerm, {quoted: true, numbervars: true});
        } else {
            return reportError && type_error('atom or structure', term);
        }

        arg = goalFunctions.get(goal);
        if (!arg) {
            arg = function () {
                proscriptls_apply(arguments, module, goal);
            };

            goalFunctions.set(goal, arg);
        }
    } else if(spec.type === 'event'){
        let eventName;
        if (TAG(term) === TAG_ATM) {
            eventName = PL_atom_chars(term);
        } else {
            return reportError && type_error('atom', term);
        }
        arg = new Event(eventName);
    } else if(spec.type === 'object'){
        if (TAG(term) === TAG_STR) {
            var objectContainer = {};
            if (!get_object_container(term, objectContainer)) {
                return existence_error('object', term);
            }
            arg = objectContainer.value;
        } else {
            return reportError && type_error('object', term);
        }
    } else if(spec.type === 'options') {
        // [key-value|_]
        if(TAG(term) === TAG_LST) {
            let optionsContainer = {};
            if(terms_to_options(term, optionsContainer)) {
                arg = optionsContainer.value;
            } else {
                return reportError && type_error('option list', term);
            }
        } else {
            return reportError && type_error('options', term);
        }
    } else {
        throw 'internal error: spec.type not recognized. ' + spec.type;
    }

    resultContainer.value = arg;
    return true;
}

function terms_to_options(listRoot, optionsContainer) {
    var options = {};

    var list = listRoot;

    while(list !== NIL) {
        if(TAG(list) !== TAG_LST) {
            return instantiation_error(list);
        }

        var keyValuePairPL = deref(memory[VAL(list)]);
        if(TAG(keyValuePairPL) !== TAG_STR) {
            return instantiation_error(codePL);
        } else {
            // key-value
            let functor = atable[ftable[VAL(memory[VAL(keyValuePairPL)])][0]];
            let arity = ftable[VAL(memory[VAL(keyValuePairPL)])][1];
            if(functor !== '-') {
                return type_error('key - value: functor should be "-".', functor);
            }

            if(arity !== 2) {
                return type_error('key - value: term should have two arguments.', arity);
            }

            let keyPL = deref(memory[VAL(keyValuePairPL) + 1]);
            if(TAG(keyPL) !== TAG_ATM) {
                return type_error('key - value: key should be an atom.', keyPL);
            }
            let keyJS = atable[VAL(keyPL)];

            // TODO: extend value to allow any JSON-ish type - atom, number, boolean, list/JSON array, or keyValue list/JSON object.
            let valuePL = deref(memory[VAL(keyValuePairPL) + 2]);
            if(TAG(valuePL) !== TAG_ATM) {
                return type_error('key - value: value should be an atom.', valuePL);
            }

            options[keyJS] = atable[VAL(valuePL)];

            list = deref(memory[VAL(list) + 1]);
        }
    }

    optionsContainer.value = options;
    return true;
}

function terms_to_array(listRoot, itemType, module, arrayContainer, reportError) {
    var array = [];

    var list = listRoot;

    while(list !== NIL) {
        if(TAG(list) !== TAG_LST) {
            return instantiation_error(list);
        }

        var itemPL = deref(memory[VAL(list)]);
        let itemContainer = {};
        if(convert_method_argument(itemPL, {type: itemType}, module, itemContainer, reportError)) {
            array.push(itemContainer.value);
        } else {
            return false;
        }

        list = deref(memory[VAL(list) + 1]);
    }

    arrayContainer.value = array;
    return true;
}

function result_array_to_terms(arrayJS, itemType, module, arrayContainer, reportError) {
    if(arrayJS.length === 0) {
        arrayContainer.value = NIL;
    } else {

        arrayContainer.value = state.H ^ (TAG_LST << WORD_BITS);
        for (var i = 0; i < arrayJS.length; i++) {
            let itemContainer = {};
            if(convert_result(arrayJS[i], {type: itemType}, module, itemContainer, reportError)) {
                memory[state.H] = itemContainer.value;
                // If there are no more items we will overwrite the last entry with [] when we exit the loop
                memory[state.H + 1] = ((state.H + 2) ^ (TAG_LST << WORD_BITS));
                state.H += 2;
            } else {
                return false;
            }
        }
        memory[state.H - 1] = NIL;
    }
    return true;
}

function convert_result(resultJS, spec, module, resultContainer, reportError) {
    let resultPL;
    if(typeof spec.type === 'object') {
        if(spec.type.arrayType) {
            // [X1, X2, ...] for array of arrayType items
            if(Array.isArray(resultJS)) {
                let arrayContainer = {};
                if(result_array_to_terms(resultJS, spec.type.arrayType, module, arrayContainer, reportError)) {
                    resultPL = arrayContainer.value;
                } else {
                    return false;
                }
            } else {
                return reportError && type_error('array of ' + JSON.stringify(spec.type.arrayType), lookup_atom(JSON.stringify(resultJS)));
            }
        } else {
            // union of multiple types
            for (let subtype of spec.type) {
                if (convert_method_argument(resultJS, {type: subtype}, module, resultContainer, false)) {
                    return true;
                }
            }
            return reportError && type_error('union: ' + spec.type, lookup_atom(JSON.stringify(resultJS)));
        }
    } else if(spec.type === 'atom' || spec.type === 'string') {
        resultPL = lookup_atom(resultJS);
    } else if(spec.type === 'string_codes') {
        resultPL = string_to_codes(resultJS);
    } else if(spec.type === 'number') {
        resultPL = PL_put_integer(resultJS);
    } else if(spec.type === 'boolean') {
        resultPL = resultJS;
    } else if(spec.type === 'object') {
        resultPL = create_object_structure(resultJS);
    } else if(spec.type === 'dom_rect') {
        let ftor = lookup_functor('dom_rect', 8);
        resultPL = alloc_structure(ftor);
        let elements = [resultJS.left, resultJS.top, resultJS.right, resultJS.bottom,
            resultJS.x, resultJS.y, resultJS.width, resultJS.height];
        for(let element of elements) {
            let localContainer = {};
            if (!getNumberPLPropertyValue(element, localContainer, reportError)) {
                return false;
            } else {
                memory[state.H++] = localContainer.value;
            }
        }
    } else {
        return reportError && type_error('method result specification type', lookup_atom(spec.type));
    }
    resultContainer.value = resultPL;
    return true;
}

function object_method_no_return() {
    let object = arguments[0];
    let object_method = arguments[1];
    let method_arguments = arguments[2];
    Reflect.apply(object[object_method], object, method_arguments);
}

function object_method_return() {
    let object = arguments[0];
    let object_method = arguments[1];
    let method_arguments = arguments[2];
    return Reflect.apply(object[object_method], object, method_arguments);
}
// File dump.js
"use strict";

function dump(filter, mode) {
    if(mode && mode === 'load') {
        load_state();
    }

    let indexedCount = 0;
    let fullyIndexedCount = 0;
    let infos = [
        {ofst: 2, label: 'CA', bit: 1},
        {ofst: 3, label: 'CI', bit: 2},
        {ofst: 4, label: 'CF', bit: 4},
        {ofst: 5, label: 'L', bit: 8},
        {ofst: 6, label: 'S', bit: 16}
        ];
    let counts = {};

    for(let key of Object.keys(predicates)) {
        let predicate = predicates[key];
        if(typeof predicate.index !== 'undefined') {
            indexedCount++;

            let sequenceCount = 0;
            for(let clauseKey of predicate.clause_keys) {
                if(clauseKey !== predicate.index) {
                    let clause = predicate.clauses[predicate.clause_keys[clauseKey]];
                    let code = clause.code;
                    if (code[0] === 30 || code[0] === 254) {
                        // trust_me or nop2 instruction
                        // trust_me indicates a sequence of 2 or more clauses,
                        // nop2 indicates a sequence of one clause.
                        sequenceCount++;
                    }
                }
            }

            if(sequenceCount === 1) {
                fullyIndexedCount++;

                // inspect switch_on_term instruction.
                let clause = predicate.clauses[predicate.clause_keys[predicate.index]];
                let code = clause.code;

                // switch_on_term: [44, V, CA, CI, CF, L, S]

                let codePosition = 2; //first two slots are for nop2 or try_me_else.

                let V = code[codePosition + 1];
                let maskName = '';
                let maskBits = 0;

                for (let info of infos) {
                    info.value = decode_address(code[codePosition + info.ofst]);
                    if (info.value !== 'fail') {
                        maskName += ((maskName !== '') ? '/' : '') + info.label;
                        maskBits = maskBits | info.bit;
                    }
                }

                if (counts[maskBits]) {
                    counts[maskBits].counter++;
                } else {
                    counts[maskBits] = {name: maskName, counter: 1};
                }
            }
        }
    }

    dumpWrite("Loaded " + Object.keys(predicates).length + " predicates");
    dumpWrite("    " + indexedCount + " indexed, " + fullyIndexedCount + " single sequence");

    let pad = '        ';

    dumpWrite("    single sequence types:");
    for(let countKey of Object.keys(counts)) {
        let count = counts[countKey];
        dumpWrite(pad + count.name + ': ' + count.counter);
    }

    dumpWrite("Loaded " + atable.length + " atoms");
    dumpWrite("Loaded " + ftable.length + " functors");

    for(var ofst = 0;ofst < ftable.length;ofst++) {
        var functionPair = ftable[ofst];
        var predicateName = atable[functionPair[0]];
        let predicate = predicates[ofst];
        if (!filter ||
            (filter && ((filter === 'defined-predicate' && predicate)
                || (filter && filter === 'indexed-predicate' && predicate && predicate.index)
                || (filter === 'undefined-predicate' && !predicate)))) {
            dumpWrite(predicateName + '/' + functionPair[1] + ': ftor=' + ofst + ', atable ofst=' + functionPair[0] +
                ', ' + (predicate ?
                    ('has' + (predicate.index ? ' indexed' : ''))
                    : 'no') + ' predicate clauses');
        }
    }

}

function dumpPredicate(targetPredicateName, targetArity, mode) {
    if(mode && mode === 'load') {
        load_state();
    }

    for(var ofst = 0;ofst < ftable.length;ofst++){
        var functionPair = ftable[ofst];
        var predicateName = atable[functionPair[0]];
        if(predicateName === targetPredicateName && (! targetArity || functionPair[1] === targetArity)) {
            dumpWrite(predicateName + '/' + functionPair[1]);
            for(let clauseKey of predicates[ofst].clause_keys) {
                dumpWrite('---');
                dumpWrite('Clause ' + clauseKey);
                dumpWrite(' ');
                let clause = predicates[ofst].clauses[clauseKey];
                dumpCode(ofst, clause.code);
            }
            return;
        }
    }
}

function dumpCode(ftableOfst, code) {
    let position = 0;
    while(position < code.length) {
        let decoded = decode_instruction_general(ftableOfst, position, code);
        dumpWrite(decoded.string);
        position += decoded.size;
    }
}

function danglingPredicates(mode) {
    if(mode && mode === 'load') {
        load_state();
    }

//    dumpWrite('Dangling predicates:');

    let dangles = [];


    for(let predicateKey of Object.keys(predicates)) {
        let predicate = predicates[predicateKey];
        for(let clauseKey of predicate.clause_keys) {
            let clause = predicate.clauses[clauseKey];
            code = clause.code;
            let position = 0;
             while(position < code.length) {
                let decoded = decode_instruction({key:predicateKey},position);
                if(decoded.goalPredicate !== 'none' && ! predicates[decoded.goalPredicate.predicate]
                 && ! foreign_predicates[decoded.goalPredicate.predicate]) {
                    let references = dangles[decoded.goalPredicate.predicate];
                    if(typeof references === 'undefined') {
                        references = [];
                        dangles[decoded.goalPredicate.predicate] = references;
                    }
                    references.push({goalPredicate: decoded.goalPredicate, callingPredicate: predicateKey});
                }
                position += decoded.size;
            }

        }
    }

    for(let moduleID of Object.keys(module_exports)) {
        let moduleExports = module_exports[moduleID];
        let moduleName = atable[moduleID];
        let importedModules = module_imports[moduleID];

        for(let nameIDArity of moduleExports) {
            let nameID = nameIDArity[0];
            let arity = nameIDArity[1];
            let name = atable[nameID];
            let qualifiedName = moduleName + ':' + name;
            let predicateIDPL = lookup_functor(qualifiedName, arity);
            let predicateID = VAL(predicateIDPL);
            if(! predicates[predicateID]
                && ! foreign_predicates[predicateID]) {
                let found = false;
                for(let importedModule of importedModules) {
                    for( let pair of module_exports[importedModule]) {
                        if(pair[0] === nameID && pair[1] === arity) {
                            found = true;
                            break;
                        }
                    }
                }

                if(! found) {
                    let references = dangles[predicateID];
                    if (typeof references === 'undefined') {
                        references = [];
                        dangles[predicateID] = references;
                    }
                    references.push({export: moduleName});
                }
            }
        }
    }

    for(let predicateKey of Object.keys(dangles)){
        var functionPair = ftable[predicateKey];
        var predicateName = atable[functionPair[0]];
            dumpWrite('Undefined predicate: ' + predicateName + '/' + functionPair[1] + ' called by ');
            let references = dangles[predicateKey];
            for(let reference of references) {
                if(reference.callingPredicate) {
                    let caller = reference.callingPredicate;
                    let callerFunctionPair = ftable[caller];
                    let callerPredicateName = atable[callerFunctionPair[0]];
                    dumpWrite(' ' + callerPredicateName + '/' + callerFunctionPair[1]);
                } else {
                    dumpWrite(' exported by ' + reference.export);
                }
            }
            dumpWrite('\n');
     }
}

function unusedPredicates(mode) {
    if(mode && mode === 'load') {
        load_state();
    }

    let used = [];

    let retractPredicateID = VAL(lookup_functor('bootstrap_js:retract', 1));

    for(let predicateKey of Object.keys(predicates)) {
        let predicate = predicates[predicateKey];
        for(let clauseKey of predicate.clause_keys) {
            let clause = predicate.clauses[clauseKey];
            code = clause.code;
            let position = 0;
            let opCodePositions = [];
            while(position < code.length) {
                opCodePositions.push(position);
                let decoded = decode_instruction({key:predicateKey},position);
                if(decoded.goalPredicate !== 'none' && ! used.includes(decoded.goalPredicate.predicate) ) {

                    // let pair = ftable[decoded.goalPredicate.predicate];
                    // let qualifiedName = atable[pair[0]];
                    // let arity = pair[1];
                    // let moduleName = qualifiedName.substring(0, qualifiedName.indexOf(":"));

                    used.push(decoded.goalPredicate.predicate);
                }

                if(decoded.goalPredicate !== 'none' && decoded.goalPredicate.predicate === retractPredicateID ) {
                    let result = decode_retract_argument(position, opCodePositions);
                    if (result.ftor) {
                        let pair = ftable[result.ftor];
                        let baseName = atable[pair[0]];
                        let arity = pair[1];

                        let pushFtor;

                        if(baseName.includes(':')) {
                            pushFtor = result.ftor;
                        } else {
                            let qualifiedName = atable[result.module] + ':' + baseName;
                            pushFtor = VAL(lookup_functor(qualifiedName, arity));
                        }
                        used.push(pushFtor);
                    }
                }
                position += decoded.size;
            }
        }
    }

    for(let moduleID of Object.keys(module_exports)) {
        let moduleExports = module_exports[moduleID];
        let moduleName = atable[moduleID];
        for(let nameIDArity of moduleExports) {
            let nameID = nameIDArity[0];
            let arity = nameIDArity[1];
            let name = atable[nameID];
            let qualifiedName = moduleName + ':' + name;
            let predicateIDPL = lookup_functor(qualifiedName, arity);
            let predicateID = VAL(predicateIDPL);
            if(! used.includes(predicateID)) {
                used.push(predicateID);
            }
        }
    }

    for(let predicateID of system) {
        if(! used.includes(predicateID)) {
            used.push(predicateID);
        }
    }

    for(let predicateID of initialization) {
        if(! used.includes(predicateID)) {
            used.push(predicateID);
        }
    }

    let unused = [];
    for(let predicateKey of Object.keys(predicates)) {
        let predicateID = parseInt(predicateKey);
        if(! used.includes(predicateID)) {
            let pair = ftable[predicateID];
            let qualifiedName = atable[pair[0]];
            //let arity = pair[1];
            let moduleName = qualifiedName.substring(0, qualifiedName.indexOf(":"));
            if(moduleName !== 'user') {
                unused.push(predicateID);
            }
        }
    }

    if(unused.length > 0) {
        let plural = unused.length === 1 ? '' : 's';

        dumpWrite('Unused predicate' + plural + ':');

        for (let predicateID of unused) {
            let pair = ftable[predicateID];
            let predicateName = atable[pair[0]];
            dumpWrite(predicateID + ' <- ' + predicateName + '/' + pair[1]);
        }
    }
}

function dumpWrite(msg) {
    if(typeof console === 'object' && typeof console.log === 'function') {
        console.log(msg);
    } else if(typeof print === 'function') {
        print(msg);
    }
}
/* This file generated automatically. It defines the ProscriptLS system that is evaluated by the runtime engine. */
"use strict";
function load_state() {
bootstrap_code = [0,255,3,185,0,4,186,42];
retry_foreign_offset = 7;
atable = ["[]", "system:acyclic_term", "system", "acyclic_term", "system:subsumes_term", "subsumes_term", "system:compare", "compare", "system:var", "var", "system:atom", "atom", "system:integer", "integer", "system:float", "float", "system:compound", "compound", "system:ground", "ground", "system:=", "=", "system:==", "==", "system:functor", "functor", "system:arg", "arg", "system:=..", "=..", "system:copy_term", "copy_term", "system:halt", "halt", "system:current_prolog_flag", "current_prolog_flag", "system:set_prolog_flag", "set_prolog_flag", "system:repeat", "repeat", "system:atom_length", "atom_length", "system:atom_concat", "atom_concat", "system:sub_atom", "sub_atom", "system:char_code", "char_code", "system:atom_chars", "atom_chars", "system:atom_codes", "atom_codes", "system:number_chars", "number_chars", "system:number_codes", "number_codes", "system:char_conversion", "char_conversion", "system:current_char_conversion", "current_char_conversion", "system:current_predicate", "current_predicate", ":", "system:@>", "@>", "system:@>=", "@>=", "system:@<", "@<", "system:@=<", "@=<", "system:is", "is", "system:>", ">", "system:<", "<", "system:=<", "=<", "system:>=", ">=", "system:=:=", "=:=", "system:=\\=", "=\\=", "system:set_input", "set_input", "system:set_output", "set_output", "system:current_output", "current_output", "system:current_input", "current_input", "system:get_char", "get_char", "system:get_code", "get_code", "system:peek_char", "peek_char", "system:peek_code", "peek_code", "system:put_char", "put_char", "system:put_code", "put_code", "system:get_byte", "get_byte", "system:peek_byte", "peek_byte", "system:put_byte", "put_byte", "system:flush_output", "flush_output", "system:at_end_of_stream", "at_end_of_stream", "system:set_stream_position", "set_stream_position", "system:stream_property_1", "stream_property_1", "system:current_stream", "current_stream", "system:write_term", "write_term", "system:current_op", "current_op", "system:fail", "fail", "system:true", "true", "system:term_variables", "term_variables", "system:writeln", "writeln", "system:gensym", "gensym", "system:atom_to_term", "atom_to_term", "system:clause", "clause", "t0", "?", "system:abolish", "abolish", "system:retract_clause", "retract_clause", "system:read_term", "read_term", "system:open", "open", "system:close", "close", "system:op", "op", "system:atom_to_memory_file", "atom_to_memory_file", "system:memory_file_to_atom", "memory_file_to_atom", "system:new_memory_file", "new_memory_file", "system:open_memory_file", "open_memory_file", "system:free_memory_file", "free_memory_file", "system:format", "format", "system:flag", "flag", "system:memory_file_description", "memory_file_description", "system:copy_memory_file_to_local_storage", "copy_memory_file_to_local_storage", "system:copy_local_storage_to_memory_file", "copy_local_storage_to_memory_file", "system:absolute_file_name", "absolute_file_name", "system:numbervars", "numbervars", "system:reset_compile_buffer", "reset_compile_buffer", "system:emit_code", "emit_code", "system:lookup_atom", "lookup_atom", "system:lookup_float", "lookup_float", "system:lookup_functor", "lookup_functor", "system:add_clause_to_predicate", "add_clause_to_predicate", "system:add_clause_to_aux", "add_clause_to_aux", "system:prepend_clause_to_predicate", "prepend_clause_to_predicate", "system:flush_stdout", "flush_stdout", "system:debug", "debug", "system:nodebug", "nodebug", "system:$jmp", "$jmp", "system:generate_initialization_goal", "generate_initialization_goal", "system:generate_system_goal", "generate_system_goal", "system:define_dynamic_predicate", "define_dynamic_predicate", "system:compiled_state_boot_code", "compiled_state_boot_code", "system:dump_tables", "dump_tables", "system:compile_buffer_codes", "compile_buffer_codes", "system:add_module_export", "add_module_export", "system:module_export", "module_export", "system:add_module_import", "add_module_import", "system:module_import", "module_import", "system:add_meta_predicate", "add_meta_predicate", "system:pls_meta_predicate", "pls_meta_predicate", "system:indexing_mode", "indexing_mode", "system:set_indexing_mode", "set_indexing_mode", "system:indexable_compiled_predicates", "indexable_compiled_predicates", "system:register_indexed_predicate", "register_indexed_predicate", "system:compiled_clauses", "compiled_clauses", "system:add_index_clause_to_predicate", "add_index_clause_to_predicate", "system:edit_clauses_for_index_sequences", "edit_clauses_for_index_sequences", "system:decode_instruction", "decode_instruction", "system:request_result", "request_result", "system:handle_result", "handle_result", "system:fetch_promise", "fetch_promise", "system:trace_unify", "trace_unify", "system:$trace_set", "$trace_set", "system:$trace_value", "$trace_value", "system:$trace_set_info", "$trace_set_info", "system:$suspend_set", "$suspend_set", "system:get_terminal_char", "get_terminal_char", "system:$trace_set_retry", "$trace_set_retry", "system:$trace_retry_value", "$trace_retry_value", "system:$trace_set_prompt", "$trace_set_prompt", "system:$get_backtrack_frame", "$get_backtrack_frame", "system:$set_backtrack_frame", "$set_backtrack_frame", "system:$trace_instruction_set", "$trace_instruction_set", "system:clear_local_storage_log", "clear_local_storage_log", "system:member_test", "member_test", "system:mark_top_choicepoint", "mark_top_choicepoint", "system:unmark_choicepoint", "unmark_choicepoint", "system:unmark_top_choicepoint", "unmark_top_choicepoint", "system:get_current_block", "get_current_block", "system:install_new_block", "install_new_block", "system:reset_block", "reset_block", "system:unwind_stack", "unwind_stack", "system:clean_up_block", "clean_up_block", "system:throw", "throw", "system:get_exception", "get_exception", "system:clear_exception", "clear_exception", "system:recorda", "recorda", "system:recordz", "recordz", "system:recorded", "recorded", "system:erase", "erase", "system:record_term", "record_term", "system:gc", "gc", "system:statistics", "statistics", "system:wam_duration", "wam_duration", "system:statistics_heap_size", "statistics_heap_size", "system:statistics_max_stack", "statistics_max_stack", "system:statistics_max_heap", "statistics_max_heap", "system:eval_javascript", "eval_javascript", "system:remove_dom_element_class", "remove_dom_element_class", "system:replace_dom_element_class", "replace_dom_element_class", "system:toggle_dom_element_class", "toggle_dom_element_class", "system:set_dom_element_attribute_value", "set_dom_element_attribute_value", "system:dom_element_attribute_value", "dom_element_attribute_value", "system:create_dom_element", "create_dom_element", "system:create_dom_text_node", "create_dom_text_node", "system:append_dom_node_child", "append_dom_node_child", "system:insert_before_dom_node", "insert_before_dom_node", "system:dom_select_element", "dom_select_element", "system:dom_select_all_elements", "dom_select_all_elements", "system:dom_object_property", "dom_object_property", "system:dom_object_method", "dom_object_method", "system:dom_object_type", "dom_object_type", "system:dom_create_object", "dom_create_object", "system:dom_type_reference", "dom_type_reference", "system:dom_release_object", "dom_release_object", "system:set_dom_object_property", "set_dom_object_property", "system:alert", "alert", "system:dom_window", "dom_window", "system:dom_type_property", "dom_type_property", "system:dom_type_method", "dom_type_method", "system:dom_type_parent", "dom_type_parent", "toplevel", "user", "user:toplevel", "bootstrap_js", "append", "assert", "reverse", "save_clausea", "handle_term_expansion", "include", "dynamic", "ensure_loaded", "module", "use_module", "compile_message", "??", "otherwise", "end_block", "findall", "setof", "bagof", "asserta", "assertz", "retract", "unify_with_occurs_check", "\\=", "\\==", "atomic", "nonvar", "number", "stream_property", "read", "write", "writeq", "write_canonical", "callable", "retractall", "sort", "keysort", "length", "delete", "call_with_module", "call", "decode_instructions", "yield", "not", "free_variables", "bootstrap_js:\\==", "not:\\==", "term_is_free_of", "not:term_is_free_of", "list_is_free_of", "not:list_is_free_of", "not:free_variables", "explicit_binding", "not:explicit_binding", "-", "\\+", "^", "+", "bootstrap_js:setof", "bootstrap_js:bagof", "bootstrap_js:set_of", "bootstrap_js:bag_of", "bootstrap_js:call", "t1", "t2", "t3", "t4", "t5", "t6", "t7", "bootstrap_js:retractall", "bootstrap_js:asserta", "bootstrap_js:assertz", "bootstrap_js:retract", "bootstrap_js:findall", "bootstrap_js:dynamic", "wam_compiler", "wam_compiler:define_current_module", "bootstrap_js:module", "wam_compiler:define_use_module", "bootstrap_js:use_module", "bootstrap_js:assert", "bootstrap_js:save_clausea", ":-", "wam_compiler:transform_meta_expression", "save_clausea1", "bootstrap_js:save_clausea1", "bootstrap_js:length", "wam_compiler:transform_predicate_name", "/", "wam_compiler:current_compilation_module", "bootstrap_js:handle_term_expansion", "bootstrap_js:include", "wam_compiler:consult", "bootstrap_js:call_with_module", "wam_compiler:compile_clause_2", "wam_compiler:push_current_compilation_module", "wam_compiler:pop_current_compilation_module", "query", ",", "wam_compiler:canonical_source", "wam_compiler:$loaded", "bootstrap_js:ensure_loaded", "bootstrap_js:format", "bootstrap_js:compile_message", "bootstrap_js:end_block", "bootstrap_js:halt", "bootstrap_js:otherwise", "CALL ~q~n", "exception", "exit", "!", "FAIL ~q~n", "EXIT ~q~n", "CUT  ~q~n", "error", "ERROR ~q ~p~n", "PEND ~q~n", "bootstrap_js:??", "bootstrap_js:?", "save_instances", "bootstrap_js:save_instances", "list_instances", "bootstrap_js:list_instances", "bag_of", "bootstrap_js:sort", "set_of", "bootstrap_js:keysort", "concordant_subset", "bootstrap_js:concordant_subset", ".", "wam_util", "wam_util:existential_variables", "replace_key_variables", "bootstrap_js:replace_key_variables", "bootstrap_js:nonvar", "bootstrap_js:unify_with_occurs_check", "bootstrap_js:\\=", "bootstrap_js:number", "bootstrap_js:atomic", "wam_compiler:save_clause", "retract1", "bootstrap_js:retract1", "bootstrap_js:open", "bootstrap_js:close", "bootstrap_js:flush_output", "bootstrap_js:stream_property", "bootstrap_js:at_end_of_stream", "bootstrap_js:get_char", "bootstrap_js:get_code", "bootstrap_js:peek_char", "bootstrap_js:peek_code", "bootstrap_js:put_char", "bootstrap_js:put_code", "bootstrap_js:get_byte", "bootstrap_js:peek_byte", "bootstrap_js:put_byte", "bootstrap_js:read_term", "bootstrap_js:read", "bootstrap_js:write_term", "quoted", "false", "ignore_ops", "bootstrap_js:write", "bootstrap_js:writeq", "bootstrap_js:write_canonical", "bootstrap_js:callable", "retractall1", "bootstrap_js:retractall1", "check_sorted", "bootstrap_js:check_sorted", "sort1", "bootstrap_js:sort1", "sort2", "bootstrap_js:sort2", "partition", "bootstrap_js:partition", "check_keysorted", "bootstrap_js:check_keysorted", "keysort1", "bootstrap_js:keysort1", "key_partition", "bootstrap_js:key_partition", "bootstrap_js:append", "bootstrap_js:reverse", "bootstrap_js:delete", "add_args", "bootstrap_js:add_args", "call_extension", "bootstrap_js:call_extension", "call_extension_with_module", "bootstrap_js:call_extension_with_module", "user::", "bootstrap_js:decode_instructions", "instruction", "inst", "state.wamYielded = true; setTimeout(() => {state.wamYielded = false;promise_backtrack();}, 0)", "bootstrap_js:yield", "debugger", "trace", "notrace", "notrace_backtrackable", "$traceR", "system_util", "member", "write_list", "capitalize", "random", "random_between", "system_util:member", "system_util:write_list", "write_list1", "system_util:write_list1", "capitalize_code", "system_util:capitalize_code", "system_util:capitalize", "random_float", "system_util:random", "system_util:random_between", "$trace_spy_specification", "debugger:$trace_spy_specification", "$trace_max_depth", "debugger:$trace_max_depth", "debugger:trace", "no_trace", "debugger:notrace", "no_trace_backtrackable", "debugger:notrace_backtrackable", "~w\n", "$trace_push_info", "debugger:$trace_push_info", "debugger:$traceR", "$trace_retry", "debugger:$trace_retry", "$trace_interact", "debugger:$trace_interact", "$trace_suspend_if_active", "debugger:$trace_suspend_if_active", "$trace_is_suspended", "debugger:$trace_is_suspended", "redo", "skip_trace", "suspend_leap_trace", "leap_trace", "none", "$trace_interaction_enabled", "debugger:$trace_interaction_enabled", "$trace_prompt", "debugger:$trace_prompt", "$trace_read_and_cmd", "debugger:$trace_read_and_cmd", "$trace_cmd", "debugger:$trace_cmd", "l", "$trace_spy_mode", "debugger:$trace_spy_mode", "all", "specified", "Found spec for ", " ", "$trace_spy_mode1", "debugger:$trace_spy_mode1", "$trace_create_prompt", "debugger:$trace_create_prompt", "pad_number", "debugger:pad_number", "extract_module", "debugger:extract_module", "concat_list", "debugger:concat_list", "max_depth", ": ", "$trace_check_command", "debugger:$trace_check_command", "read_char", "debugger:read_char", "d", "z", "y", "x", "m", "n", "a", "g", "r", "f", "w", "s", "c", "Commands are: \"c\" (creep), \"s\" (skip), \"l\" (leap), \"+\" (spy this), \"-\" (nospy this), \n    \"d(Depth)\" (prompt max depth Depth), \"w\" (d(0)),  \n    \"f\" (fail), \"r\" (retry), \"g\" (ancestors), \"a\" (abort), \"n\" (nodebug), \n    \"m\" (creep wam), \"x\" (creep wam long), \"y\" (trace wam), \"z\" (trace wam long).", "$trace_cmd_creep", "debugger:$trace_cmd_creep", "leap_trace_next_jmp", "$trace_write_ancestors", "debugger:$trace_write_ancestors", "Spypoint placed on ", "Spypoint removed from ", "$trace_cmd_creep_wam", "debugger:$trace_cmd_creep_wam", "trace_next_jmp", "step", "$trace_write_ancestors1", "debugger:$trace_write_ancestors1", "Ancestors:", "$suspend", "debugger:$suspend", "pad_codes", "debugger:pad_codes", "pad_codes1", "debugger:pad_codes1", "", "~w", "url", "current_compile_url", "push_current_compile_url", "pop_current_compile_url", "convert_URL_to_base", "url_directory", "resolve_url", "$current_compile_url", "url:$current_compile_url", "url:current_compile_url", "url:push_current_compile_url", "url:pop_current_compile_url", "absolute_url", "url:absolute_url", "url:url_directory", "url:resolve_url", "url:convert_URL_to_base", "./", "sub", "url:sub", "trim_directory", "url:trim_directory", "promise", "promise_result", "promise:promise_result", "compile_clause", "compile_files", "compile_and_free_memory_file", "save_compiled_state", "bootstrap_toplevel", "consult_atom", "repl", "call_atom", "compile_clause_system_directive", "wam_compiler:compile_clause_system_directive", "wam_compiler:bootstrap_toplevel", "wam_assemble", "assemble", "encode_opcodes_1", "wam_assemble:encode_opcodes_1", "link", "wam_assemble:link", "linking", "linked", "wam_assemble:assemble", "link_1", "wam_assemble:link_1", "label", "address_of", "to", "emitting_address_1_of", "aux_address_of", "emitting_aux_address_2_of", "extended_address", "emit_codes", "wam_assemble:emit_codes", "encode_register", "wam_assemble:encode_register", "encode_fail", "wam_assemble:encode_fail", "aux_label", "auxiliary", "call_aux", "    ", "at", "need_address_of", "execute_aux", "try_me_else", "retry_me_else", "encode_label_or_fail", "wam_assemble:encode_label_or_fail", "switch_on_term", "encode_switch_table_typed", "wam_assemble:encode_switch_table_typed", "switch_on_constant", "switch_on_structure", "encode_opcode", "wam_assemble:encode_opcode", "clause_offset", "invalid_address", "encode_switch_table_hash", "wam_assemble:encode_switch_table_hash", "encode_switch_table", "wam_assemble:encode_switch_table", "label_targets", "wam_assemble:label_targets", "complete_buckets", "wam_assemble:complete_buckets", "bucket_labels", "wam_assemble:bucket_labels", "encode_switch_bucket_labels", "wam_assemble:encode_switch_bucket_labels", "encode_switch_bucket_tables", "wam_assemble:encode_switch_bucket_tables", "log", "floor", "min", "<<", "hash", "wam_assemble:hash", "/\\", "complete_buckets1", "wam_assemble:complete_buckets1", "invalid_bucket_id", "encode_switch_table_targets", "wam_assemble:encode_switch_table_targets", "encode_switch_table_target", "wam_assemble:encode_switch_table_target", "invalid_table_value", "allocate", "deallocate", "execute", "proceed", "put_variable", "put_value", "put_unsafe_value", "illegal_unsafe_value_register", "put_constant", "put_nil", "put_structure", "put_list", "put_integer", "put_float", "get_variable", "get_value", "get_constant", "get_nil", "get_structure", "get_list", "get_integer", "get_float", "unify_void", "unify_variable", "unify_value", "unify_local_value", "unify_constant", "unify_integer", "unify_float", "trust_me", "*", "try", "retry", "trust", "goto_clause", "neck_cut", "cut", "get_level", "retry_foreign", "get_choicepoint", "nop2", "transform_predicate_name1", "path_to_module_name", "list_length", "library_name_to_path", "library_path", "existential_variables", "wam_util:transform_predicate_name1", "../../library/", "wam_util:library_path", "wam_util:library_name_to_path", "path_to_file_name", "wam_util:path_to_file_name", "strip_suffix", "wam_util:strip_suffix", "wam_util:path_to_module_name", "list_length_1", "wam_util:list_length_1", "wam_util:list_length", "wam_index_predicates", "wam_index_writeln", "wam_index_predicates:wam_index_writeln", "wam_index_predicates:wam_index_predicates", "wam_index_predicate", "wam_index_predicates:wam_index_predicate", "wam_index_clauses", "wam_index_predicates:wam_index_clauses", "===", "indexing", "wam_index_clauses1", "wam_index_predicates:wam_index_clauses1", "clause_sequences", "wam_index_predicates:clause_sequences", "trim_sequences", "wam_index_predicates:trim_sequences", "degenerate_sequences", "wam_index_predicates:degenerate_sequences", "index_sequences", "wam_index_predicates:index_sequences", "sequences_ids", "wam_index_predicates:sequences_ids", "degenerate", "sequence_ids", "wam_index_predicates:sequence_ids", "clause_sequence", "wam_index_predicates:clause_sequence", "head_has_var_in_first_arg", "wam_index_predicates:head_has_var_in_first_arg", "trim_sequence", "wam_index_predicates:trim_sequence", "index_sequences1", "wam_index_predicates:index_sequences1", "index_sequence", "wam_index_predicates:index_sequence", "analyze", "wam_index_predicates:analyze", "group", "wam_index_predicates:group", "switch_instruction", "wam_index_predicates:switch_instruction", "list", "structure", "constant", "head_has_atom_in_first_arg", "wam_index_predicates:head_has_atom_in_first_arg", "head_has_integer_in_first_arg", "wam_index_predicates:head_has_integer_in_first_arg", "head_has_float_in_first_arg", "wam_index_predicates:head_has_float_in_first_arg", "head_has_list_in_first_arg", "wam_index_predicates:head_has_list_in_first_arg", "head_has_structure_in_first_arg", "wam_index_predicates:head_has_structure_in_first_arg", "list_group_instruction", "wam_index_predicates:list_group_instruction", "constants", "wam_index_predicates:constants", "structures", "wam_index_predicates:structures", "group1", "wam_index_predicates:group1", "group_instructions", "wam_index_predicates:group_instructions", "select", "wam_index_predicates:select", "head_constant_in_first_arg", "wam_index_predicates:head_constant_in_first_arg", "head_structure_in_first_arg", "wam_index_predicates:head_structure_in_first_arg", "group_instruction1", "wam_index_predicates:group_instruction1", "group_instruction", "wam_index_predicates:group_instruction", "group_instruction2", "wam_index_predicates:group_instruction2", "type_instruction", "wam_index_predicates:type_instruction", "bootstrap", "wam_compiler:bootstrap", "delayed_initialization", "wam_compiler:delayed_initialization", "$loaded", "current_compilation_module", "term_expansion", "expand_term", "wam_compiler:expand_term", "wam_compiler:compile_clause", "compile", "mode", "compile_clause_1", "wam_compiler:compile_clause_1", "Compiling ", "done_gc", "Failed compilation of ", "compile_clause_save", "wam_compiler:compile_clause_save", "compile_clause_directive", "wam_compiler:compile_clause_directive", "mode_skip", "wam_compiler:mode_skip", "compile_clause_2", "save_clause", "skip", "compile_clause_directive_macro", "wam_compiler:compile_clause_directive_macro", "compile_clause_directive_nonmacro", "wam_compiler:compile_clause_directive_nonmacro", "compile_clause_compilation_directive", "wam_compiler:compile_clause_compilation_directive", "compile_clause_initialization_directive", "wam_compiler:compile_clause_initialization_directive", "initialization", "reexport", "meta_predicate", "if", "else", "elif", "endif", "define_dynamic_predicates", "wam_compiler:define_dynamic_predicates", "define_current_module", "define_use_module", "define_reexport", "wam_compiler:define_reexport", "define_meta_predicate", "wam_compiler:define_meta_predicate", "macro_if", "wam_compiler:macro_if", "macro_else", "wam_compiler:macro_else", "macro_elif", "wam_compiler:macro_elif", "macro_endif", "wam_compiler:macro_endif", "if skip level >= iflevelnext", "else ifLevel less than skiplevel", "else ifLevel greater than compileLevel", "else ifLevel less than compileLevel", "elif skip level > iflevel", "if skip level not = iflevel", "if compile level not = iflevel", "endif unknown action", "define_dynamic_predicate1", "wam_compiler:define_dynamic_predicate1", "current_compilation_stream", "wam_compiler:current_compilation_stream", "push_current_compilation_module", "define_module_export", "wam_compiler:define_module_export", "library", "wam_compiler_compilation_modules", "pop_current_compilation_module", "setup_use_module", "wam_compiler:setup_use_module", "cyclic_module", "load_file_for_use_module", "wam_compiler:load_file_for_use_module", "canonical_source", "consult", "use_module_imports", "wam_compiler:use_module_imports", "no_imports", "transform_predicate_name", "default_meta_arg_types", "wam_compiler:default_meta_arg_types", "defined_meta_predicate", "wam_compiler:defined_meta_predicate", "current_import1", "wam_compiler:current_import1", "current_import", "wam_compiler:current_import", "plausible_module_name", "wam_compiler:plausible_module_name", "lowercase_letter_code", "wam_compiler:lowercase_letter_code", "uppercase_letter_code", "wam_compiler:uppercase_letter_code", "number_code", "wam_compiler:number_code", "transform_meta_expression", "?-", "transform_body", "wam_compiler:transform_body", "commit_to_cut", "wam_compiler:commit_to_cut", "entail", "wam_compiler:entail", "permanent_variable_list", "wam_compiler:permanent_variable_list", "environment_size_required", "wam_compiler:environment_size_required", "allocate_environment", "wam_compiler:allocate_environment", "first_goal_arity", "wam_compiler:first_goal_arity", "compile_body", "wam_compiler:compile_body", "compile_auxiliary_goals", "wam_compiler:compile_auxiliary_goals", "first", "next", "compile_head", "wam_compiler:compile_head", "no_cut", "user:term_expansion", "next_free_variable", "wam_compiler:next_free_variable", "has_cut", "goal", "not_first", "once", "include_cut_point_as_argument_if_needed", "wam_compiler:include_cut_point_as_argument_if_needed", "aux_head", "aux_definition", "local_cut", "instantiate_local_cut", "wam_compiler:instantiate_local_cut", "setup_call_catcher_cleanup", "get_top_choicepoint", "no_local_cut", "need", "catch", "forall", ";", "->", "variable_is_in_list", "wam_compiler:variable_is_in_list", "include_local_cut_in_arity", "wam_compiler:include_local_cut_in_arity", "compile_head_args", "wam_compiler:compile_head_args", "last_goal", "wam_compiler:last_goal", "grab_variables_from_goals", "wam_compiler:grab_variables_from_goals", "classify_variables", "wam_compiler:classify_variables", "variable_is_permanent", "wam_compiler:variable_is_permanent", "permanent_var", "last_occurrence", "ensure_vars_allocated", "wam_compiler:ensure_vars_allocated", "variable_is_known_permanent", "wam_compiler:variable_is_known_permanent", "get", "put", "deallocate_environment", "wam_compiler:deallocate_environment", "atom_or_empty_list", "wam_compiler:atom_or_empty_list", "compile_head_arg", "wam_compiler:compile_head_arg", "wam_compiler:get_variable", "compile_head_unification", "wam_compiler:compile_head_unification", "complete_head_unification", "wam_compiler:complete_head_unification", "wam_compiler:unify_variable", "fresh_variable", "wam_compiler:fresh_variable", "unify", "already_used", "wam_compiler:already_used", "unify_possibly_local_variable", "wam_compiler:unify_possibly_local_variable", "put_possibly_unsafe_value", "wam_compiler:put_possibly_unsafe_value", "wam_compiler:put_variable", "set", "mark_variable_as_safe", "wam_compiler:mark_variable_as_safe", "variable_has_not_been_trimmed", "wam_compiler:variable_has_not_been_trimmed", "resize_state", "wam_compiler:resize_state", "trim_environment", "wam_compiler:trim_environment", "compile_body_goals", "wam_compiler:compile_body_goals", "depart", "compile_goal", "wam_compiler:compile_goal", "compile_body_args", "wam_compiler:compile_body_args", "compile_aux_call", "wam_compiler:compile_aux_call", "variable_must_be_known_permanent", "wam_compiler:variable_must_be_known_permanent", "compile_predicate_call", "wam_compiler:compile_predicate_call", "wrong_type_of_functor", "missing_permanent_var", "compile_body_arg", "wam_compiler:compile_body_arg", "compile_body_arg_adjust", "wam_compiler:compile_body_arg_adjust", "guv", "adjust_unify_variable", "wam_compiler:adjust_unify_variable", "compile_body_unification", "wam_compiler:compile_body_unification", "functor_is_qualified", "wam_compiler:functor_is_qualified", "complete_compile_body_arg", "wam_compiler:complete_compile_body_arg", "complete_compile_body_arg_integer", "wam_compiler:complete_compile_body_arg_integer", "complete_compile_body_arg_tilde", "wam_compiler:complete_compile_body_arg_tilde", "info", "generated_unify_variable", "adjust_unify_variable1", "wam_compiler:adjust_unify_variable1", "adjust", "adjust_unify_variable11", "wam_compiler:adjust_unify_variable11", "adjust_unify_variable10", "wam_compiler:adjust_unify_variable10", "adjust_unify_variable2", "wam_compiler:adjust_unify_variable2", "wam_compiler:compile_files", "compile_file", "wam_compiler:compile_file", "index", "compile_stream", "wam_compiler:compile_stream", "wam_compiler_compilation_stream", "push_current_compilation_stream", "wam_compiler:push_current_compilation_stream", "pop_current_compilation_stream", "wam_compiler:pop_current_compilation_stream", "analyze_singletons", "wam_compiler:analyze_singletons", "compile_stream_term", "wam_compiler:compile_stream_term", "singletons", "variable_names", "assign_variable_names", "wam_compiler:assign_variable_names", "trim_singletons", "wam_compiler:trim_singletons", "singleton_variables", "in", "WARNING", "end_of_file", "macro error: missing endif directive.", "on", "wam_compiler:save_compiled_state", "/* This file generated automatically. It defines the ProscriptLS system that is evaluated by the runtime engine. */~n", "\"use strict\";~n", "function load_state() {~n", "bootstrap_code = ~w;~n", "retry_foreign_offset = 7;~n", "}~n", "call_list", "wam_compiler:call_list", "compile_atoms", "wam_compiler:compile_atoms", "compile_atom", "wam_compiler:compile_atom", "wam_compiler:compile_and_free_memory_file", "compile_memory_file", "wam_compiler:compile_memory_file", "process_delayed_initializations", "wam_compiler:process_delayed_initializations", "wam_compiler:consult_atom", "canonical_sources", "wam_compiler:canonical_sources", "fetch_promises", "wam_compiler:fetch_promises", "compile_results", "wam_compiler:compile_results", "true.", "print_bindings", "wam_compiler:print_bindings", "print_bindings_1", "wam_compiler:print_bindings_1", "repl_1", "wam_compiler:repl_1", "wam_compiler:repl", "//", "ms)", "(", "wam_compiler:call_atom"];
floats = [];
ftable = [[1,1], [4,2], [6,3], [8,1], [10,1], [12,1], [14,1], [16,1], [18,1], [20,2], [22,2], [24,3], [26,3], [28,2], [30,2], [32,1], [34,2], [36,2], [38,0], [40,2], [42,3], [44,5], [46,2], [48,2], [50,2], [52,2], [54,2], [56,2], [58,2], [60,1], [63,2], [65,2], [67,2], [69,2], [71,2], [73,2], [75,2], [77,2], [79,2], [81,2], [83,2], [85,1], [87,1], [89,1], [91,1], [93,2], [95,2], [97,2], [99,2], [101,2], [103,2], [105,2], [107,2], [109,2], [111,1], [113,1], [115,2], [117,2], [119,1], [121,3], [123,3], [125,0], [127,0], [129,2], [131,1], [133,2], [135,3], [137,2], [141,1], [143,2], [145,3], [147,4], [149,2], [151,3], [153,2], [155,2], [157,1], [159,3], [161,1], [163,3], [165,3], [167,2], [169,2], [171,2], [173,3], [175,3], [177,0], [179,2], [181,2], [183,2], [185,3], [187,3], [189,4], [191,3], [193,0], [195,0], [197,0], [199,1], [201,2], [203,2], [205,1], [207,1], [209,1], [211,1], [213,2], [215,2], [217,2], [219,2], [221,3], [223,3], [225,1], [227,1], [229,1], [231,1], [233,2], [235,1], [237,2], [239,4], [241,1], [243,2], [245,2], [247,2], [249,1], [251,1], [253,1], [255,1], [257,1], [259,1], [261,1], [263,1], [265,1], [267,1], [269,1], [271,0], [273,2], [275,2], [277,1], [279,0], [281,1], [283,1], [285,1], [287,0], [289,1], [291,1], [293,1], [295,0], [297,3], [299,3], [301,3], [303,1], [305,2], [307,0], [309,0], [311,1], [313,1], [315,1], [317,1], [319,1], [319,2], [321,2], [323,3], [325,3], [327,3], [329,3], [331,2], [333,2], [335,2], [337,3], [339,2], [341,2], [343,4], [345,2], [345,3], [347,2], [349,2], [349,3], [351,4], [353,1], [355,3], [355,4], [357,1], [359,1], [361,4], [363,5], [365,2], [367,0], [33,0], [369,0], [414,2], [415,2], [417,2], [419,2], [420,4], [422,4], [420,5], [423,2], [424,1], [425,2], [426,2], [427,3], [428,3], [429,3], [430,3], [417,3], [446,2], [447,2], [448,1], [449,1], [62,2], [441,1], [450,1], [451,1], [452,2], [453,3], [455,1], [456,2], [457,4], [458,2], [459,1], [460,1], [461,1], [462,1], [431,1], [463,2], [467,1], [464,1], [465,2], [466,2], [444,1], [468,2], [469,2], [470,1], [471,1], [472,2], [473,1], [474,2], [478,1], [475,0], [484,1], [476,0], [487,1], [488,1], [443,4], [443,3], [423,1], [490,2], [492,2], [494,2], [492,4], [496,2], [498,3], [501,3], [492,3], [492,5], [503,3], [504,1], [498,4], [498,5], [505,2], [506,2], [507,1], [508,1], [440,1], [509,1], [511,1], [442,1], [512,3], [513,1], [514,0], [515,2], [516,0], [517,1], [518,1], [519,1], [520,1], [521,1], [522,1], [523,1], [524,1], [525,1], [526,2], [527,1], [527,2], [528,2], [529,1], [531,1], [176,1], [532,1], [532,2], [533,1], [533,2], [534,1], [534,2], [535,1], [537,1], [439,1], [539,2], [541,3], [543,4], [545,4], [547,2], [549,3], [551,4], [552,3], [553,3], [553,2], [456,3], [554,3], [556,3], [558,2], [431,2], [431,3], [431,4], [431,5], [431,6], [431,7], [431,8], [561,2], [560,3], [562,4], [562,2], [563,5], [564,5], [566,0], [578,2], [579,3], [579,2], [581,3], [583,2], [584,2], [586,1], [576,1], [587,3], [589,3], [591,1], [592,0], [594,0], [596,0], [11,1], [599,3], [600,3], [602,2], [604,6], [606,0], [608,0], [608,1], [606,1], [604,5], [615,2], [617,4], [619,5], [621,6], [624,1], [615,3], [630,2], [632,5], [634,3], [636,2], [639,1], [638,2], [632,4], [642,1], [644,1], [645,1], [660,6], [590,1], [663,1], [588,3], [667,1], [671,2], [674,0], [676,2], [678,2], [638,3], [689,1], [690,1], [688,1], [691,1], [692,1], [694,1], [695,2], [696,3], [697,2], [700,3], [702,2], [705,2], [720,6], [723,2], [722,2], [725,2], [728,2], [727,3], [729,2], [730,1], [731,3], [732,2], [733,3], [734,1], [736,2], [647,1], [738,3], [648,1], [740,1], [741,1], [742,2], [743,3], [743,4], [744,1], [745,1], [746,2], [747,2], [747,3], [748,1], [748,2], [749,1], [749,2], [752,6], [752,7], [751,5], [755,2], [755,3], [754,8], [756,2], [756,3], [728,1], [744,2], [758,3], [759,1], [760,1], [762,8], [764,6], [775,1], [776,1], [777,2], [778,2], [766,3], [573,2], [768,3], [770,2], [772,5], [774,8], [780,3], [781,2], [768,4], [783,3], [784,1], [786,5], [788,5], [789,2], [138,1], [409,2], [792,1], [794,2], [794,1], [795,2], [796,2], [797,1], [798,2], [799,1], [800,2], [801,1], [802,2], [803,2], [804,2], [805,2], [806,2], [807,1], [808,2], [809,1], [810,2], [811,2], [812,1], [813,1], [814,1], [815,1], [816,1], [817,1], [818,1], [752,4], [820,2], [821,1], [822,1], [823,1], [824,1], [826,1], [827,1], [829,2], [837,4], [839,1], [840,2], [842,2], [844,3], [845,2], [847,3], [848,2], [851,1], [852,1], [852,0], [854,1], [858,1], [856,2], [860,2], [862,3], [864,2], [866,1], [871,1], [868,3], [870,2], [873,2], [875,5], [877,1], [879,3], [881,3], [883,9], [885,6], [887,5], [889,5], [894,1], [896,1], [898,1], [900,1], [902,1], [904,4], [906,2], [908,2], [910,3], [912,4], [910,4], [914,4], [916,2], [918,2], [920,3], [922,4], [924,3], [926,4], [928,0], [930,1], [933,2], [935,2], [936,1], [716,1], [937,1], [938,2], [936,3], [940,3], [945,3], [452,1], [947,3], [949,1], [952,1], [954,3], [956,3], [152,3], [715,1], [377,1], [958,3], [961,1], [960,1], [378,1], [376,1], [379,2], [380,1], [962,1], [963,1], [964,1], [966,1], [969,1], [973,1], [975,1], [977,3], [979,2], [981,3], [983,2], [993,1], [995,1], [998,2], [999,1], [459,2], [1003,2], [1004,1], [1006,1], [1010,2], [973,2], [1011,1], [1014,2], [1016,3], [1018,4], [1020,4], [1022,1], [1024,1], [1026,1], [1028,1], [929,1], [1030,1], [1032,6], [1034,1], [1036,2], [128,1], [1038,3], [1040,3], [1050,1], [1042,7], [1044,2], [1046,8], [1048,4], [1052,8], [1054,2], [1056,2], [1057,1], [1058,1], [1060,1], [409,1], [1063,2], [1064,4], [1065,1], [480,1], [1062,3], [1068,4], [282,1], [276,2], [284,1], [1069,2], [384,2], [286,1], [294,1], [21,2], [278,1], [1071,1], [1067,2], [1072,3], [1073,2], [1074,2], [1075,2], [408,2], [1077,2], [1079,3], [1081,7], [1083,3], [1036,3], [1085,3], [1087,6], [1090,3], [1091,1], [1089,6], [1093,5], [9,3], [1095,3], [1099,3], [1101,1], [1103,7], [1104,7], [1106,7], [1108,6], [1109,6], [1112,2], [1111,3], [1114,4], [1116,7], [1118,10], [1119,8], [1122,3], [1124,3], [1126,4], [1128,5], [1130,10], [1133,10], [1135,10], [1137,6], [1139,3], [1141,6], [1142,2], [1143,2], [1145,11], [1148,1], [1147,3], [1150,2], [1152,11], [1154,1], [1161,8], [1156,5], [1158,6], [1160,3], [1150,4], [1165,2], [1164,2], [1167,1], [1169,2], [1171,1], [1172,1], [1174,1], [1175,1], [1177,1], [931,1], [1180,1], [1182,1], [1177,2], [1187,1], [1188,1], [1184,4], [1186,3], [1190,1], [1192,2], [1193,1], [1194,1], [1195,2], [1185,1], [1199,2], [1199,1], [1207,1], [1209,1], [1211,1], [1212,1], [1214,1], [1216,0], [1217,1], [1219,2], [1221,2], [1223,1], [1226,1], [1228,1], [1230,1], [1231,1], [1232,2], [1235,2]];
dtable = [231, 333, 334, 374, 535, 536];
predicates = {187: {is_public:false, clauses:{0:{code:[254,0,4,62], key:0}}, clause_keys:[0], next_key:1, key:187}, 188: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,15,1,4,1,8,1,3,0,8,1,4,1,41,21,2,28,2147483700,1,60,0,33,0,15,1,5,0,15,1,6,1,8,1,5,0,8,1,6,1,3,10,1,32,0,2,4,61,30,0,15,1,2,0,15,1,3,1,4,62], key:0}}, clause_keys:[0], next_key:1, key:188}, 189: {is_public:false, clauses:{0:{code:[254,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,4,188], key:0}}, clause_keys:[0], next_key:1, key:189}, 190: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,33,2,16,0,0,0,16,0,1,1,8,0,0,0,3,3,3,32,2,9,0,0,9,1,1,2,4,189], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,16,0,1,0,16,0,2,1,8,0,1,0,7,5,1,8,0,0,2,3,11,3,9,0,0,9,1,1,9,2,2,2,4,203], key:1}}, clause_keys:[0, 1], next_key:2, key:190}, 191: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,33,2,20,0,23,1,4,25,0,0,16,0,1,1,8,1,4,0,8,0,1,1,3,189,3,32,2,9,0,0,9,1,1,2,4,191], key:0}, 1:{code:[30,0,17,0,0,15,1,2,1,5], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,0,1000000,45,0,1,0,1], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:191, index: 2}, 192: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,33,0,16,0,2,0,16,0,3,1,16,0,1,2,20,3,25,0,2,25,0,1,8,0,2,0,3,3,4,9,3,0,8,0,2,1,3,190,4,9,1,0,9,2,1,3,191,3,32,0,2,5], key:0}, 1:{code:[29,2,1,60,0,33,0,15,1,5,0,15,1,6,1,15,1,7,2,16,1,7,3,8,1,5,0,3,3,1,32,0,2,5], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,60,4,33,4,15,1,8,0,15,1,9,1,16,0,2,2,16,0,3,3,8,1,8,0,8,1,9,1,8,0,0,2,8,0,1,3,3,193,5,32,4,9,0,0,9,1,1,9,2,2,9,3,3,2,4,192], key:2}, 3:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,16,0,1,0,16,0,2,1,16,0,3,2,16,0,4,3,8,0,1,0,7,7,1,8,0,0,2,3,11,5,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,194], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:192}, 193: {is_public:false, clauses:{0:{code:[28,1,19,196,0,23,1,4,15,1,5,1,17,126,2,16,1,5,3,31,5], key:0}, 1:{code:[29,2,19,197,0,23,1,4,23,1,5,15,1,6,1,16,1,5,2,19,198,3,25,1,6,24,1,4,31,5], key:1}, 2:{code:[29,3,19,199,0,23,1,4,23,1,5,23,1,6,15,1,7,1,19,195,2,24,1,5,24,1,6,19,198,3,25,1,7,24,1,4,31,5], key:2}, 3:{code:[29,4,19,200,0,23,1,4,23,1,5,23,1,6,15,1,7,1,19,195,2,24,1,5,24,1,6,19,198,3,25,1,7,24,1,4,31,5], key:3}, 4:{code:[29,5,19,201,0,23,1,4,23,1,5,23,1,6,15,1,7,1,19,195,2,24,1,5,24,1,6,19,198,3,25,1,7,24,1,4,31,5], key:4}, 5:{code:[30,0,19,202,0,23,1,4,23,1,5,23,1,6,15,1,7,1,19,195,2,24,1,5,24,1,6,19,198,3,25,1,7,24,1,4,31,5], key:5}, 6:{code:[254,0,44,0,1000000,1000000,1000000,1000000,2147483657,46,0,6,196,0,197,1,199,2,200,3,201,4,202,5], key:6}}, clause_keys:[0, 1, 2, 3, 4, 5, 6], next_key:7, key:193, index: 6}, 194: {is_public:false, clauses:{0:{code:[28,1,21,0,0,15,1,5,1,15,1,6,2,15,1,7,3,16,1,7,4,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,33,5,16,0,6,0,16,0,1,1,16,0,2,2,16,0,8,3,16,0,4,4,8,0,6,0,8,0,1,1,8,0,7,2,3,12,9,9,7,0,8,0,2,1,9,8,2,8,0,3,3,3,192,9,8,0,0,0,12,195,1,25,0,6,27,1,3,34,7,32,5,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,194], key:1}}, clause_keys:[0, 1], next_key:2, key:194}, 199: {is_public:false, clauses:{0:{code:[254,0,15,1,6,0,15,1,7,1,15,1,8,2,8,1,6,0,8,1,7,1,8,1,8,2,4,201], key:0}}, clause_keys:[0], next_key:1, key:199}, 200: {is_public:false, clauses:{0:{code:[254,0,15,1,6,0,15,1,7,1,15,1,8,2,8,1,6,0,8,1,7,1,8,1,8,2,4,202], key:0}}, clause_keys:[0], next_key:1, key:200}, 201: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,15,1,6,0,15,1,7,1,16,0,1,2,8,1,6,0,8,1,7,1,8,0,0,2,3,202,2,9,0,0,9,1,1,2,4,247], key:0}}, clause_keys:[0], next_key:1, key:201}, 202: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,33,4,16,0,7,0,16,0,8,1,16,0,0,2,8,0,8,0,8,0,7,1,10,0,2,8,0,9,3,3,192,10,8,0,9,0,10,0,1,3,188,10,32,4,8,0,3,0,13,1,26,499,25,0,9,3,13,10,8,0,3,0,10,499,1,8,0,6,2,3,11,9,12,195,0,25,0,3,25,0,7,9,8,1,3,245,9,8,0,3,0,9,6,1,10,0,2,8,0,5,3,3,248,7,9,5,0,8,0,2,1,3,249,6,32,4,9,2,0,9,3,1,8,0,1,2,3,250,4,9,0,0,9,1,1,2,4,9], key:0}, 1:{code:[30,0,1,60,0,15,1,5,0,15,1,6,1,16,0,0,2,12,244,0,25,1,5,8,1,6,1,3,245,1,10,0,0,8,0,0,1,3,246,1,9,0,0,10,0,1,2,4,188], key:1}}, clause_keys:[0, 1], next_key:2, key:202}, 203: {is_public:false, clauses:{0:{code:[28,1,21,0,0,15,1,3,1,15,1,4,2,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,33,3,16,0,4,0,16,0,1,1,16,0,2,2,8,0,4,0,8,0,1,1,8,0,5,2,3,12,6,9,5,0,8,0,2,1,3,190,6,8,0,0,0,12,195,1,25,0,4,27,1,3,34,5,32,3,9,0,0,9,1,1,9,2,2,2,4,203], key:1}}, clause_keys:[0, 1], next_key:2, key:203}, 204: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,0,0,16,0,1,1,8,0,2,0,3,574,3,8,0,0,0,9,2,1,3,226,3,9,1,0,8,0,0,1,3,575,2,9,0,0,2,41,51,1,28,2147483730,1,60,0,33,0,15,1,4,0,8,1,4,0,10,2,1,3,259,1,32,0,12,576,0,26,2,2,4,206,30,0,15,1,1,0,4,62], key:0}}, clause_keys:[0], next_key:1, key:204}, 205: {is_public:false, clauses:{0:{code:[254,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,4,204], key:0}}, clause_keys:[0], next_key:1, key:205}, 206: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,15,1,3,0,8,1,3,0,8,0,1,1,3,578,2,8,0,0,0,7,2,1,3,577,2,9,0,0,9,1,1,2,41,42,2,28,2147483729,1,60,0,60,1,33,1,16,0,0,0,15,1,5,1,8,0,0,0,8,1,5,1,3,9,2,32,1,12,579,0,25,0,0,2,4,143,30,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,4,106], key:0}}, clause_keys:[0], next_key:1, key:206}, 207: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,8,1,3,0,4,206], key:0}}, clause_keys:[0], next_key:1, key:207}, 209: {is_public:false, clauses:{0:{code:[254,0,1,60,0,16,0,0,0,8,0,0,0,3,225,1,9,0,0,2,4,263], key:0}}, clause_keys:[0], next_key:1, key:209}, 210: {is_public:false, clauses:{0:{code:[254,0,15,1,2,0,12,208,0,26,370,25,1,2,4,209], key:0}}, clause_keys:[0], next_key:1, key:210}, 211: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,19,208,0,23,1,1,23,1,2,19,208,2,25,0,0,25,0,1,31,12,208,0,25,0,0,25,0,1,2,4,211], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,19,208,0,25,0,0,23,1,1,19,212,1,25,0,1,25,0,2,31,12,208,1,25,0,0,25,0,1,12,208,2,25,0,0,25,0,2,12,212,0,24,1,1,24,1,2,2,4,211], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,19,212,0,23,1,1,25,0,2,19,208,1,23,1,2,23,1,3,19,208,3,25,0,0,25,0,1,31,12,208,1,25,0,0,25,0,1,12,212,0,24,1,1,25,0,2,2,4,211], key:2}, 3:{code:[29,4,1,60,0,60,1,60,2,60,3,19,212,0,23,1,1,23,1,2,19,208,1,25,0,0,25,0,1,19,208,2,23,1,3,23,1,4,19,208,4,25,0,2,25,0,3,31,12,208,1,25,0,0,25,0,1,12,208,2,25,0,2,25,0,3,12,212,0,24,1,1,24,1,2,2,4,211], key:3}, 4:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,19,212,0,23,1,1,23,1,2,19,208,1,25,0,0,25,0,1,19,208,2,25,0,4,25,0,3,31,9,3,0,9,4,1,8,0,2,2,3,213,5,12,208,1,25,0,0,25,0,1,12,212,0,24,1,1,25,0,2,2,4,214], key:4}, 5:{code:[254,0,15,1,2,0,8,1,2,0,4,214], key:5}, 6:{code:[28,2147483674,44,0,1000000,1000000,1000000,1000000,2147483657,46,0,2,208,2147483664,212,2147483668,71,0,73,1,71,2,72,3,73,4,30,0,74,5], key:6}}, clause_keys:[0, 1, 2, 3, 4, 5, 6], next_key:7, key:211, index: 6}, 213: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,19,229,0,25,0,3,25,0,0,16,0,1,1,19,229,2,25,0,4,25,0,2,31,9,3,0,8,0,1,1,9,4,2,3,213,5,9,0,0,9,1,1,9,2,2,2,4,213], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,19,610,0,25,0,0,16,0,1,1,19,610,2,25,0,2,31,9,0,0,9,1,1,9,2,2,2,4,213], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,19,617,0,25,0,5,25,0,3,23,1,3,25,0,0,16,0,1,1,19,617,2,25,0,6,25,0,4,24,1,3,25,0,2,31,9,5,0,8,0,1,1,9,6,2,3,213,7,9,3,0,8,0,1,1,9,4,2,3,213,5,9,0,0,9,1,1,9,2,2,2,4,213], key:2}, 3:{code:[29,4,1,60,0,60,1,60,2,60,3,60,4,19,629,0,25,0,3,23,1,3,25,0,0,16,0,1,1,19,629,2,25,0,4,24,1,3,25,0,2,31,9,3,0,8,0,1,1,9,4,2,3,213,5,9,0,0,9,1,1,9,2,2,2,4,213], key:3}, 4:{code:[29,5,1,60,0,60,1,60,2,60,3,60,4,19,630,0,25,0,3,25,0,0,16,0,1,1,19,630,2,25,0,4,25,0,2,31,9,3,0,8,0,1,1,9,4,2,3,213,5,9,0,0,9,1,1,9,2,2,2,4,213], key:4}, 5:{code:[29,6,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,19,631,0,23,1,3,25,0,0,19,632,3,25,0,5,25,0,3,16,0,1,1,19,631,2,23,1,4,25,0,2,19,632,4,25,0,6,25,0,4,31,9,5,0,8,0,1,1,9,6,2,3,213,7,9,3,0,8,0,1,1,9,4,2,3,213,5,9,0,0,9,1,1,9,2,2,2,4,213], key:5}, 6:{code:[29,7,1,60,0,60,1,60,2,60,3,60,4,19,632,0,25,0,3,25,0,0,16,0,1,1,19,632,2,25,0,4,25,0,2,31,9,3,0,8,0,1,1,9,4,2,3,213,5,9,0,0,9,1,1,9,2,2,2,4,213], key:6}, 7:{code:[29,8,1,60,0,60,1,60,2,60,3,60,4,19,631,0,25,0,3,25,0,0,16,0,1,1,19,631,2,25,0,4,25,0,2,31,9,3,0,8,0,1,1,9,4,2,3,213,5,9,0,0,9,1,1,9,2,2,2,4,213], key:7}, 8:{code:[29,9,1,60,0,60,1,60,2,19,196,0,25,0,0,16,0,1,1,19,196,2,25,0,2,31,9,0,0,9,1,1,9,2,2,2,4,213], key:8}, 9:{code:[30,0,1,60,0,60,1,60,2,19,208,0,25,0,1,25,0,0,15,1,3,1,16,0,2,2,31,9,0,0,9,1,1,9,2,2,2,4,213], key:9}, 10:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,15,1,5,0,16,0,5,1,16,0,0,2,8,1,5,0,13,1,25,0,3,25,0,2,3,13,6,8,0,2,0,8,0,4,1,3,494,6,9,3,0,9,4,1,9,5,2,8,0,1,3,3,216,6,9,0,0,13,1,25,0,1,25,0,2,2,4,13], key:10}, 11:{code:[28,2147483682,44,0,1000000,1000000,1000000,1000000,2147483657,46,0,9,196,8,208,9,229,0,610,1,617,2,629,3,630,4,631,2147483678,632,6,71,5,73,7,30,0,74,10], key:11}}, clause_keys:[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11], next_key:12, key:213, index: 11}, 214: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,19,212,0,23,1,1,25,0,3,19,208,1,25,0,6,25,0,7,31,9,7,0,13,1,25,0,5,25,0,4,3,13,8,8,0,4,0,8,0,1,1,3,215,7,9,5,0,8,0,1,1,9,6,2,8,0,0,3,3,216,7,8,0,2,0,13,1,25,0,0,25,0,4,3,13,5,12,217,0,25,0,0,25,0,1,9,2,1,9,3,2,2,4,93], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,19,212,0,25,0,7,25,0,3,31,9,7,0,13,1,25,0,5,25,0,4,3,13,8,8,0,4,0,8,0,1,1,3,215,7,8,0,6,0,3,218,7,9,5,0,8,0,1,1,9,6,2,8,0,0,3,3,216,7,8,0,2,0,13,1,25,0,0,25,0,4,3,13,5,12,217,0,25,0,0,25,0,1,9,2,1,9,3,2,2,4,93], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,19,208,0,25,0,5,25,0,6,31,9,6,0,13,1,25,0,4,25,0,3,3,13,7,8,0,3,0,8,0,1,1,3,215,6,9,4,0,8,0,1,1,9,5,2,8,0,0,3,3,216,6,8,0,2,0,13,1,25,0,0,25,0,3,3,13,4,12,217,0,25,0,0,25,0,1,9,2,1,10,128,2,2,4,93], key:2}, 3:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,16,0,6,0,31,9,6,0,13,1,25,0,4,25,0,3,3,13,7,8,0,3,0,8,0,1,1,3,215,6,8,0,5,0,3,218,6,9,4,0,8,0,1,1,9,5,2,8,0,0,3,3,216,6,8,0,2,0,13,1,25,0,0,25,0,3,3,13,4,12,217,0,25,0,0,25,0,1,9,2,1,10,128,2,2,4,93], key:3}, 4:{code:[28,2147483668,44,0,1000000,1000000,1000000,1000000,2147483657,46,0,2,208,2,212,2147483664,71,0,73,1,30,0,74,3], key:4}}, clause_keys:[0, 1, 2, 3, 4], next_key:5, key:214, index: 4}, 215: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,8,1,5,0,14,0,1,8,1,6,2,4,306], key:0}}, clause_keys:[0], next_key:1, key:215}, 216: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,16,0,2,0,16,0,4,1,16,0,5,2,16,0,3,3,8,0,2,0,8,0,1,1,3,24,6,13,0,27,58,26,0,13,1,25,0,0,26,0,3,9,6,7,10,0,9,0,1,7,11,2,9,1,3,9,2,4,9,3,5,7,12,6,9,4,7,9,5,8,7,13,9,2,41,92,10,28,2147483829,1,60,0,60,1,60,2,60,3,33,2,16,0,3,0,15,1,14,1,15,1,15,2,15,1,16,3,16,0,0,4,16,0,1,5,15,1,17,6,15,1,18,7,15,1,19,8,15,1,20,9,8,0,3,0,13,1,25,1,14,25,1,15,8,1,16,2,3,303,4,9,3,0,3,588,4,32,2,9,0,0,9,1,1,2,4,9,30,0,15,1,12,0,15,1,13,1,15,1,14,2,15,1,15,3,15,1,16,4,15,1,17,5,15,1,18,6,15,1,19,7,15,1,20,8,15,1,21,9,8,1,16,0,8,1,19,1,8,1,20,2,8,1,21,3,8,1,13,4,8,1,15,5,8,1,17,6,41,254,7,28,2147483981,1,60,0,60,1,60,2,60,3,60,4,33,4,15,1,12,0,15,1,13,1,15,1,14,2,16,0,0,3,16,0,1,4,16,0,2,5,16,0,3,6,8,1,12,0,8,1,13,1,8,1,14,2,8,0,0,3,3,587,5,32,4,9,0,0,9,1,1,9,2,2,9,3,3,2,4,487,30,0,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,15,1,15,4,15,1,16,5,15,1,17,6,8,1,13,0,8,1,15,1,8,1,16,2,8,1,17,3,4,487], key:0}}, clause_keys:[0], next_key:1, key:216}, 218: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,8,1,3,0,7,4,1,4,577], key:0}}, clause_keys:[0], next_key:1, key:218}, 219: {is_public:false, clauses:{0:{code:[254,0,15,1,1,0,4,62], key:0}}, clause_keys:[0], next_key:1, key:219}, 220: {is_public:false, clauses:{0:{code:[254,0,15,1,1,0,5], key:0}}, clause_keys:[0], next_key:1, key:220}, 221: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,15,1,3,0,8,1,3,0,8,0,1,1,3,715,2,9,1,0,8,0,0,1,3,716,2,9,0,0,3,717,1,2,4,713], key:0}}, clause_keys:[0], next_key:1, key:221}, 222: {is_public:false, clauses:{0:{code:[28,1,20,0,23,1,2,23,1,3,13,4,24,1,2,24,1,3,12,221,0,24,1,4,4,222], key:0}, 1:{code:[30,0,1,60,0,60,1,19,208,0,25,0,0,25,0,1,31,9,0,0,9,1,1,2,4,223], key:1}, 2:{code:[254,0,15,1,3,0,10,368,0,8,1,3,1,4,223], key:2}, 3:{code:[28,2147483662,44,0,1000000,1000000,1000000,0,2147483657,46,0,1,208,1,30,0,74,2], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:222, index: 3}, 223: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,15,1,2,0,19,208,1,25,0,0,25,0,1,31,9,0,0,9,1,1,2,4,223], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,33,3,16,0,0,0,16,0,2,1,8,0,2,0,8,0,1,1,3,63,4,9,0,0,9,1,1,9,2,2,8,0,3,3,7,6,4,7,7,5,2,41,55,6,28,2147483781,1,60,0,60,1,60,2,60,3,33,3,15,1,9,0,16,0,0,1,16,0,2,2,16,0,1,3,15,1,10,4,15,1,11,5,8,1,9,0,10,368,1,3,9,4,32,3,12,224,1,25,0,0,12,212,0,24,1,1,25,0,2,3,225,3,32,1,9,0,0,3,97,1,2,4,62,30,0,1,60,0,60,1,60,2,60,3,60,4,15,1,8,0,16,0,0,1,16,0,4,2,16,0,1,3,16,0,2,4,16,0,3,5,8,1,8,0,10,409,1,3,226,5,12,224,1,25,0,0,12,212,0,24,1,1,25,0,4,3,225,5,9,2,0,9,3,1,3,227,4,32,1,9,0,0,3,97,1,2,4,62], key:1}}, clause_keys:[0, 1], next_key:2, key:223}, 225: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,19,208,0,25,0,0,23,1,1,19,593,1,25,0,1,31,12,208,1,25,0,0,25,0,1,12,593,0,24,1,1,2,4,225], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,19,593,0,25,0,9,31,8,0,9,0,10,1049,1,8,0,12,2,8,0,1,3,10,0,4,8,0,11,5,3,594,13,8,0,11,0,3,595,13,9,12,0,8,0,4,1,3,596,13,12,597,0,25,0,11,8,0,4,1,8,0,5,2,3,598,12,8,0,5,0,8,0,4,1,8,0,6,2,3,599,12,8,0,6,0,9,11,1,8,0,5,2,8,0,0,3,8,0,8,4,12,600,7,25,0,10,13,5,24,1,7,26,0,8,0,7,6,3,601,12,9,9,0,9,10,1,3,602,11,8,0,2,0,3,218,9,9,4,0,8,0,2,1,9,5,2,9,6,3,9,7,4,7,8,5,9,8,6,8,0,3,7,3,603,9,9,1,0,9,2,1,9,3,2,10,0,3,3,604,4,3,86,1,9,0,0,14,2,1,2,4,389], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,19,208,0,25,0,0,23,1,1,19,212,1,25,0,1,25,0,2,31,12,208,1,25,0,0,25,0,1,12,212,0,24,1,1,25,0,2,2,4,225], key:2}, 3:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,33,0,19,212,0,25,0,1,25,0,3,31,8,0,3,0,10,1049,1,8,0,14,2,8,0,5,3,10,0,4,8,0,13,5,3,594,15,8,0,13,0,3,595,15,9,14,0,8,0,7,1,3,596,15,12,195,0,25,0,1,25,0,13,8,0,7,1,8,0,8,2,3,598,14,8,0,8,0,8,0,7,1,8,0,9,2,3,599,14,8,0,3,0,8,0,12,1,3,602,14,8,0,1,0,9,12,1,9,13,2,8,0,8,3,8,0,9,4,8,0,10,5,8,0,4,6,8,0,11,7,3,605,14,8,0,2,0,3,218,12,9,7,0,8,0,2,1,9,8,2,9,9,3,9,10,4,7,8,5,9,11,6,8,0,6,7,3,603,12,9,5,0,8,0,2,1,9,6,2,10,0,3,3,604,7,3,86,5,32,0,9,4,0,14,2,1,3,389,5,8,0,0,0,9,1,1,7,6,2,7,7,3,9,2,4,9,3,5,2,41,255,6,28,2147483979,1,60,0,60,1,60,2,60,3,33,3,15,1,9,0,16,0,1,1,15,1,10,2,15,1,11,3,16,0,0,4,16,0,2,5,8,0,1,0,12,537,1,25,1,10,25,1,11,3,9,4,32,3,12,208,1,25,0,0,25,0,1,12,212,0,24,1,1,25,0,2,2,4,219,30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,15,1,10,4,15,1,11,5,4,62], key:3}, 4:{code:[254,0,1,60,0,60,1,16,0,0,0,8,0,0,0,14,0,1,10,1053,2,10,0,3,10,613,4,7,9,5,8,0,1,6,13,7,26,793,26,0,3,605,2,3,86,2,9,1,0,14,2,1,3,389,2,7,7,0,9,0,1,7,8,2,7,9,3,7,10,4,7,11,5,7,12,6,2,41,80,7,28,2147483812,1,60,0,60,1,60,2,33,2,15,1,10,0,16,0,1,1,15,1,11,2,15,1,12,3,15,1,13,4,15,1,14,5,16,0,0,6,8,0,1,0,8,1,11,1,8,1,12,2,8,1,13,3,8,1,14,4,40,196,5,3,32,2,8,0,0,0,3,218,2,12,208,0,25,0,0,25,0,1,2,4,219,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,15,1,12,5,15,1,13,6,4,62,28,2147483881,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,7,0,12,537,1,25,1,8,25,1,9,4,9,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,7,0,12,606,1,25,1,10,25,1,11,4,9], key:4}, 5:{code:[28,2147483670,44,0,1000000,1000000,1000000,1000000,2147483657,46,0,3,208,2147483666,212,3,593,1,71,0,73,2,30,0,74,4], key:5}}, clause_keys:[0, 1, 2, 3, 4, 5], next_key:6, key:225, index: 5}, 226: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,33,0,16,0,1,0,16,0,2,1,8,0,3,0,7,4,1,40,62,2,4,10,1000,0,12,195,3,25,0,1,25,0,2,13,1,24,1,3,25,0,3,7,4,2,3,146,4,32,0,2,5,28,2147483749,1,60,0,60,1,33,1,15,1,6,0,16,0,0,1,10,1000,0,8,1,6,1,8,0,0,2,3,148,2,32,1,9,0,0,2,4,149,30,0,15,1,4,0,15,1,5,1,8,1,4,0,10,0,1,4,9], key:0}}, clause_keys:[0], next_key:1, key:226}, 227: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,33,0,15,1,5,0,15,1,6,1,10,1000,0,12,195,7,25,1,5,25,1,6,13,1,24,1,7,25,0,1,8,0,2,2,3,148,3,9,2,0,3,149,3,10,1000,0,9,1,1,7,3,2,3,146,2,32,0,2,5], key:0}}, clause_keys:[0], next_key:1, key:227}, 228: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,19,208,0,25,0,0,23,1,1,19,217,1,25,0,1,25,0,2,31,12,217,1,25,0,1,25,0,2,12,208,0,25,0,0,24,1,1,2,4,100], key:0}, 1:{code:[29,2,19,208,0,23,1,1,26,0,5], key:1}, 2:{code:[29,3,17,0,0,5], key:2}, 3:{code:[29,4,1,60,0,60,1,60,2,19,208,0,25,0,0,23,1,1,20,1,25,0,2,25,0,1,31,12,208,0,25,0,0,25,0,2,3,228,3,12,208,0,25,0,0,25,0,1,2,4,228], key:3}, 4:{code:[30,0,1,60,0,60,1,19,208,0,25,0,0,23,1,2,19,229,2,23,1,3,25,0,1,12,208,0,25,0,0,24,1,3,3,228,2,12,208,0,25,0,0,25,0,1,2,4,228], key:4}, 5:{code:[254,0,44,0,2147483657,1000000,1000000,1000000,2147483662,45,0,1,0,2,46,0,1,208,2147483667,71,0,72,1,72,3,73,4], key:5}}, clause_keys:[0, 1, 2, 3, 4, 5], next_key:6, key:228, index: 5}, 230: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,16,0,3,0,16,0,1,1,8,0,3,0,8,0,2,1,3,24,4,7,5,0,9,2,1,8,0,0,2,9,3,3,7,6,4,40,59,5,4,9,0,0,9,1,1,2,4,382,28,2147483780,1,60,0,60,1,60,2,33,2,15,1,9,0,15,1,10,1,16,0,0,2,16,0,1,3,15,1,11,4,8,1,9,0,13,13,27,108,26,0,13,12,27,112,24,1,13,13,1,27,46,24,1,12,8,1,10,2,3,303,3,32,2,9,0,0,9,1,1,2,4,9,30,0,1,60,0,60,1,15,1,8,0,15,1,9,1,16,0,0,2,15,1,10,3,16,0,1,4,8,1,9,0,13,12,27,108,26,0,13,11,27,112,24,1,12,13,1,27,46,24,1,11,8,0,1,2,3,303,2,9,0,0,9,1,1,2,4,24], key:0}}, clause_keys:[0], next_key:1, key:230}, 231: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:231}, 232: {is_public:false, clauses:{0:{code:[254,0,1,60,0,15,1,3,0,8,1,3,0,8,0,0,1,3,230,1,9,0,0,2,41,27,1,28,2147483698,1,60,0,33,0,15,1,4,0,8,1,4,0,3,231,1,32,0,2,4,62,30,0,15,1,2,0,13,0,25,1,2,26,0,4,221], key:0}}, clause_keys:[0], next_key:1, key:232}, 233: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,1,0,16,0,2,1,8,0,0,0,3,43,3,9,0,0,9,1,1,9,2,2,2,4,79], key:0}}, clause_keys:[0], next_key:1, key:233}, 234: {is_public:false, clauses:{0:{code:[254,0,15,1,1,0,5], key:0}}, clause_keys:[0], next_key:1, key:234}, 235: {is_public:false, clauses:{0:{code:[28,1,1,60,0,16,0,0,0,15,1,3,1,8,1,3,0,3,142,1,9,0,0,2,4,140], key:0}, 1:{code:[30,0,1,15,1,3,0,15,1,4,1,8,1,4,0,3,140,0,2,4,61], key:1}}, clause_keys:[0, 1], next_key:2, key:235}, 237: {is_public:false, clauses:{0:{code:[254,0,14,0,0,4,15], key:0}}, clause_keys:[0], next_key:1, key:237}, 239: {is_public:false, clauses:{0:{code:[254,0,5], key:0}}, clause_keys:[0], next_key:1, key:239}, 240: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,1,0,10,477,0,13,1,25,0,1,26,0,3,233,3,8,0,2,0,3,138,3,9,2,0,8,0,1,1,8,0,0,2,7,4,3,40,64,4,3,9,0,0,9,1,1,7,3,2,2,41,836,3,28,2147483830,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,16,0,6,0,16,0,4,1,16,0,3,2,16,0,5,3,13,7,25,0,5,26,0,13,6,25,0,4,24,1,7,13,0,25,0,3,24,1,6,8,0,2,1,3,135,7,40,436,0,7,8,0,0,0,3,139,7,8,0,4,0,40,336,1,7,43,0,1,9,6,0,8,0,0,1,3,235,7,9,0,0,8,0,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,41,348,6,29,2147483915,1,60,0,60,1,60,2,60,3,60,4,33,4,15,1,6,0,16,0,1,1,16,0,0,2,16,0,2,3,8,1,6,0,3,140,5,8,0,3,0,3,144,5,32,4,8,0,0,0,12,236,1,25,0,3,3,9,4,9,0,0,9,1,1,7,6,2,7,7,3,7,8,4,9,2,5,40,492,6,3,2,4,141,30,0,1,60,0,60,1,60,2,60,3,33,0,15,1,7,0,16,0,2,1,16,0,1,2,16,0,3,3,8,0,1,0,10,126,1,3,9,4,3,137,4,9,1,0,9,2,1,7,6,2,7,7,3,7,8,4,9,3,5,40,492,6,4,32,0,2,4,61,254,0,15,1,2,0,8,1,2,0,4,222,28,2147484080,1,60,0,60,1,60,2,60,3,15,1,8,0,15,1,9,1,16,0,3,2,16,0,0,3,16,0,1,4,16,0,2,5,8,1,8,0,8,1,9,1,3,9,4,8,0,0,0,10,479,1,3,9,4,9,3,0,3,136,4,9,0,0,9,1,1,7,6,2,7,7,3,7,8,4,9,2,5,2,41,492,6,30,0,4,62,254,0,1,60,0,60,1,60,2,16,0,0,0,16,0,1,1,16,0,2,2,8,0,0,0,10,480,1,3,9,3,9,0,0,9,1,1,7,6,2,7,7,3,7,8,4,9,2,5,40,492,6,3,2,4,237,28,2147484198,1,60,0,60,1,33,1,15,1,9,0,16,0,0,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,9,0,10,126,1,3,10,2,32,1,10,481,0,13,1,25,0,0,26,0,2,4,233,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,8,0,8,1,9,1,8,1,11,2,8,1,12,3,8,1,13,4,41,599,5,28,2147484301,1,60,0,60,1,33,1,15,1,8,0,16,0,0,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,8,0,10,479,1,3,10,2,32,1,10,482,0,13,1,25,0,0,26,0,2,4,233,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,7,0,8,1,8,1,8,1,10,2,8,1,11,3,41,694,4,28,2147484392,1,60,0,60,1,33,1,15,1,7,0,16,0,0,1,15,1,8,2,15,1,9,3,8,1,7,0,10,480,1,3,10,2,32,1,10,483,0,13,1,25,0,0,26,0,2,4,233,30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,8,1,6,0,8,1,9,1,8,1,7,2,41,777,3,254,0,1,60,0,60,1,60,2,33,2,15,1,6,0,16,0,1,1,16,0,0,2,8,1,6,0,12,238,1,25,0,1,3,9,3,32,2,10,485,0,13,2,25,0,1,26,0,13,1,25,0,0,24,1,2,2,4,233,28,2147484527,1,60,0,60,1,33,1,15,1,5,0,16,0,0,1,15,1,6,2,8,1,5,0,3,3,2,32,1,10,486,0,13,1,25,0,0,26,0,2,4,233,30,0,15,1,5,0,15,1,6,1,15,1,7,2,41,896,0,254,0,1,60,0,33,0,3,239,1,32,0,2,4,62], key:0}}, clause_keys:[0], next_key:1, key:240}, 241: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,16,0,4,0,8,0,4,0,8,0,1,1,8,0,2,2,3,11,5,10,477,0,12,217,2,25,0,1,25,0,2,13,1,24,1,2,26,0,3,233,5,8,0,3,0,3,138,5,9,3,0,9,4,1,8,0,0,2,8,0,1,3,8,0,2,4,7,6,5,40,102,6,5,9,0,0,9,1,1,9,2,2,7,4,3,2,41,1061,4,28,2147483902,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,16,0,8,0,16,0,4,1,16,0,3,2,16,0,5,3,16,0,6,4,16,0,7,5,13,11,25,0,7,26,0,13,10,25,0,6,24,1,11,13,9,25,0,5,24,1,10,13,8,25,0,4,24,1,9,13,0,25,0,3,24,1,8,8,0,2,1,3,135,9,40,562,0,9,8,0,0,0,3,139,9,8,0,4,0,40,444,1,9,43,0,1,9,8,0,8,0,0,1,3,235,9,9,0,0,8,0,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,2,41,456,8,29,2147484005,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,15,1,8,0,16,0,1,1,16,0,0,2,16,0,2,3,16,0,3,4,16,0,4,5,8,1,8,0,3,140,7,8,0,5,0,3,144,7,32,6,8,0,0,0,12,236,1,25,0,5,3,9,6,9,0,0,9,1,1,7,8,2,9,2,3,9,3,4,7,9,5,7,10,6,9,4,7,40,636,8,5,2,4,141,30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,33,0,15,1,9,0,16,0,2,1,16,0,1,2,16,0,3,3,16,0,4,4,16,0,5,5,8,0,1,0,10,126,1,3,9,6,3,137,6,9,1,0,9,2,1,7,8,2,9,3,3,9,4,4,7,9,5,7,10,6,9,5,7,40,636,8,6,32,0,2,4,61,254,0,15,1,2,0,8,1,2,0,4,222,28,2147484206,1,60,0,60,1,60,2,60,3,60,4,60,5,15,1,10,0,15,1,11,1,16,0,5,2,16,0,0,3,16,0,1,4,16,0,2,5,16,0,3,6,16,0,4,7,8,1,10,0,8,1,11,1,3,9,6,8,0,0,0,10,479,1,3,9,6,9,5,0,3,136,6,9,0,0,9,1,1,7,8,2,9,2,3,9,3,4,7,9,5,7,10,6,9,4,7,2,41,636,8,30,0,4,62,254,0,1,60,0,60,1,60,2,60,3,60,4,16,0,0,0,16,0,1,1,16,0,2,2,16,0,3,3,16,0,4,4,8,0,0,0,10,480,1,3,9,5,9,0,0,9,1,1,7,8,2,9,2,3,9,3,4,7,9,5,7,10,6,9,4,7,40,636,8,5,2,4,237,28,2147484350,1,60,0,60,1,33,1,15,1,11,0,16,0,0,1,15,1,12,2,15,1,13,3,15,1,14,4,15,1,15,5,15,1,16,6,15,1,17,7,8,1,11,0,10,126,1,3,10,2,32,1,10,481,0,13,1,25,0,0,26,0,2,4,233,30,0,15,1,10,0,15,1,11,1,15,1,12,2,15,1,13,3,15,1,14,4,15,1,15,5,15,1,16,6,15,1,17,7,8,1,10,0,8,1,13,1,8,1,14,2,8,1,15,3,8,1,16,4,8,1,17,5,41,763,6,28,2147484480,1,60,0,60,1,60,2,33,2,15,1,9,0,16,0,0,1,16,0,1,2,15,1,10,3,15,1,11,4,15,1,12,5,8,1,9,0,10,479,1,3,10,3,32,2,10,482,0,12,217,2,25,0,0,25,0,1,13,1,24,1,2,26,0,2,4,233,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,8,0,8,1,9,1,8,1,10,2,8,1,12,3,8,1,13,4,41,881,5,28,2147484594,1,60,0,60,1,60,2,33,2,15,1,8,0,16,0,0,1,16,0,1,2,15,1,9,3,15,1,10,4,8,1,8,0,10,480,1,3,10,3,32,2,10,483,0,12,217,2,25,0,0,25,0,1,13,1,24,1,2,26,0,2,4,233,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,7,0,8,1,11,1,8,1,8,2,8,1,9,3,41,987,4,254,0,1,60,0,60,1,60,2,60,3,33,3,15,1,7,0,16,0,2,1,16,0,0,2,16,0,1,3,8,1,7,0,12,238,1,25,0,2,3,9,4,32,3,10,485,0,12,217,2,25,0,0,25,0,1,13,3,25,0,2,26,0,13,1,24,1,2,24,1,3,2,4,233,28,2147484767,1,60,0,60,1,60,2,33,2,15,1,6,0,16,0,0,1,16,0,1,2,15,1,7,3,8,1,6,0,3,3,3,32,2,10,486,0,12,217,2,25,0,0,25,0,1,13,1,24,1,2,26,0,2,4,233,30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,41,1140,0,254,0,1,60,0,33,0,3,239,1,32,0,2,4,62], key:0}}, clause_keys:[0], next_key:1, key:241}, 242: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,15,1,6,0,15,1,7,1,16,0,0,2,16,0,1,3,12,244,0,25,1,6,8,1,7,1,3,245,2,9,0,0,9,1,1,2,4,246], key:0}}, clause_keys:[0], next_key:1, key:242}, 243: {is_public:false, clauses:{0:{code:[254,0,15,1,7,0,15,1,8,1,15,1,9,2,8,1,7,0,8,1,8,1,10,0,2,8,1,9,3,4,242], key:0}}, clause_keys:[0], next_key:1, key:243}, 245: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,16,0,0,0,15,1,4,1,8,1,4,0,7,5,1,8,0,1,2,3,251,2,10,499,0,10,423,1,7,3,2,3,146,2,9,1,0,3,222,2,10,499,0,9,0,1,7,3,2,3,146,1,2,4,61], key:0}, 1:{code:[30,0,15,1,2,0,15,1,3,1,5], key:1}}, clause_keys:[0, 1], next_key:2, key:245}, 246: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,33,3,16,0,1,0,16,0,2,1,10,499,0,8,0,0,1,8,0,4,2,3,148,5,9,4,0,3,149,5,32,3,9,0,0,9,1,1,9,2,2,2,4,252], key:0}}, clause_keys:[0], next_key:1, key:246}, 247: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,3,296,1,32,0,2,5], key:0}, 1:{code:[30,0,15,1,5,0,15,1,6,1,8,1,5,0,8,1,6,1,10,0,2,4,297], key:1}}, clause_keys:[0, 1], next_key:2, key:247}, 248: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,5,16,0,1,0,16,0,2,1,16,0,3,2,16,0,4,3,10,499,0,8,0,0,1,8,0,6,2,3,148,7,9,6,0,3,149,7,32,5,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,253], key:0}}, clause_keys:[0], next_key:1, key:248}, 249: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,3,300,1,32,0,2,5], key:0}, 1:{code:[30,0,15,1,5,0,15,1,6,1,8,1,5,0,8,1,6,1,10,0,2,4,301], key:1}}, clause_keys:[0, 1], next_key:2, key:249}, 250: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,20,0,23,1,7,23,1,8,19,195,7,25,0,1,25,0,2,16,0,4,1,16,0,5,2,8,1,8,0,8,0,1,1,8,0,3,2,8,0,0,3,3,256,6,9,0,0,9,1,1,13,2,25,0,2,25,0,3,9,4,3,9,5,4,2,4,257], key:0}}, clause_keys:[0], next_key:1, key:250}, 251: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,19,197,0,23,1,3,25,0,0,20,1,24,1,3,25,0,1,16,0,2,2,31,9,0,0,9,1,1,9,2,2,2,4,251], key:0}, 1:{code:[30,0,15,1,3,0,17,0,1,16,1,3,2,5], key:1}}, clause_keys:[0, 1], next_key:2, key:251}, 252: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,17,423,0,16,0,1,1,16,0,0,2,31,9,0,0,9,1,1,2,4,9], key:0}, 1:{code:[30,0,19,244,0,23,1,5,15,1,6,1,15,1,7,2,13,0,24,1,5,25,1,6,8,1,7,1,4,246], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1000000,2147483662,45,0,1,423,0,46,0,1,244,1], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:252, index: 2}, 253: {is_public:false, clauses:{0:{code:[28,1,17,423,0,15,1,5,1,15,1,6,2,15,1,7,3,16,1,7,4,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,19,195,0,25,0,2,25,0,3,16,0,0,1,16,0,1,2,16,0,4,3,16,0,5,4,8,0,1,0,8,0,0,1,8,0,2,2,3,254,7,32,6,9,0,0,9,1,1,12,195,4,25,0,2,25,0,3,13,2,24,1,4,25,0,4,9,5,3,2,4,248], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1000000,2147483662,45,0,1,423,0,46,0,1,195,1], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:253, index: 2}, 254: {is_public:false, clauses:{0:{code:[28,1,21,0,0,15,1,3,1,15,1,4,2,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,33,4,16,0,3,0,16,0,1,1,16,0,2,2,8,0,3,0,8,0,2,1,8,0,5,2,3,12,6,9,5,0,3,255,6,32,4,8,0,0,0,12,195,1,25,0,3,27,1,3,34,4,9,0,0,9,1,1,9,2,2,2,4,254], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,16,0,3,0,16,0,1,1,16,0,2,2,8,0,3,0,8,0,1,1,8,0,4,2,3,12,5,8,0,3,0,8,0,2,1,9,4,2,3,12,5,8,0,0,0,12,195,1,25,0,3,27,1,3,34,4,9,0,0,9,1,1,9,2,2,2,4,254], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:254}, 255: {is_public:false, clauses:{0:{code:[254,0,15,1,2,0,8,1,2,0,41,13,1,28,2147483684,1,60,0,33,0,15,1,3,0,8,1,3,0,3,3,1,32,0,2,4,61,30,0,15,1,1,0,4,62], key:0}}, clause_keys:[0], next_key:1, key:255}, 256: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,33,4,20,0,23,1,6,25,0,0,19,195,6,23,1,7,23,1,8,16,0,1,1,20,2,24,1,8,25,0,2,16,0,3,3,8,1,7,0,8,0,1,1,3,10,5,32,4,9,0,0,9,1,1,9,2,2,9,3,3,2,4,256], key:0}, 1:{code:[30,0,15,1,4,0,15,1,5,1,17,0,2,16,1,4,3,5], key:1}}, clause_keys:[0, 1], next_key:2, key:256}, 257: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,5,1,15,1,6,2,16,1,5,3,16,1,6,4,31,5], key:0}, 1:{code:[29,2,15,1,5,0,15,1,6,1,15,1,7,2,16,1,6,3,16,1,7,4,5], key:1}, 2:{code:[30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,8,1,8,0,8,1,11,1,8,1,12,2,4,250], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:257}, 258: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,16,1,3,1,8,1,3,0,4,0], key:0}}, clause_keys:[0], next_key:1, key:258}, 259: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,15,1,4,1,8,1,3,0,8,1,4,1,41,21,2,28,2147483700,1,60,0,33,0,15,1,5,0,15,1,6,1,8,1,5,0,8,1,6,1,3,9,1,32,0,2,4,61,30,0,15,1,2,0,15,1,3,1,4,62], key:0}}, clause_keys:[0], next_key:1, key:259}, 260: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,8,1,3,0,41,13,1,28,2147483684,1,60,0,33,0,15,1,3,0,8,1,3,0,3,5,1,32,0,2,4,62,30,0,15,1,2,0,8,1,2,0,4,6], key:0}}, clause_keys:[0], next_key:1, key:260}, 261: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,8,1,3,0,41,13,1,28,2147483684,1,60,0,33,0,15,1,3,0,8,1,3,0,3,4,1,32,0,2,4,62,30,0,15,1,2,0,8,1,2,0,4,260], key:0}}, clause_keys:[0], next_key:1, key:261}, 262: {is_public:false, clauses:{0:{code:[254,0,1,60,0,16,0,0,0,8,0,0,0,3,225,1,9,0,0,2,4,211], key:0}}, clause_keys:[0], next_key:1, key:262}, 263: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,19,212,0,23,1,1,25,0,2,19,208,1,23,1,2,23,1,3,19,208,3,25,0,0,25,0,1,31,12,208,1,25,0,0,25,0,1,12,212,0,24,1,1,25,0,2,2,4,263], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,19,212,0,23,1,1,25,0,3,19,208,1,25,0,6,25,0,7,31,9,7,0,13,1,25,0,5,25,0,4,3,13,8,8,0,4,0,8,0,1,1,3,215,7,9,5,0,8,0,1,1,9,6,2,8,0,0,3,3,216,7,8,0,2,0,13,1,25,0,0,25,0,4,3,13,5,12,217,0,25,0,0,25,0,1,9,2,1,9,3,2,2,4,91], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,19,208,0,23,1,1,23,1,2,19,208,2,25,0,0,23,1,3,19,212,3,25,0,1,25,0,2,31,12,212,1,25,0,1,25,0,2,12,208,0,25,0,0,24,1,1,2,4,263], key:2}, 3:{code:[29,4,1,60,0,60,1,60,2,19,208,0,25,0,0,23,1,1,19,212,1,25,0,1,25,0,2,31,12,208,1,25,0,0,25,0,1,12,212,0,24,1,1,25,0,2,2,4,263], key:3}, 4:{code:[29,5,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,19,212,0,25,0,7,25,0,3,31,9,7,0,13,1,25,0,5,25,0,4,3,13,8,8,0,4,0,8,0,1,1,3,215,7,8,0,6,0,3,218,7,9,5,0,8,0,1,1,9,6,2,8,0,0,3,3,216,7,8,0,2,0,13,1,25,0,0,25,0,4,3,13,5,12,217,0,25,0,0,25,0,1,9,2,1,9,3,2,2,4,91], key:4}, 5:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,19,208,0,25,0,5,25,0,6,31,9,6,0,13,1,25,0,4,25,0,3,3,13,7,8,0,3,0,8,0,1,1,3,215,6,9,4,0,8,0,1,1,9,5,2,8,0,0,3,3,216,6,8,0,2,0,13,1,25,0,0,25,0,3,3,13,4,12,217,0,25,0,0,25,0,1,9,2,1,10,128,2,2,4,91], key:5}, 6:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,16,0,6,0,31,9,6,0,13,1,25,0,4,25,0,3,3,13,7,8,0,3,0,8,0,1,1,3,215,6,8,0,5,0,3,218,6,9,4,0,8,0,1,1,9,5,2,8,0,0,3,3,216,6,8,0,2,0,13,1,25,0,0,25,0,3,3,13,4,12,217,0,25,0,0,25,0,1,9,2,1,10,128,2,2,4,91], key:6}, 7:{code:[28,2147483676,44,0,1000000,1000000,1000000,1000000,2147483657,46,0,2,208,2147483664,212,2147483670,71,2,72,3,73,5,71,0,72,1,73,4,30,0,74,6], key:7}}, clause_keys:[0, 1, 2, 3, 4, 5, 6, 7], next_key:8, key:263, index: 7}, 264: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,19,208,0,23,1,1,23,1,2,19,208,2,23,1,3,23,1,4,19,208,4,25,0,0,25,0,1,31,12,208,0,25,0,0,25,0,1,2,4,264], key:0}, 1:{code:[29,2,1,60,0,60,1,19,208,0,23,1,1,23,1,2,19,208,2,25,0,0,25,0,1,31,12,208,0,25,0,0,25,0,1,2,4,264], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,19,208,0,23,1,1,23,1,2,19,212,2,23,1,3,25,0,2,19,208,3,25,0,0,25,0,1,31,12,208,1,25,0,0,25,0,1,12,212,0,24,1,1,25,0,2,2,4,264], key:2}, 3:{code:[29,4,1,60,0,60,1,60,2,19,208,0,25,0,0,23,1,1,19,212,1,25,0,1,25,0,2,31,12,208,1,25,0,0,25,0,1,12,212,0,24,1,1,25,0,2,2,4,264], key:3}, 4:{code:[29,5,1,60,0,60,1,60,2,19,212,0,23,1,1,25,0,2,19,208,1,23,1,2,23,1,3,19,208,3,25,0,0,25,0,1,31,12,208,1,25,0,0,25,0,1,12,212,0,24,1,1,25,0,2,2,4,264], key:4}, 5:{code:[30,0,1,60,0,60,1,19,212,0,25,0,0,25,0,1,31,9,0,0,9,1,1,2,4,69], key:5}, 6:{code:[254,0,1,60,0,16,0,0,0,31,9,0,0,10,128,1,2,4,69], key:6}, 7:{code:[28,2147483676,44,0,1000000,1000000,1000000,1000000,2147483657,46,0,2,208,2147483664,212,2147483672,71,0,72,1,72,2,73,3,71,4,73,5,30,0,74,6], key:7}}, clause_keys:[0, 1, 2, 3, 4, 5, 6, 7], next_key:8, key:264, index: 7}, 265: {is_public:false, clauses:{0:{code:[254,0,15,1,2,0,8,1,2,0,4,264], key:0}}, clause_keys:[0], next_key:1, key:265}, 266: {is_public:false, clauses:{0:{code:[254,0,15,1,7,0,15,1,8,1,15,1,9,2,8,1,7,0,8,1,8,1,8,1,9,2,10,0,3,4,71], key:0}}, clause_keys:[0], next_key:1, key:266}, 267: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,8,1,3,0,10,0,1,4,72], key:0}}, clause_keys:[0], next_key:1, key:267}, 268: {is_public:false, clauses:{0:{code:[254,0,1,60,0,8,0,0,0,3,43,1,9,0,0,2,4,54], key:0}}, clause_keys:[0], next_key:1, key:268}, 269: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,33,2,16,0,0,0,16,0,1,1,8,0,0,0,3,3,3,32,2,8,0,0,0,3,58,2,9,0,0,9,1,1,2,4,57], key:0}, 1:{code:[30,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,4,57], key:1}}, clause_keys:[0, 1], next_key:2, key:269}, 270: {is_public:false, clauses:{0:{code:[254,0,1,60,0,8,0,0,0,3,43,1,9,0,0,2,4,55], key:0}}, clause_keys:[0], next_key:1, key:270}, 271: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,44,2,9,0,0,9,1,1,2,4,45], key:0}}, clause_keys:[0], next_key:1, key:271}, 272: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,44,2,9,0,0,9,1,1,2,4,46], key:0}}, clause_keys:[0], next_key:1, key:272}, 273: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,44,2,9,0,0,9,1,1,2,4,47], key:0}}, clause_keys:[0], next_key:1, key:273}, 274: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,44,2,9,0,0,9,1,1,2,4,48], key:0}}, clause_keys:[0], next_key:1, key:274}, 275: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,43,2,9,0,0,9,1,1,2,4,49], key:0}}, clause_keys:[0], next_key:1, key:275}, 276: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,43,2,9,0,0,9,1,1,2,4,50], key:0}}, clause_keys:[0], next_key:1, key:276}, 277: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,44,2,9,0,0,9,1,1,2,4,51], key:0}}, clause_keys:[0], next_key:1, key:277}, 278: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,44,2,9,0,0,9,1,1,2,4,52], key:0}}, clause_keys:[0], next_key:1, key:278}, 279: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,43,2,9,0,0,9,1,1,2,4,53], key:0}}, clause_keys:[0], next_key:1, key:279}, 280: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,1,0,16,0,2,1,8,0,0,0,3,44,3,9,0,0,9,1,1,9,2,2,2,4,70], key:0}}, clause_keys:[0], next_key:1, key:280}, 281: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,44,2,9,0,0,9,1,1,10,0,2,2,4,70], key:0}}, clause_keys:[0], next_key:1, key:281}, 282: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,8,1,5,0,8,1,6,1,10,0,2,4,70], key:0}}, clause_keys:[0], next_key:1, key:282}, 283: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,1,0,16,0,2,1,8,0,0,0,3,43,3,9,0,0,9,1,1,9,2,2,2,4,59], key:0}}, clause_keys:[0], next_key:1, key:283}, 287: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,43,2,9,0,0,9,1,1,12,284,3,26,530,12,285,5,26,530,12,286,7,26,128,13,6,24,1,7,26,0,13,4,24,1,5,24,1,6,13,2,24,1,3,24,1,4,2,4,59], key:0}}, clause_keys:[0], next_key:1, key:287}, 288: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,8,1,5,0,8,1,6,1,12,284,7,26,530,12,285,9,26,530,12,286,11,26,128,13,10,24,1,11,26,0,13,8,24,1,9,24,1,10,13,2,24,1,7,24,1,8,4,59], key:0}}, clause_keys:[0], next_key:1, key:288}, 289: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,43,2,9,0,0,9,1,1,12,284,3,26,128,12,285,5,26,530,12,286,7,26,128,13,6,24,1,7,26,0,13,4,24,1,5,24,1,6,13,2,24,1,3,24,1,4,2,4,59], key:0}}, clause_keys:[0], next_key:1, key:289}, 290: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,8,1,5,0,8,1,6,1,12,284,7,26,128,12,285,9,26,530,12,286,11,26,128,13,10,24,1,11,26,0,13,8,24,1,9,24,1,10,13,2,24,1,7,24,1,8,4,59], key:0}}, clause_keys:[0], next_key:1, key:290}, 291: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,43,2,9,0,0,9,1,1,12,284,3,26,128,12,285,5,26,128,12,286,7,26,530,13,6,24,1,7,26,0,13,4,24,1,5,24,1,6,13,2,24,1,3,24,1,4,2,4,59], key:0}}, clause_keys:[0], next_key:1, key:291}, 292: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,8,1,5,0,8,1,6,1,12,284,7,26,128,12,285,9,26,128,12,286,11,26,530,13,10,24,1,11,26,0,13,8,24,1,9,24,1,10,13,2,24,1,7,24,1,8,4,59], key:0}}, clause_keys:[0], next_key:1, key:292}, 293: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,8,1,3,0,41,13,1,28,2147483684,1,60,0,33,0,15,1,3,0,8,1,3,0,3,4,1,32,0,2,4,62,30,0,15,1,2,0,8,1,2,0,4,7], key:0}}, clause_keys:[0], next_key:1, key:293}, 294: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,19,208,0,23,1,1,23,1,2,19,208,2,23,1,3,23,1,4,19,208,4,25,0,0,25,0,1,31,12,208,0,25,0,0,25,0,1,3,294,2,2,4,61], key:0}, 1:{code:[30,0,1,60,0,60,1,19,208,0,23,1,1,23,1,2,19,208,2,25,0,0,25,0,1,31,12,208,0,25,0,0,25,0,1,3,294,2,2,4,61], key:1}, 2:{code:[254,0,1,15,1,2,0,8,1,2,0,3,264,0,2,4,61], key:2}, 3:{code:[254,0,15,1,1,0,5], key:3}, 4:{code:[28,2147483666,44,0,1000000,1000000,1000000,1000000,2147483657,46,0,1,208,2147483662,71,0,73,1,29,2147483670,74,2,30,0,74,3], key:4}}, clause_keys:[0, 1, 2, 3, 4], next_key:5, key:294, index: 4}, 295: {is_public:false, clauses:{0:{code:[254,0,15,1,2,0,8,1,2,0,4,294], key:0}}, clause_keys:[0], next_key:1, key:295}, 296: {is_public:false, clauses:{0:{code:[28,1,17,0,0,17,0,1,5], key:0}, 1:{code:[29,2,20,0,23,1,2,26,0,20,1,24,1,2,26,0,31,5], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,20,0,23,1,4,23,1,5,20,5,25,0,0,25,0,1,15,1,6,1,8,1,4,0,8,0,0,1,8,1,6,2,8,0,2,3,7,7,4,40,66,5,3,13,0,25,0,0,25,0,1,9,2,1,2,4,296,28,2147483774,1,60,0,60,1,60,2,60,3,33,3,16,0,1,0,15,1,8,1,16,0,0,2,16,0,2,3,15,1,9,4,8,0,1,0,8,1,8,1,3,32,4,32,3,9,0,0,13,1,25,0,1,25,0,2,2,4,9,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,7,0,8,1,8,1,8,1,9,2,8,1,10,3,41,167,4,254,0,1,60,0,60,1,60,2,33,2,15,1,7,0,15,1,8,1,16,0,0,2,16,0,1,3,8,1,7,0,8,1,8,1,3,10,3,32,2,9,0,0,9,1,1,2,4,9], key:2}, 3:{code:[254,0,44,0,2147483657,1000000,1000000,2147483662,1000000,45,0,1,0,0,71,1,73,2], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:296, index: 3}, 297: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,20,0,25,0,0,25,0,1,16,0,2,1,16,0,3,2,31,9,0,0,9,1,1,9,2,2,9,3,3,2,4,298], key:0}, 1:{code:[30,0,17,0,0,15,1,3,1,16,1,3,2,5], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,0,1000000,45,0,1,0,1], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:297, index: 2}, 298: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,16,0,5,0,15,1,8,1,16,0,4,2,16,0,2,3,8,1,8,0,8,0,5,1,8,0,3,2,8,0,0,3,3,299,6,9,3,0,9,4,1,13,2,25,0,5,25,0,1,3,297,6,9,0,0,9,1,1,9,2,2,2,4,297], key:0}}, clause_keys:[0], next_key:1, key:298}, 299: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,33,4,20,0,23,1,6,25,0,0,16,0,1,1,16,0,2,2,16,0,3,3,8,1,6,0,8,0,1,1,3,10,5,32,4,9,0,0,9,1,1,9,2,2,9,3,3,2,4,299], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,33,4,20,0,23,1,6,25,0,0,16,0,1,1,20,2,24,1,6,25,0,2,16,0,3,3,8,1,6,0,8,0,1,1,3,32,5,32,4,9,0,0,9,1,1,9,2,2,9,3,3,2,4,299], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,20,0,23,1,4,25,0,0,16,0,1,1,16,0,2,2,20,3,24,1,4,25,0,3,31,9,0,0,9,1,1,9,2,2,9,3,3,2,4,299], key:2}, 3:{code:[30,0,17,0,0,15,1,4,1,17,0,2,17,0,3,5], key:3}, 4:{code:[254,0,44,0,2147483657,1000000,1000000,2147483662,1000000,45,0,1,0,3,71,0,72,1,73,2], key:4}}, clause_keys:[0, 1, 2, 3, 4], next_key:5, key:299, index: 4}, 300: {is_public:false, clauses:{0:{code:[28,1,17,0,0,17,0,1,5], key:0}, 1:{code:[29,2,20,0,23,1,2,26,0,20,1,24,1,2,26,0,31,5], key:1}, 2:{code:[30,0,1,60,0,60,1,20,0,23,1,4,23,1,5,19,195,4,23,1,6,23,1,7,20,5,23,1,8,23,1,9,19,195,8,23,1,10,23,1,11,15,1,12,1,8,1,6,0,8,1,10,1,8,1,12,2,8,1,7,3,8,0,1,4,8,0,0,5,8,1,11,6,8,1,9,7,7,13,8,40,93,9,2,9,0,0,9,1,1,2,4,300,28,2147483859,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,33,8,16,0,5,0,16,0,1,1,16,0,4,2,16,0,6,3,16,0,7,4,16,0,0,5,16,0,2,6,16,0,3,7,15,1,12,8,8,0,5,0,8,0,1,1,3,32,9,32,8,9,4,0,12,195,2,25,0,5,25,0,6,13,1,24,1,2,25,0,7,3,9,8,9,0,0,12,195,2,25,0,1,25,0,2,13,1,24,1,2,25,0,3,2,4,9,30,0,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,15,1,15,4,15,1,16,5,15,1,17,6,15,1,18,7,15,1,19,8,8,1,11,0,8,1,12,1,8,1,13,2,8,1,15,3,8,1,16,4,8,1,14,5,8,1,18,6,41,280,7,254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,16,0,1,0,15,1,10,1,16,0,4,2,16,0,5,3,16,0,0,4,16,0,2,5,16,0,3,6,8,0,1,0,8,1,10,1,3,10,7,32,6,9,4,0,9,5,1,3,9,6,9,0,0,12,195,2,25,0,1,25,0,2,13,1,24,1,2,25,0,3,2,4,9], key:2}, 3:{code:[254,0,44,0,2147483657,1000000,1000000,2147483662,1000000,45,0,1,0,0,71,1,73,2], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:300, index: 3}, 301: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,20,0,23,1,7,23,1,8,19,195,7,25,0,5,25,0,6,16,0,4,1,16,0,2,2,8,1,8,0,8,0,5,1,8,0,3,2,8,0,0,3,3,302,7,9,3,0,9,4,1,12,195,3,25,0,5,25,0,6,13,2,24,1,3,25,0,1,3,301,7,9,0,0,9,1,1,9,2,2,2,4,301], key:0}, 1:{code:[30,0,17,0,0,15,1,3,1,16,1,3,2,5], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,0,1000000,45,0,1,0,1], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:301, index: 2}, 302: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,33,4,20,0,23,1,6,25,0,0,19,195,6,23,1,7,23,1,8,16,0,1,1,16,0,2,2,16,0,3,3,8,1,7,0,8,0,1,1,3,10,5,32,4,9,0,0,9,1,1,9,2,2,9,3,3,2,4,302], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,33,4,20,0,23,1,6,25,0,0,19,195,6,23,1,7,23,1,8,16,0,1,1,20,2,23,1,9,25,0,2,19,195,9,24,1,7,24,1,8,16,0,3,3,8,1,7,0,8,0,1,1,3,32,5,32,4,9,0,0,9,1,1,9,2,2,9,3,3,2,4,302], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,20,0,23,1,4,25,0,0,19,195,4,23,1,5,23,1,6,16,0,1,1,16,0,2,2,20,3,23,1,7,25,0,3,19,195,7,24,1,5,24,1,6,31,9,0,0,9,1,1,9,2,2,9,3,3,2,4,302], key:2}, 3:{code:[30,0,17,0,0,15,1,4,1,17,0,2,17,0,3,5], key:3}, 4:{code:[254,0,44,0,2147483657,1000000,1000000,2147483662,1000000,45,0,1,0,3,71,0,72,1,73,2], key:4}}, clause_keys:[0, 1, 2, 3, 4], next_key:5, key:302, index: 4}, 303: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,16,1,3,2,5], key:0}, 1:{code:[30,0,20,0,23,1,6,23,1,7,15,1,8,1,20,2,24,1,6,23,1,9,8,1,7,0,8,1,8,1,8,1,9,2,4,303], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:303, index: 2}, 304: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,16,1,3,2,5], key:0}, 1:{code:[30,0,20,0,23,1,6,23,1,7,15,1,8,1,15,1,9,2,8,1,7,0,13,1,24,1,6,25,1,8,8,1,9,2,4,304], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:304, index: 2}, 305: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,8,1,5,0,10,0,1,8,1,6,2,4,304], key:0}}, clause_keys:[0], next_key:1, key:305}, 306: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,16,1,3,2,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,20,0,23,1,5,25,0,0,15,1,6,1,16,0,2,2,8,0,1,0,12,198,1,25,1,6,27,1,3,34,3,9,0,0,9,1,1,9,2,2,2,4,306], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:306, index: 2}, 307: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,17,0,2,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,20,0,25,0,1,25,0,0,16,0,1,1,16,0,2,2,31,9,0,0,9,1,1,9,2,2,2,4,307], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,20,0,23,1,3,25,0,0,16,0,1,1,20,2,24,1,3,25,0,2,31,9,0,0,9,1,1,9,2,2,2,4,307], key:2}, 3:{code:[254,0,44,0,2147483657,1000000,1000000,2147483662,1000000,45,0,1,0,0,71,1,73,2], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:307, index: 3}, 308: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,15,1,5,0,16,0,4,1,16,0,0,2,8,1,5,0,13,1,25,0,1,25,0,3,3,13,5,9,3,0,9,4,1,8,0,2,2,3,303,5,9,0,0,13,1,25,0,1,25,0,2,2,4,13], key:0}}, clause_keys:[0], next_key:1, key:308}, 309: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,19,317,0,25,0,0,25,0,1,16,0,2,1,31,9,0,0,9,1,1,9,2,2,2,4,318], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,19,208,0,25,0,0,25,0,1,16,0,2,1,31,9,0,0,9,1,1,9,2,2,2,4,318], key:1}, 2:{code:[254,0,15,1,5,0,15,1,6,1,10,368,0,8,1,5,1,8,1,6,2,4,318], key:2}, 3:{code:[28,2147483664,44,0,1000000,1000000,1000000,1000000,2147483657,46,0,2,208,1,317,0,30,0,74,2], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:309, index: 3}, 310: {is_public:false, clauses:{0:{code:[254,0,15,1,4,0,15,1,5,1,8,1,4,0,13,1,25,1,5,26,0,4,309], key:0}}, clause_keys:[0], next_key:1, key:310}, 311: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,5,0,13,8,25,1,7,26,0,13,1,25,1,6,24,1,8,4,309], key:0}}, clause_keys:[0], next_key:1, key:311}, 312: {is_public:false, clauses:{0:{code:[254,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,8,1,6,0,13,11,25,1,9,26,0,13,10,25,1,8,24,1,11,13,1,25,1,7,24,1,10,4,309], key:0}}, clause_keys:[0], next_key:1, key:312}, 313: {is_public:false, clauses:{0:{code:[254,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,7,0,13,14,25,1,11,26,0,13,13,25,1,10,24,1,14,13,12,25,1,9,24,1,13,13,1,25,1,8,24,1,12,4,309], key:0}}, clause_keys:[0], next_key:1, key:313}, 314: {is_public:false, clauses:{0:{code:[254,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,8,0,13,17,25,1,13,26,0,13,16,25,1,12,24,1,17,13,15,25,1,11,24,1,16,13,14,25,1,10,24,1,15,13,1,25,1,9,24,1,14,4,309], key:0}}, clause_keys:[0], next_key:1, key:314}, 315: {is_public:false, clauses:{0:{code:[254,0,15,1,9,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,15,1,14,5,15,1,15,6,8,1,9,0,13,20,25,1,15,26,0,13,19,25,1,14,24,1,20,13,18,25,1,13,24,1,19,13,17,25,1,12,24,1,18,13,16,25,1,11,24,1,17,13,1,25,1,10,24,1,16,4,309], key:0}}, clause_keys:[0], next_key:1, key:315}, 316: {is_public:false, clauses:{0:{code:[254,0,15,1,10,0,15,1,11,1,15,1,12,2,15,1,13,3,15,1,14,4,15,1,15,5,15,1,16,6,15,1,17,7,8,1,10,0,13,23,25,1,17,26,0,13,22,25,1,16,24,1,23,13,21,25,1,15,24,1,22,13,20,25,1,14,24,1,21,13,19,25,1,13,24,1,20,13,18,25,1,12,24,1,19,13,1,25,1,11,24,1,18,4,309], key:0}}, clause_keys:[0], next_key:1, key:316}, 318: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,15,1,3,0,19,317,1,25,0,0,25,0,1,16,0,2,2,31,9,0,0,9,1,1,9,2,2,2,4,318], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,15,1,3,0,19,208,1,25,0,0,25,0,1,16,0,2,2,31,9,0,0,9,1,1,9,2,2,2,4,318], key:1}, 2:{code:[30,0,1,60,0,60,1,16,0,0,0,15,1,6,1,15,1,7,2,8,1,6,0,8,1,7,1,8,0,1,2,3,308,2,9,0,0,9,1,1,2,4,223], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:318}, 319: {is_public:false, clauses:{0:{code:[28,1,15,1,4,0,15,1,5,1,15,1,6,2,16,1,6,3,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,16,0,0,0,16,0,1,1,16,0,4,2,16,0,3,3,8,0,4,0,8,0,3,1,3,36,11,8,0,0,0,8,0,1,1,8,0,4,2,12,321,3,25,0,10,25,0,7,25,0,8,25,0,5,25,0,9,3,117,11,8,0,6,0,9,10,1,3,24,11,12,322,0,25,0,6,25,0,7,25,0,8,25,0,5,25,0,9,3,64,10,8,0,2,0,12,198,1,25,0,4,25,0,5,3,34,6,9,0,0,9,1,1,9,2,2,9,3,3,2,4,319], key:1}}, clause_keys:[0, 1], next_key:2, key:319}, 320: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,0,0,16,0,1,1,8,0,1,0,8,0,2,1,3,215,3,9,0,0,9,1,1,14,0,2,9,2,3,2,4,319], key:0}}, clause_keys:[0], next_key:1, key:320}, 323: {is_public:false, clauses:{0:{code:[28,1,1,10,565,0,3,157,0,2,4,237], key:0}, 1:{code:[29,2,4,62], key:1}, 2:{code:[30,0,5], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:323}, 324: {is_public:false, clauses:{0:{code:[28,1,15,1,2,0,20,1,25,1,2,23,1,3,5], key:0}, 1:{code:[30,0,15,1,4,0,20,1,23,1,5,23,1,6,8,1,4,0,8,1,6,1,4,324], key:1}}, clause_keys:[0, 1], next_key:2, key:324}, 325: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,15,1,4,2,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,20,0,23,1,5,25,0,0,16,0,1,1,16,0,2,2,8,0,2,0,8,1,5,1,3,288,3,9,0,0,9,1,1,9,2,2,2,4,327], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:325, index: 2}, 326: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,0,0,16,0,1,1,8,0,2,0,3,43,3,9,0,0,9,1,1,9,2,2,2,4,325], key:0}}, clause_keys:[0], next_key:1, key:326}, 327: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,15,1,4,2,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,20,0,25,0,3,25,0,0,16,0,1,1,16,0,2,2,8,0,2,0,8,0,1,1,3,288,4,8,0,2,0,9,3,1,3,288,4,9,0,0,9,1,1,9,2,2,2,4,327], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:327, index: 2}, 328: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,16,0,1,0,16,0,3,1,13,0,27,97,26,0,13,1,25,0,0,26,0,3,9,4,13,0,27,122,26,0,13,1,25,0,2,26,0,3,9,4,9,0,0,9,1,1,9,2,2,7,6,3,7,7,4,9,3,5,2,41,73,6,28,2147483834,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,5,16,0,4,0,16,0,3,1,16,0,6,2,16,0,2,3,16,0,1,4,16,0,0,5,8,0,4,0,8,0,3,1,3,37,7,8,0,3,0,9,6,1,3,37,7,32,5,13,0,27,65,26,0,13,1,25,0,2,26,0,3,9,5,8,0,1,0,12,195,1,25,0,3,25,0,4,3,34,5,9,0,0,12,198,1,25,0,1,25,0,2,2,4,34,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,13,0,8,1,9,1,4,9], key:0}}, clause_keys:[0], next_key:1, key:328}, 329: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,15,1,4,0,16,0,0,1,8,1,4,0,8,0,4,1,3,24,5,9,4,0,13,1,25,0,3,25,0,2,3,9,5,9,3,0,8,0,1,1,3,328,4,9,0,0,13,1,25,0,1,25,0,2,2,4,24], key:0}}, clause_keys:[0], next_key:1, key:329}, 330: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,8,1,3,0,10,585,1,4,34], key:0}}, clause_keys:[0], next_key:1, key:330}, 332: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,33,3,16,0,1,0,16,0,2,1,16,0,0,2,8,0,1,0,3,5,4,8,0,2,0,3,5,4,32,3,8,0,2,0,8,0,1,1,3,38,3,9,0,0,12,198,4,25,0,2,27,1,12,195,3,24,1,4,25,0,1,12,331,2,24,1,3,12,198,1,25,0,1,24,1,2,2,4,34], key:0}}, clause_keys:[0], next_key:1, key:332}, 333: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:333}, 334: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:334}, 335: {is_public:false, clauses:{0:{code:[254,0,10,568,0,4,122], key:0}}, clause_keys:[0], next_key:1, key:335}, 336: {is_public:false, clauses:{0:{code:[254,0,10,593,0,4,122], key:0}}, clause_keys:[0], next_key:1, key:336}, 337: {is_public:false, clauses:{0:{code:[254,0,10,595,0,4,122], key:0}}, clause_keys:[0], next_key:1, key:337}, 339: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,16,0,0,0,15,1,6,1,16,0,2,2,12,338,0,25,0,1,10,597,1,13,2,25,1,6,26,0,3,79,3,12,195,1,25,0,0,25,0,1,13,0,24,1,1,25,0,2,2,4,124], key:0}, 1:{code:[30,0,1,60,0,33,0,15,1,4,0,15,1,5,1,15,1,6,2,8,1,6,0,3,124,1,32,0,2,4,61], key:1}}, clause_keys:[0, 1], next_key:2, key:339}, 340: {is_public:false, clauses:{0:{code:[28,1,1,17,569,0,15,1,3,1,15,1,4,2,31,10,593,0,2,4,122], key:0}, 1:{code:[30,0,1,17,570,0,15,1,3,1,15,1,4,2,31,10,570,0,2,4,122], key:1}, 2:{code:[254,0,1,60,0,60,1,60,2,60,3,16,0,1,0,16,0,0,1,16,0,2,2,8,0,2,0,8,0,3,1,3,341,4,10,409,0,10,126,1,8,0,1,2,8,0,0,3,8,0,2,4,8,0,3,5,3,342,4,8,0,1,0,3,222,4,3,343,4,9,0,0,9,1,1,9,2,2,9,3,3,2,41,85,4,28,2147483797,1,60,0,60,1,60,2,60,3,60,4,33,4,16,0,1,0,16,0,0,1,16,0,2,2,16,0,3,3,3,344,5,32,4,8,0,1,0,3,124,4,10,479,0,10,609,1,9,0,2,9,1,3,9,2,4,9,3,5,2,4,342,30,0,15,1,4,0,15,1,5,1,15,1,6,2,15,1,7,3,4,62], key:2}, 3:{code:[28,2147483664,44,0,2147483657,1000000,1000000,1000000,1000000,45,0,2,569,0,570,1,30,0,74,2], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:340, index: 3}, 341: {is_public:false, clauses:{0:{code:[28,1,15,1,3,0,15,1,4,1,8,1,4,0,4,130], key:0}, 1:{code:[30,0,1,60,0,60,1,16,0,0,0,16,0,1,1,8,0,0,0,3,128,2,10,613,0,3,127,2,9,0,0,9,1,1,2,4,341], key:1}}, clause_keys:[0, 1], next_key:2, key:341}, 342: {is_public:false, clauses:{0:{code:[28,1,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,15,1,15,4,15,1,16,5,8,1,11,0,8,1,13,1,8,1,14,2,8,1,15,3,8,1,16,4,4,347], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,33,0,15,1,7,0,16,0,1,1,16,0,2,2,16,0,3,3,16,0,4,4,16,0,5,5,40,72,0,6,40,94,0,6,9,1,0,9,2,1,9,3,2,9,4,3,9,5,4,3,347,6,32,0,2,4,61,28,2147483738,1,60,0,33,0,10,593,0,3,123,1,32,0,2,4,61,30,0,4,62,28,2147483760,1,60,0,33,0,10,595,0,3,123,1,32,0,2,4,61,30,0,4,62], key:1}}, clause_keys:[0, 1], next_key:2, key:342}, 343: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,33,0,8,0,1,0,3,123,2,9,1,0,3,346,2,32,0,2,5], key:0}}, clause_keys:[0], next_key:1, key:343}, 344: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,33,0,8,0,1,0,3,123,2,9,1,0,3,345,2,32,0,2,5], key:0}}, clause_keys:[0], next_key:1, key:344}, 345: {is_public:false, clauses:{0:{code:[28,1,17,610,0,5], key:0}, 1:{code:[30,0,17,611,0,5], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1000000,1000000,45,0,2,610,0,611,1], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:345, index: 2}, 346: {is_public:false, clauses:{0:{code:[28,1,1,17,568,0,31,10,610,0,2,4,122], key:0}, 1:{code:[30,0,1,17,612,0,31,10,611,0,2,4,122], key:1}, 2:{code:[254,0,15,1,1,0,5], key:2}, 3:{code:[28,2147483664,44,0,2147483657,1000000,1000000,1000000,1000000,45,0,2,568,0,612,1,30,0,74,2], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:346, index: 3}, 347: {is_public:false, clauses:{0:{code:[254,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,7,0,8,1,8,1,8,1,9,2,8,1,10,3,8,1,11,4,41,45,5,28,2147483780,1,60,0,60,1,60,2,60,3,60,4,60,5,33,5,16,0,0,0,16,0,1,1,16,0,2,2,16,0,3,3,16,0,4,4,8,0,0,0,8,0,1,1,3,348,6,32,5,8,0,0,0,8,0,1,1,8,0,2,2,8,0,3,3,3,349,5,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,350,30,0,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,15,1,15,4,10,622,0,8,1,11,1,8,1,12,2,8,1,13,3,8,1,14,4,8,1,15,5,4,351], key:0}}, clause_keys:[0], next_key:1, key:347}, 348: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,1,0,16,0,2,1,8,0,0,0,3,352,3,9,0,0,9,1,1,9,2,2,2,4,353], key:0}}, clause_keys:[0], next_key:1, key:348}, 349: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,16,0,2,0,16,0,3,1,15,1,6,2,16,0,4,3,8,1,6,0,8,0,1,1,3,215,5,9,1,0,9,2,1,9,3,2,9,4,3,8,0,0,4,3,355,5,9,0,0,2,4,129], key:0}}, clause_keys:[0], next_key:1, key:349}, 350: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,16,0,1,0,16,0,2,1,16,0,3,2,16,0,4,3,16,0,5,4,8,0,0,0,3,361,7,32,6,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,351], key:0}}, clause_keys:[0], next_key:1, key:350}, 351: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,17,657,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,31,10,657,0,9,0,1,9,1,2,9,2,3,9,3,4,9,4,5,2,4,364], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,17,649,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,31,10,649,0,9,0,1,9,1,2,9,2,3,9,3,4,9,4,5,2,4,364], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,60,4,17,648,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,31,10,648,0,9,0,1,9,1,2,9,2,3,9,3,4,9,4,5,2,4,364], key:2}, 3:{code:[29,4,1,60,0,60,1,60,2,60,3,60,4,17,647,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,31,10,647,0,9,0,1,9,1,2,9,2,3,9,3,4,9,4,5,2,4,364], key:3}, 4:{code:[29,5,1,60,0,60,1,60,2,60,3,60,4,17,646,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,31,10,646,0,9,0,1,9,1,2,9,2,3,9,3,4,9,4,5,2,4,364], key:4}, 5:{code:[29,6,1,60,0,60,1,60,2,17,622,0,17,409,1,16,0,1,2,16,0,2,3,16,0,0,4,15,1,6,5,31,9,0,0,9,1,1,9,2,2,3,339,3,10,661,0,2,4,122], key:5}, 6:{code:[29,7,1,17,622,0,17,479,1,15,1,6,2,15,1,7,3,15,1,8,4,15,1,9,5,31,10,612,0,2,4,122], key:6}, 7:{code:[29,8,17,622,0,15,1,6,1,15,1,7,2,15,1,8,3,15,1,9,4,15,1,10,5,31,5], key:7}, 8:{code:[29,9,1,17,656,0,17,409,1,15,1,6,2,15,1,7,3,15,1,8,4,15,1,9,5,31,10,610,0,2,4,122], key:8}, 9:{code:[29,10,17,656,0,15,1,6,1,15,1,7,2,15,1,8,3,15,1,9,4,15,1,10,5,31,5], key:9}, 10:{code:[29,11,1,17,650,0,15,1,6,1,15,1,7,2,15,1,8,3,15,1,9,4,15,1,10,5,31,10,593,0,2,4,122], key:10}, 11:{code:[29,12,1,17,654,0,15,1,6,1,15,1,7,2,15,1,8,3,15,1,9,4,15,1,10,5,31,2,4,61], key:11}, 12:{code:[29,13,1,60,0,60,1,17,653,0,15,1,6,1,15,1,7,2,15,1,8,3,16,0,1,4,16,0,0,5,31,9,1,0,3,127,2,9,0,0,3,131,1,2,4,61], key:12}, 13:{code:[29,14,1,17,651,0,15,1,6,1,15,1,7,2,15,1,8,3,15,1,9,4,15,1,10,5,31,2,4,237], key:13}, 14:{code:[29,15,1,60,0,60,1,60,2,60,3,60,4,60,5,19,363,0,25,0,5,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,31,12,365,1,23,1,2,12,208,0,26,567,24,1,1,3,295,6,12,365,1,25,0,5,12,208,0,26,567,24,1,1,3,262,6,8,0,0,0,8,0,1,1,8,0,2,2,8,0,3,3,3,349,5,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,350], key:14}, 15:{code:[29,16,1,60,0,60,1,60,2,60,3,60,4,17,655,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,31,12,363,0,27,0,9,0,1,9,1,2,9,2,3,9,3,4,9,4,5,2,4,351], key:15}, 16:{code:[29,17,1,60,0,60,1,60,2,60,3,60,4,17,652,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,31,8,0,2,0,3,366,5,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,350], key:16}, 17:{code:[29,18,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,17,426,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,31,8,0,1,0,13,1,25,0,5,25,0,9,3,13,10,9,9,0,8,0,6,1,3,215,10,8,0,8,0,8,0,6,1,3,215,9,8,0,7,0,13,1,25,0,5,25,0,8,3,13,9,12,367,1,23,1,2,25,0,7,26,128,12,208,0,26,567,24,1,1,3,209,8,10,664,0,3,287,7,12,217,0,25,0,5,25,0,6,3,64,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,350], key:17}, 18:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,17,423,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,31,8,0,1,0,13,1,25,0,5,25,0,9,3,13,10,9,9,0,8,0,6,1,3,215,10,8,0,8,0,8,0,6,1,3,215,9,8,0,7,0,13,1,25,0,5,25,0,8,3,13,9,12,367,1,23,1,2,25,0,7,23,1,3,12,208,0,26,567,24,1,1,3,295,8,10,665,0,3,287,7,12,217,0,25,0,5,25,0,6,3,64,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,350], key:18}, 19:{code:[254,0,44,0,2147483657,1000000,1000000,1000000,2147483690,45,0,15,423,18,426,17,622,2147483695,646,4,647,3,648,2,649,1,650,10,651,13,652,16,653,12,654,11,655,15,656,2147483701,657,0,46,0,1,363,14,71,5,72,6,73,7,71,8,73,9], key:19}}, clause_keys:[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19], next_key:20, key:351, index: 19}, 352: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,123,2,9,0,0,9,1,1,2,4,354], key:0}}, clause_keys:[0], next_key:1, key:352}, 353: {is_public:false, clauses:{0:{code:[28,1,17,625,0,15,1,3,1,15,1,4,2,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,17,626,0,16,0,0,1,16,0,1,2,31,9,0,0,9,1,1,7,3,2,2,41,32,3,28,2147483720,1,60,0,33,0,15,1,6,0,15,1,7,1,15,1,8,2,8,1,6,0,8,1,7,1,8,1,8,2,40,88,3,1,32,0,2,4,61,30,0,15,1,3,0,15,1,4,1,15,1,5,2,4,62,28,2147483815,1,60,0,60,1,60,2,60,3,33,0,16,0,3,0,16,0,2,1,16,0,1,2,8,0,3,0,8,0,2,1,8,0,1,2,3,333,4,10,627,0,3,287,4,9,3,0,3,287,4,10,628,0,3,287,3,9,2,0,3,64,3,7,2,0,9,1,1,40,183,2,2,32,0,2,4,61,30,0,15,1,3,0,15,1,4,1,15,1,5,2,4,62,28,2147483859,1,60,0,33,0,15,1,5,0,15,1,6,1,8,1,6,0,40,227,1,1,32,0,2,4,62,30,0,15,1,3,0,15,1,4,1,8,1,4,0,4,222,28,2147483890,15,1,3,0,8,1,3,0,10,128,1,4,9,30,0,15,1,3,0,8,1,3,0,10,127,1,4,9], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1000000,1000000,45,0,2,625,0,626,1], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:353, index: 2}, 354: {is_public:false, clauses:{0:{code:[28,1,17,568,0,17,625,1,5], key:0}, 1:{code:[29,2,17,610,0,17,625,1,5], key:1}, 2:{code:[29,3,17,612,0,17,626,1,5], key:2}, 3:{code:[30,0,17,611,0,17,626,1,5], key:3}, 4:{code:[254,0,44,0,2147483657,1000000,1000000,1000000,1000000,45,0,4,568,0,610,1,611,3,612,2], key:4}}, clause_keys:[0, 1, 2, 3, 4], next_key:5, key:354, index: 4}, 355: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,16,0,9,0,16,0,8,1,16,0,7,2,15,1,8,3,16,0,4,4,8,1,8,0,14,7,1,8,0,0,2,3,356,10,9,9,0,14,5,1,8,0,1,2,3,356,10,9,8,0,8,0,2,1,3,329,9,9,7,0,8,0,5,1,3,357,8,8,0,6,0,40,213,1,7,12,338,0,25,0,3,9,5,1,12,358,3,25,0,6,12,284,5,26,128,12,285,7,26,530,12,286,9,26,128,13,8,24,1,9,26,0,13,6,24,1,7,24,1,8,13,4,24,1,5,24,1,6,13,2,24,1,3,24,1,4,3,59,7,13,6,25,0,3,26,0,13,5,26,640,24,1,6,13,4,25,0,2,24,1,5,13,3,26,628,24,1,4,13,2,25,0,1,24,1,3,13,0,25,0,0,24,1,2,9,4,1,2,4,359,28,2147483884,1,60,0,33,0,15,1,3,0,8,1,3,0,3,334,1,32,0,2,4,62,30,0,15,1,3,0,8,1,3,0,14,10,1,4,9], key:0}}, clause_keys:[0], next_key:1, key:355}, 356: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,15,1,5,0,16,0,3,1,16,0,0,2,8,1,5,0,8,0,4,1,3,26,5,8,0,4,0,8,0,2,1,3,215,5,9,2,0,9,3,1,7,6,2,7,7,3,9,4,4,8,0,1,5,40,79,6,5,9,0,0,9,1,1,2,4,24,28,2147483821,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,16,0,5,0,16,0,4,1,16,0,3,2,16,0,0,3,16,0,1,4,16,0,2,5,8,0,5,0,8,0,4,1,3,36,7,32,6,8,0,3,0,12,195,1,25,0,4,25,0,5,3,34,6,9,3,0,8,0,0,1,3,371,4,9,0,0,9,1,1,9,2,2,2,4,303,30,0,15,1,9,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,15,1,14,5,13,0,27,32,26,0,8,1,13,1,8,1,14,2,4,303], key:0}}, clause_keys:[0], next_key:1, key:356}, 357: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,16,0,3,0,16,0,2,1,8,0,3,0,13,1,25,0,4,25,0,1,3,13,5,9,4,0,8,0,0,1,3,24,5,7,10,0,7,11,1,7,12,2,9,0,3,7,13,4,7,14,5,7,15,6,9,1,7,9,2,8,9,3,9,2,41,80,10,28,2147483875,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,33,7,16,0,6,0,16,0,8,1,16,0,5,2,16,0,9,3,16,0,1,4,16,0,3,5,16,0,2,6,16,0,4,7,16,0,0,8,15,1,14,9,8,0,6,0,13,1,27,58,26,0,8,0,8,2,3,303,10,9,8,0,8,0,5,1,9,9,2,3,303,10,32,7,8,0,1,0,9,6,1,3,24,7,8,0,3,0,9,5,1,3,24,6,8,0,2,0,13,1,25,0,3,25,0,4,3,13,5,9,0,0,12,208,1,25,0,1,25,0,2,2,4,9,30,0,15,1,12,0,15,1,13,1,15,1,14,2,15,1,15,3,15,1,16,4,15,1,17,5,15,1,18,6,15,1,19,7,15,1,20,8,15,1,21,9,8,1,20,0,8,1,21,1,4,9], key:0}}, clause_keys:[0], next_key:1, key:357}, 359: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,8,1,5,0,10,679,1,8,1,6,2,4,373], key:0}}, clause_keys:[0], next_key:1, key:359}, 360: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,16,0,7,0,16,0,6,1,15,1,7,2,16,0,3,3,8,1,7,0,14,7,1,8,0,0,2,3,356,8,9,7,0,14,5,1,8,0,1,2,3,356,8,9,6,0,8,0,4,1,3,357,7,8,0,5,0,40,180,1,6,12,338,0,25,0,2,9,4,1,12,358,3,25,0,5,12,284,5,26,128,12,285,7,26,530,12,286,9,26,128,13,8,24,1,9,26,0,13,6,24,1,7,24,1,8,13,4,24,1,5,24,1,6,13,2,24,1,3,24,1,4,3,59,6,13,4,25,0,2,26,0,13,3,26,628,24,1,4,13,2,25,0,1,24,1,3,13,0,25,0,0,24,1,2,9,3,1,2,4,359,28,2147483851,1,60,0,33,0,15,1,3,0,8,1,3,0,3,334,1,32,0,2,4,62,30,0,15,1,3,0,8,1,3,0,14,10,1,4,9], key:0}}, clause_keys:[0], next_key:1, key:360}, 361: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,33,0,16,0,1,0,8,0,2,0,3,362,3,9,2,0,8,0,1,1,7,3,2,3,66,3,9,1,0,12,363,7,23,1,8,13,18,26,646,26,0,13,17,26,647,24,1,18,13,16,26,648,24,1,17,13,15,26,649,24,1,16,13,14,26,650,24,1,15,13,13,26,651,24,1,14,13,12,26,652,24,1,13,13,11,26,653,24,1,12,13,10,26,654,24,1,11,13,9,26,655,24,1,10,13,6,24,1,7,24,1,9,13,5,26,423,24,1,6,13,4,26,426,24,1,5,13,3,26,622,24,1,4,13,2,26,656,24,1,3,13,1,26,657,24,1,2,3,324,2,32,0,2,5], key:0}, 1:{code:[30,0,1,60,0,16,0,0,0,10,658,0,3,64,1,9,0,0,2,4,361], key:1}}, clause_keys:[0, 1], next_key:2, key:361}, 362: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,2,0,8,1,2,0,3,126,1,32,0,2,5], key:0}, 1:{code:[30,0,1,60,0,16,0,0,0,3,370,1,9,0,0,2,4,126], key:1}}, clause_keys:[0, 1], next_key:2, key:362}, 364: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,16,0,0,0,17,409,1,16,0,2,2,16,0,3,3,16,0,1,4,15,1,6,5,31,9,1,0,9,2,1,9,3,2,3,339,4,9,0,0,3,368,1,10,668,0,2,4,122], key:0}, 1:{code:[29,2,1,15,1,6,0,17,479,1,15,1,7,2,15,1,8,3,15,1,9,4,15,1,10,5,31,10,568,0,2,4,122], key:1}, 2:{code:[30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,15,1,10,4,15,1,11,5,31,5], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:364}, 366: {is_public:false, clauses:{0:{code:[28,1,17,0,0,31,5], key:0}, 1:{code:[30,0,1,60,0,15,1,3,0,8,1,3,0,8,0,0,1,3,305,1,10,672,0,3,64,1,9,0,0,14,1,1,2,4,369], key:1}}, clause_keys:[0, 1], next_key:2, key:366}, 368: {is_public:false, clauses:{0:{code:[28,1,1,17,657,0,10,593,0,3,132,0,2,4,96], key:0}, 1:{code:[29,2,1,17,649,0,10,669,0,3,132,0,2,4,95], key:1}, 2:{code:[29,3,1,17,648,0,10,669,0,3,132,0,2,4,96], key:2}, 3:{code:[29,4,1,17,647,0,10,568,0,3,132,0,2,4,95], key:3}, 4:{code:[30,0,1,17,646,0,10,568,0,3,132,0,2,4,96], key:4}, 5:{code:[254,0,44,0,2147483657,1000000,1000000,1000000,1000000,45,0,5,646,4,647,3,648,2,649,1,657,0], key:5}}, clause_keys:[0, 1, 2, 3, 4, 5], next_key:6, key:368, index: 5}, 369: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,2,1,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,20,0,23,1,6,25,0,0,19,195,6,23,1,7,23,1,8,16,0,2,1,8,0,2,0,8,1,8,1,8,1,7,2,8,0,3,3,3,360,4,9,3,0,3,64,4,8,0,1,0,12,198,1,25,0,2,27,1,3,34,3,9,0,0,9,1,1,2,4,369], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:369, index: 2}, 370: {is_public:false, clauses:{0:{code:[28,1,1,10,128,0,3,125,0,2,4,237], key:0}, 1:{code:[30,0,10,530,0,4,125], key:1}}, clause_keys:[0, 1], next_key:2, key:370}, 371: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,15,1,4,0,16,0,0,1,8,0,0,0,8,1,4,1,3,215,2,13,0,27,32,26,0,13,1,25,0,1,26,0,3,9,2,9,0,0,9,1,1,2,4,372], key:0}}, clause_keys:[0], next_key:1, key:371}, 372: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,2,1,5], key:0}, 1:{code:[30,0,20,0,23,1,4,23,1,5,16,1,4,1,8,1,5,0,8,1,4,1,4,372], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:372, index: 2}, 373: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,16,1,3,2,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,20,0,23,1,5,25,0,0,15,1,6,1,16,0,2,2,8,1,5,0,8,1,6,1,8,0,1,2,7,7,3,7,8,4,7,9,5,40,62,6,3,9,0,0,9,1,1,9,2,2,2,4,373,28,2147483768,1,60,0,60,1,60,2,60,3,33,3,16,0,1,0,16,0,0,1,16,0,2,2,15,1,8,3,15,1,9,4,15,1,10,5,8,0,1,0,3,4,4,32,3,9,0,0,9,1,1,9,2,2,2,4,20,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,8,0,8,1,12,1,8,1,13,2,8,1,9,3,8,1,10,4,41,169,5,28,2147483895,1,60,0,60,1,60,2,60,3,60,4,60,5,33,5,16,0,4,0,16,0,3,1,16,0,1,2,16,0,0,3,16,0,2,4,8,0,4,0,3,260,6,32,5,9,4,0,8,0,3,1,3,26,5,8,0,1,0,9,3,1,3,24,4,9,0,0,9,1,1,9,2,2,2,4,20,30,0,1,60,0,60,1,60,2,15,1,8,0,15,1,9,1,16,0,1,2,16,0,0,3,16,0,2,4,12,338,0,25,0,1,10,680,1,13,2,25,1,8,26,0,3,79,3,9,0,0,9,1,1,9,2,2,2,4,20], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:373, index: 2}, 374: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:374}, 375: {is_public:false, clauses:{0:{code:[254,0,15,1,2,0,13,0,25,1,2,23,1,3,4,374], key:0}}, clause_keys:[0], next_key:1, key:375}, 377: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,0,0,8,0,1,0,40,44,1,2,13,2,25,0,0,25,0,1,12,376,1,24,1,2,12,208,0,26,681,24,1,1,2,4,262,28,2147483725,1,60,0,33,0,15,1,3,0,12,376,4,25,1,3,12,208,0,26,681,24,1,4,3,265,1,32,0,2,4,62,30,0,15,1,3,0,8,1,3,0,10,0,1,4,9], key:0}}, clause_keys:[0], next_key:1, key:377}, 378: {is_public:false, clauses:{0:{code:[254,0,1,60,0,15,1,2,0,13,4,25,1,2,25,0,0,12,376,3,24,1,4,12,208,0,26,681,24,1,3,3,265,1,12,376,1,25,0,0,12,208,0,26,681,24,1,1,2,4,262], key:0}}, clause_keys:[0], next_key:1, key:378}, 379: {is_public:false, clauses:{0:{code:[254,0,1,60,0,15,1,3,0,8,1,3,0,8,0,0,1,3,24,1,7,5,0,7,6,1,7,7,2,9,0,3,7,8,4,2,41,39,5,28,2147483754,1,60,0,60,1,60,2,60,3,33,0,15,1,9,0,16,0,2,1,16,0,1,2,16,0,3,3,15,1,10,4,13,0,27,58,26,0,8,1,9,1,8,0,2,2,3,303,4,9,1,0,9,2,1,9,3,2,3,303,4,32,0,2,4,62,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,13,0,27,47,26,0,8,1,12,1,8,1,11,2,4,303], key:0}}, clause_keys:[0], next_key:1, key:379}, 380: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,15,1,4,0,16,0,4,1,8,1,4,0,8,0,1,1,3,24,5,13,0,27,47,26,0,7,3,1,8,0,2,2,3,303,5,13,0,27,47,26,0,8,0,3,1,8,0,0,2,3,303,5,7,9,0,9,0,1,9,1,2,7,10,3,7,11,4,9,2,5,9,3,6,7,12,7,9,4,8,2,41,96,9,28,2147483856,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,33,3,16,0,2,0,15,1,13,1,15,1,14,2,16,0,4,3,16,0,5,4,16,0,6,5,16,0,7,6,16,0,1,7,16,0,0,8,8,0,2,0,8,1,13,1,8,1,14,2,3,303,8,9,5,0,9,6,1,9,7,2,40,255,3,8,32,3,9,2,0,13,1,27,47,26,0,8,0,1,2,3,303,3,9,0,0,9,1,1,2,4,24,30,0,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,15,1,15,4,15,1,16,5,15,1,17,6,15,1,18,7,15,1,19,8,8,1,19,0,10,698,1,4,9,28,2147483942,1,60,0,33,0,15,1,7,0,15,1,8,1,15,1,9,2,8,1,7,0,8,1,8,1,8,1,9,2,3,303,1,32,0,2,4,61,30,0,15,1,3,0,15,1,4,1,15,1,5,2,4,62], key:0}}, clause_keys:[0], next_key:1, key:380}, 381: {is_public:false, clauses:{0:{code:[28,1,17,698,0,15,1,3,1,16,1,3,2,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,16,0,0,0,15,1,5,1,16,0,2,2,8,1,5,0,8,0,1,1,3,24,3,9,0,0,9,1,1,9,2,2,2,4,383], key:1}}, clause_keys:[0, 1], next_key:2, key:381}, 382: {is_public:false, clauses:{0:{code:[254,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,7,6,2,7,7,3,7,8,4,7,9,5,7,10,6,41,36,7,28,2147483741,1,60,0,60,1,60,2,33,2,16,0,0,0,16,0,1,1,15,1,9,2,15,1,10,3,15,1,11,4,15,1,12,5,15,1,13,6,8,0,0,0,3,379,3,32,2,9,0,0,9,1,1,2,4,9,30,0,15,1,9,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,15,1,14,5,15,1,15,6,8,1,12,0,8,1,13,1,8,1,9,2,8,1,10,3,8,1,14,4,8,1,15,5,41,150,6,28,2147483868,1,60,0,60,1,60,2,60,3,60,4,33,4,16,0,3,0,16,0,0,1,16,0,1,2,16,0,2,3,15,1,8,4,15,1,9,5,8,0,3,0,3,375,5,32,4,9,3,0,8,0,0,1,3,380,4,9,0,0,9,1,1,9,2,2,2,4,381,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,10,0,8,1,13,1,8,1,11,2,41,261,3,254,0,1,60,0,60,1,60,2,33,2,15,1,7,0,16,0,1,1,16,0,0,2,8,1,7,0,8,0,1,1,10,0,2,3,84,3,32,2,9,0,0,9,1,1,2,4,9], key:0}}, clause_keys:[0], next_key:1, key:382}, 383: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,15,1,7,2,7,8,0,8,1,6,1,8,1,5,2,7,9,3,7,10,4,8,1,7,5,7,11,6,7,12,7,41,44,8,28,2147483806,1,60,0,60,1,60,2,60,3,60,4,60,5,33,5,16,0,3,0,15,1,12,1,16,0,4,2,16,0,0,3,16,0,1,4,16,0,2,5,15,1,13,6,15,1,14,7,13,16,27,47,26,0,13,15,27,46,24,1,16,13,0,27,46,24,1,15,8,0,3,1,8,1,12,2,3,303,6,32,5,9,4,0,8,0,0,1,3,384,5,8,0,1,0,9,3,1,3,24,4,9,0,0,9,1,1,9,2,2,2,4,381,30,0,1,60,0,60,1,60,2,60,3,15,1,10,0,16,0,3,1,15,1,11,2,15,1,12,3,15,1,13,4,16,0,0,5,16,0,2,6,16,0,1,7,8,1,11,0,8,0,2,1,3,24,4,9,2,0,9,3,1,8,0,1,2,3,303,4,9,0,0,9,1,1,2,4,24], key:0}}, clause_keys:[0], next_key:1, key:383}, 384: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,15,1,4,0,16,0,0,1,8,1,4,0,8,0,7,1,3,24,8,9,7,0,8,0,6,1,3,305,8,13,0,27,47,26,0,8,0,5,1,9,6,2,3,303,7,7,3,0,13,1,27,47,26,0,8,0,4,2,3,303,6,9,4,0,8,0,3,1,9,5,2,3,303,6,13,0,27,47,26,0,9,3,1,8,0,2,2,3,303,4,9,2,0,8,0,1,1,3,305,3,9,0,0,9,1,1,2,4,24], key:0}}, clause_keys:[0], next_key:1, key:384}, 385: {is_public:false, clauses:{0:{code:[28,1,1,15,1,3,0,15,1,4,1,8,1,3,0,3,118,0,2,4,237], key:0}, 1:{code:[30,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,4,119], key:1}}, clause_keys:[0, 1], next_key:2, key:385}, 386: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,6,1,15,1,7,2,16,1,7,3,15,1,8,4,16,1,8,5,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,20,0,23,1,6,25,0,0,19,403,6,25,0,7,16,0,6,1,16,0,8,2,16,0,3,3,16,0,4,4,16,0,5,5,31,8,0,7,0,8,0,6,1,9,8,2,8,0,2,3,3,92,9,12,404,0,25,0,7,25,0,6,3,234,8,8,0,1,0,12,198,1,25,0,6,27,2,3,34,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,386], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,20,0,23,1,6,25,0,0,19,405,6,25,0,7,25,0,10,25,0,11,16,0,6,1,16,0,2,2,16,0,3,3,20,4,23,1,7,25,0,4,19,392,7,25,0,7,25,0,8,16,0,5,5,31,12,406,1,25,0,7,25,0,10,25,0,11,25,0,6,12,407,0,24,1,1,3,234,12,8,0,8,0,12,198,1,25,0,6,27,1,3,34,12,8,0,6,0,13,1,27,40,26,0,3,398,12,8,0,9,0,12,198,1,25,0,6,27,2,3,34,12,9,9,0,13,2,25,0,11,26,0,13,1,25,0,10,24,1,2,3,398,12,12,408,1,25,0,8,12,409,0,25,0,7,24,1,1,3,234,9,8,0,1,0,12,198,1,25,0,6,27,4,3,34,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,386], key:2}, 3:{code:[29,4,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,20,0,23,1,6,25,0,0,19,410,6,25,0,10,25,0,8,16,0,6,1,16,0,2,2,16,0,3,3,20,4,23,1,7,25,0,4,19,392,7,25,0,10,25,0,9,16,0,5,5,31,12,411,1,25,0,10,25,0,8,25,0,6,12,407,0,24,1,1,3,234,11,9,9,0,12,198,1,25,0,6,27,1,3,34,10,8,0,6,0,13,1,27,41,26,0,3,398,9,8,0,7,0,12,198,1,25,0,6,27,2,3,34,9,9,7,0,13,1,25,0,8,26,0,3,398,9,8,0,1,0,12,198,1,25,0,6,27,3,3,34,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,386], key:3}, 4:{code:[29,5,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,20,0,23,1,6,25,0,0,19,412,6,25,0,8,16,0,6,1,16,0,2,2,16,0,3,3,20,4,23,1,7,25,0,4,19,395,7,25,0,8,25,0,7,16,0,5,5,31,12,413,0,25,0,8,25,0,6,3,234,9,9,7,0,12,198,1,25,0,6,27,1,3,34,8,8,0,6,0,13,1,27,28,26,0,3,398,7,8,0,1,0,12,198,1,25,0,6,27,2,3,34,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,386], key:4}, 5:{code:[29,6,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,20,0,23,1,6,25,0,0,19,414,6,25,0,8,16,0,6,1,16,0,2,2,16,0,3,3,20,4,23,1,7,25,0,4,19,395,7,25,0,8,25,0,7,16,0,5,5,31,12,415,0,25,0,8,25,0,6,3,234,9,9,7,0,12,198,1,25,0,6,27,1,3,34,8,8,0,6,0,13,1,27,29,26,0,3,398,7,8,0,1,0,12,198,1,25,0,6,27,2,3,34,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,386], key:5}, 6:{code:[29,7,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,60,15,60,16,60,17,60,18,60,19,60,20,60,21,60,22,60,23,20,0,23,1,6,25,0,0,19,416,6,25,0,22,25,0,20,25,0,17,25,0,14,25,0,11,25,0,8,16,0,23,1,16,0,2,2,16,0,3,3,16,0,21,4,16,0,5,5,31,12,417,0,25,0,22,25,0,20,25,0,17,25,0,14,25,0,11,25,0,8,25,0,23,3,234,24,8,0,19,0,12,198,1,25,0,23,27,1,3,34,24,9,23,0,13,1,27,44,26,0,3,398,24,9,22,0,7,3,1,8,0,19,2,40,305,3,23,9,19,0,8,0,16,1,9,20,2,9,21,3,8,0,18,4,3,418,22,9,16,0,8,0,13,1,9,17,2,9,18,3,8,0,15,4,3,418,19,9,13,0,8,0,10,1,9,14,2,9,15,3,8,0,12,4,3,418,16,9,10,0,8,0,7,1,9,11,2,9,12,3,8,0,9,4,3,418,13,9,7,0,8,0,6,1,9,8,2,9,9,3,8,0,4,4,3,418,10,8,0,1,0,12,198,1,25,0,6,27,1,3,34,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,386,28,2147484004,1,60,0,60,1,60,2,33,2,15,1,6,0,16,0,1,1,16,0,0,2,8,1,6,0,10,126,1,3,9,3,32,2,8,0,1,0,3,402,2,9,0,0,9,1,1,2,4,87,30,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,7,0,8,1,5,1,4,87], key:6}, 7:{code:[29,8,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,20,0,23,1,6,25,0,0,19,419,6,25,0,6,25,0,7,16,0,11,1,16,0,9,2,16,0,3,3,16,0,10,4,16,0,5,5,31,12,420,0,25,0,6,25,0,7,25,0,11,3,234,12,8,0,8,0,12,198,1,25,0,11,27,1,3,34,12,9,11,0,13,1,27,45,26,0,3,398,12,9,6,0,9,7,1,9,8,2,8,0,1,3,9,9,4,8,0,2,5,9,10,6,8,0,4,7,3,421,11,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,386], key:7}, 8:{code:[29,9,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,20,0,23,1,6,25,0,0,19,422,6,25,0,6,25,0,7,16,0,11,1,16,0,9,2,16,0,3,3,16,0,10,4,16,0,5,5,31,12,423,0,25,0,6,25,0,7,25,0,11,3,234,12,8,0,8,0,12,198,1,25,0,11,27,1,3,34,12,9,11,0,13,1,27,46,26,0,3,398,12,9,6,0,9,7,1,9,8,2,8,0,1,3,9,9,4,8,0,2,5,9,10,6,8,0,4,7,3,421,11,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,386], key:8}, 9:{code:[29,10,1,60,0,60,1,60,2,60,3,60,4,60,5,20,0,23,1,6,25,0,0,19,424,6,23,1,7,16,0,1,1,20,2,23,1,8,25,0,2,19,390,8,24,1,7,25,0,1,16,0,3,3,16,0,4,4,16,0,5,5,31,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,386], key:9}, 10:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,20,0,25,0,9,25,0,0,16,0,6,1,16,0,2,2,16,0,3,3,16,0,4,4,16,0,5,5,31,12,425,0,25,0,9,25,0,6,3,234,10,9,9,0,8,0,7,1,8,0,8,2,3,426,10,8,0,6,0,9,8,1,3,398,9,8,0,1,0,12,198,1,25,0,6,25,0,7,3,34,8,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,386], key:10}, 11:{code:[254,0,44,0,2147483657,1000000,1000000,2147483662,1000000,45,0,1,0,0,71,1,72,2,72,3,72,4,72,5,72,6,72,7,72,8,72,9,73,10], key:11}}, clause_keys:[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11], next_key:12, key:386, index: 11}, 388: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,2,1,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,20,0,23,1,5,25,0,0,19,390,5,23,1,6,23,1,7,16,0,1,1,8,1,6,0,8,1,7,1,8,0,1,2,3,391,2,9,0,0,9,1,1,2,4,388], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:388, index: 2}, 389: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,16,0,3,0,16,0,4,1,8,0,3,0,8,0,2,1,3,63,5,9,3,0,9,4,1,8,0,0,2,10,0,3,8,0,1,4,10,0,5,3,386,5,12,387,0,25,0,2,25,0,0,3,234,3,9,0,0,9,1,1,3,388,2,10,724,0,2,4,234], key:0}}, clause_keys:[0], next_key:1, key:389}, 391: {is_public:false, clauses:{0:{code:[28,1,15,1,3,0,15,1,4,1,17,0,2,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,33,5,16,0,0,0,16,0,1,1,20,2,23,1,5,25,0,2,19,392,5,25,0,4,25,0,3,8,0,0,0,8,0,4,1,3,10,6,32,5,12,393,1,25,0,3,12,394,0,25,0,4,25,0,1,24,1,1,3,234,5,9,3,0,8,0,1,1,3,87,4,9,0,0,9,1,1,9,2,2,2,4,391], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,60,4,60,5,33,5,16,0,0,0,16,0,1,1,20,2,23,1,5,25,0,2,19,395,5,25,0,4,25,0,3,8,0,0,0,8,0,4,1,3,10,6,32,5,12,393,1,25,0,3,12,396,0,25,0,4,25,0,1,24,1,1,3,234,5,9,3,0,12,397,1,25,0,1,3,87,4,9,0,0,9,1,1,9,2,2,2,4,391], key:2}, 3:{code:[30,0,15,1,6,0,15,1,7,1,20,2,23,1,8,23,1,9,8,1,6,0,8,1,7,1,8,1,9,2,4,391], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:391}, 398: {is_public:false, clauses:{0:{code:[28,1,15,1,2,0,17,0,1,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,16,0,2,0,20,1,23,1,4,25,0,1,8,0,2,0,8,1,4,1,3,87,3,8,0,0,0,12,198,1,25,0,2,27,1,3,34,3,9,0,0,9,1,1,2,4,398], key:1}}, clause_keys:[0, 1], next_key:2, key:398}, 400: {is_public:false, clauses:{0:{code:[28,1,19,399,0,23,1,3,20,1,27,0,23,1,4,20,4,24,1,3,23,1,5,16,1,5,2,5], key:0}, 1:{code:[30,0,19,401,0,23,1,3,20,1,27,1,23,1,4,20,4,24,1,3,23,1,5,16,1,5,2,5], key:1}, 2:{code:[254,0,44,0,1000000,1000000,1000000,1000000,2147483657,46,0,2,399,0,401,1], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:400, index: 2}, 402: {is_public:false, clauses:{0:{code:[254,0,21,1000000,0,5], key:0}}, clause_keys:[0], next_key:1, key:402}, 418: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,15,1,7,0,16,0,2,1,16,0,0,2,16,0,1,3,16,0,3,4,8,0,2,0,12,198,1,25,1,7,27,1,3,34,4,9,0,0,9,1,1,9,2,2,9,3,3,7,8,4,7,9,5,7,10,6,7,11,7,2,41,74,8,28,2147483801,1,60,0,60,1,60,2,60,3,60,4,33,4,16,0,1,0,16,0,0,1,16,0,2,2,16,0,3,3,15,1,10,4,15,1,11,5,15,1,12,6,15,1,13,7,8,0,1,0,3,3,5,32,4,9,0,0,12,395,2,25,0,1,25,0,2,13,1,24,1,2,25,0,3,2,4,9,30,0,15,1,10,0,15,1,11,1,15,1,12,2,15,1,13,3,15,1,14,4,15,1,15,5,15,1,16,6,15,1,17,7,8,1,10,0,8,1,15,1,8,1,12,2,8,1,11,3,8,1,13,4,8,1,16,5,8,1,17,6,41,218,7,28,2147483946,1,60,0,60,1,60,2,60,3,60,4,33,4,15,1,10,0,16,0,3,1,16,0,2,2,16,0,0,3,16,0,1,4,15,1,11,5,15,1,12,6,8,1,10,0,10,126,1,3,9,5,32,4,8,0,3,0,3,402,4,9,2,0,9,3,1,3,87,4,9,0,0,9,1,1,2,4,9,30,0,15,1,9,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,15,1,14,5,15,1,15,6,8,1,9,0,8,1,15,1,8,1,11,2,8,1,12,3,8,1,13,4,41,351,5,28,2147484067,1,60,0,60,1,60,2,60,3,60,4,33,4,15,1,8,0,16,0,3,1,16,0,2,2,16,0,0,3,16,0,1,4,8,1,8,0,12,427,1,25,0,3,3,9,5,32,4,9,2,0,9,3,1,3,87,4,9,0,0,9,1,1,2,4,9,30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,15,1,10,4,12,428,0,25,1,6,4,143], key:0}}, clause_keys:[0], next_key:1, key:418}, 421: {is_public:false, clauses:{0:{code:[254,0,15,1,10,0,15,1,11,1,15,1,12,2,15,1,13,3,15,1,14,4,15,1,15,5,15,1,16,6,15,1,17,7,8,1,10,0,8,1,12,1,7,18,2,8,1,11,3,8,1,13,4,8,1,14,5,8,1,15,6,8,1,16,7,8,1,17,8,41,72,9,28,2147483845,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,33,9,16,0,0,0,16,0,8,1,16,0,2,2,16,0,1,3,16,0,3,4,16,0,4,5,16,0,5,6,16,0,6,7,16,0,7,8,8,0,0,0,14,15,1,3,35,10,32,9,8,0,8,0,14,1,1,3,87,9,8,0,2,0,12,198,1,25,0,8,27,1,3,34,9,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,2,4,429,30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,16,0,0,0,16,0,8,1,16,0,2,2,16,0,1,3,16,0,3,4,16,0,6,5,16,0,7,6,16,0,4,7,16,0,5,8,8,0,8,0,14,0,1,3,87,9,8,0,2,0,12,198,1,25,0,8,27,1,3,34,9,9,6,0,9,7,1,3,9,8,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,430], key:0}}, clause_keys:[0], next_key:1, key:421}, 426: {is_public:false, clauses:{0:{code:[28,1,19,449,0,23,1,3,21,0,1,17,0,2,5], key:0}, 1:{code:[29,2,17,790,0,21,1,1,20,2,27,1,26,0,5], key:1}, 2:{code:[29,3,17,791,0,21,1,1,20,2,27,2,26,0,5], key:2}, 3:{code:[29,4,19,450,0,23,1,6,23,1,7,19,217,6,23,1,8,23,1,9,21,3,1,20,2,27,3,23,1,10,20,10,23,1,11,23,1,12,20,12,24,1,7,26,0,8,1,8,0,8,1,9,1,8,1,11,2,4,90], key:3}, 4:{code:[29,5,19,451,0,23,1,6,19,217,6,23,1,7,23,1,8,21,2,1,20,2,27,4,23,1,9,20,9,23,1,10,26,0,8,1,7,0,8,1,8,1,8,1,10,2,4,90], key:4}, 5:{code:[29,6,17,793,0,21,1,1,20,2,27,5,26,0,5], key:5}, 6:{code:[29,7,19,452,0,23,1,3,23,1,4,19,399,3,23,1,5,19,401,4,23,1,6,21,3,1,20,2,27,6,23,1,7,20,7,24,1,5,23,1,8,20,8,24,1,6,26,0,5], key:6}, 7:{code:[29,8,19,453,0,23,1,3,19,399,3,23,1,4,21,2,1,20,2,27,60,23,1,5,20,5,24,1,4,26,0,5], key:7}, 8:{code:[29,9,19,452,0,23,1,3,23,1,4,19,401,3,23,1,5,19,401,4,23,1,6,21,3,1,20,2,27,7,23,1,7,20,7,24,1,5,23,1,8,20,8,24,1,6,26,0,5], key:8}, 9:{code:[29,10,19,454,0,23,1,6,23,1,7,19,401,7,23,1,8,21,4,1,20,2,27,8,23,1,9,8,1,6,0,8,1,9,1,13,2,24,1,8,26,0,4,400], key:9}, 10:{code:[29,11,19,455,0,23,1,3,23,1,4,19,399,3,23,1,5,19,401,4,23,1,6,21,3,1,20,2,27,9,23,1,7,20,7,24,1,5,23,1,8,20,8,24,1,6,26,0,5], key:10}, 11:{code:[29,12,19,455,0,23,1,4,23,1,5,19,401,4,23,1,6,19,401,5,23,1,7,21,3,1,20,2,27,9,23,1,8,20,8,24,1,6,23,1,9,20,9,24,1,7,26,0,12,401,10,24,1,6,12,456,0,24,1,10,4,143], key:11}, 12:{code:[29,13,19,457,0,23,1,5,23,1,6,19,401,6,23,1,7,21,3,1,20,2,27,10,23,1,8,20,8,23,1,9,23,1,10,20,10,24,1,7,26,0,8,1,5,0,8,1,9,1,4,88], key:12}, 13:{code:[29,14,19,458,0,23,1,3,19,401,3,23,1,4,21,2,1,20,2,27,11,23,1,5,20,5,24,1,4,26,0,5], key:13}, 14:{code:[29,15,19,459,0,23,1,6,23,1,7,19,217,6,23,1,8,23,1,9,19,401,7,23,1,10,21,3,1,20,2,27,12,23,1,11,20,11,23,1,12,23,1,13,20,13,24,1,10,26,0,8,1,8,0,8,1,9,1,8,1,12,2,4,90], key:14}, 15:{code:[29,16,19,460,0,23,1,3,19,401,3,23,1,4,21,2,1,20,2,27,13,23,1,5,20,5,24,1,4,26,0,5], key:15}, 16:{code:[29,17,19,461,0,23,1,3,23,1,4,19,401,4,23,1,5,21,3,1,20,2,27,14,23,1,6,20,6,24,1,3,23,1,7,20,7,24,1,5,26,0,5], key:16}, 17:{code:[29,18,19,462,0,23,1,5,23,1,6,19,401,6,23,1,7,21,3,1,20,2,27,51,23,1,8,20,8,23,1,9,23,1,10,20,10,24,1,7,26,0,8,1,5,0,8,1,9,1,4,89], key:17}, 18:{code:[29,19,19,463,0,23,1,6,23,1,7,19,401,7,23,1,8,21,4,1,20,2,27,15,23,1,9,8,1,6,0,8,1,9,1,13,2,24,1,8,26,0,4,400], key:18}, 19:{code:[29,20,19,464,0,23,1,6,23,1,7,19,401,7,23,1,8,21,4,1,20,2,27,16,23,1,9,8,1,6,0,8,1,9,1,13,2,24,1,8,26,0,4,400], key:19}, 20:{code:[29,21,19,465,0,23,1,5,23,1,6,19,401,6,23,1,7,21,3,1,20,2,27,17,23,1,8,20,8,23,1,9,23,1,10,20,10,24,1,7,26,0,8,1,5,0,8,1,9,1,4,88], key:20}, 21:{code:[29,22,19,466,0,23,1,3,19,401,3,23,1,4,21,2,1,20,2,27,18,23,1,5,20,5,24,1,4,26,0,5], key:21}, 22:{code:[29,23,19,467,0,23,1,6,23,1,7,19,217,6,23,1,8,23,1,9,19,401,7,23,1,10,21,3,1,20,2,27,19,23,1,11,20,11,23,1,12,23,1,13,20,13,24,1,10,26,0,8,1,8,0,8,1,9,1,8,1,12,2,4,90], key:22}, 23:{code:[29,24,19,468,0,23,1,3,19,401,3,23,1,4,21,2,1,20,2,27,20,23,1,5,20,5,24,1,4,26,0,5], key:23}, 24:{code:[29,25,19,469,0,23,1,3,23,1,4,19,401,4,23,1,5,21,3,1,20,2,27,21,23,1,6,20,6,24,1,3,23,1,7,20,7,24,1,5,26,0,5], key:24}, 25:{code:[29,26,19,470,0,23,1,5,23,1,6,19,401,6,23,1,7,21,3,1,20,2,27,50,23,1,8,20,8,23,1,9,23,1,10,20,10,24,1,7,26,0,8,1,5,0,8,1,9,1,4,89], key:25}, 26:{code:[29,27,19,471,0,23,1,3,21,2,1,20,2,27,22,23,1,4,20,4,24,1,3,26,0,5], key:26}, 27:{code:[29,28,19,472,0,23,1,6,21,3,1,20,2,27,23,23,1,7,8,1,6,0,8,1,7,1,10,0,2,4,400], key:27}, 28:{code:[29,29,19,473,0,23,1,6,21,3,1,20,2,27,24,23,1,7,8,1,6,0,8,1,7,1,10,0,2,4,400], key:28}, 29:{code:[29,30,19,474,0,23,1,6,21,3,1,20,2,27,25,23,1,7,8,1,6,0,8,1,7,1,10,0,2,4,400], key:29}, 30:{code:[29,31,19,475,0,23,1,5,21,2,1,20,2,27,26,23,1,6,20,6,23,1,7,26,0,8,1,5,0,8,1,7,1,4,88], key:30}, 31:{code:[29,32,19,476,0,23,1,3,21,2,1,20,2,27,27,23,1,4,20,4,24,1,3,26,0,5], key:31}, 32:{code:[29,33,19,477,0,23,1,5,21,2,1,20,2,27,52,23,1,6,20,6,23,1,7,26,0,8,1,5,0,8,1,7,1,4,89], key:32}, 33:{code:[29,34,19,412,0,23,1,3,21,2,1,20,2,27,28,23,1,4,20,4,24,1,3,26,0,5], key:33}, 34:{code:[29,35,19,414,0,23,1,3,21,2,1,20,2,27,29,23,1,4,20,4,24,1,3,26,0,5], key:34}, 35:{code:[29,36,17,819,0,21,2,1,20,2,27,30,23,1,3,20,3,27,0,26,0,5], key:35}, 36:{code:[29,37,19,478,0,23,1,3,23,1,4,23,1,5,23,1,6,21,5,1,20,2,27,44,23,1,7,20,7,24,1,3,23,1,8,20,8,24,1,4,23,1,9,20,9,24,1,5,23,1,10,20,10,24,1,6,26,0,5], key:36}, 37:{code:[29,38,19,419,0,23,1,5,23,1,6,15,1,7,1,20,2,27,45,23,1,8,20,8,24,1,5,24,1,6,8,1,7,0,12,479,9,27,2,24,1,5,12,198,1,27,2,24,1,9,4,34], key:37}, 38:{code:[29,39,19,422,0,23,1,5,23,1,6,15,1,7,1,20,2,27,46,23,1,8,20,8,24,1,5,24,1,6,8,1,7,0,12,479,9,27,2,24,1,5,12,198,1,27,2,24,1,9,4,34], key:38}, 39:{code:[29,40,19,480,0,23,1,3,21,2,1,20,2,27,71,23,1,4,20,4,24,1,3,26,0,5], key:39}, 40:{code:[29,41,19,481,0,23,1,3,21,2,1,20,2,27,72,23,1,4,20,4,24,1,3,26,0,5], key:40}, 41:{code:[29,42,19,482,0,23,1,3,21,2,1,20,2,27,73,23,1,4,20,4,24,1,3,26,0,5], key:41}, 42:{code:[29,43,19,483,0,23,1,3,21,2,1,20,2,27,74,23,1,4,20,4,24,1,3,26,0,5], key:42}, 43:{code:[29,44,17,825,0,21,1,1,20,2,27,31,26,0,5], key:43}, 44:{code:[29,45,19,484,0,23,1,3,19,399,3,23,1,4,21,2,1,20,2,27,32,23,1,5,20,5,24,1,4,26,0,5], key:44}, 45:{code:[29,46,19,485,0,23,1,3,19,399,3,23,1,4,21,2,1,20,2,27,33,23,1,5,20,5,24,1,4,26,0,5], key:45}, 46:{code:[29,47,19,405,0,23,1,3,23,1,4,23,1,5,21,4,1,20,2,27,40,23,1,6,20,6,24,1,3,23,1,7,20,7,24,1,4,23,1,8,20,8,24,1,5,26,0,5], key:46}, 47:{code:[29,48,19,410,0,23,1,3,23,1,4,21,3,1,20,2,27,41,23,1,5,20,5,24,1,3,23,1,6,20,6,24,1,4,26,0,5], key:47}, 48:{code:[29,49,17,828,0,21,1,1,20,2,27,42,26,0,5], key:48}, 49:{code:[29,50,19,486,0,23,1,3,23,1,4,19,399,4,23,1,5,21,3,1,20,2,27,43,23,1,6,20,6,24,1,3,23,1,7,20,7,24,1,5,26,0,5], key:49}, 50:{code:[30,0,17,830,0,21,2,1,20,2,27,254,23,1,3,20,3,27,0,26,0,5], key:50}, 51:{code:[254,0,44,0,2147483657,1000000,1000000,1000000,2147483674,45,0,7,790,1,791,2,793,5,819,35,825,43,828,48,830,50,46,1,64,1000000,2147483741,2147483744,2147483747,2147483750,2147483753,2147483756,2147483759,1000000,2147483762,2147483765,2147483768,2147483771,2147483774,2147483777,2147483780,2147483783,2147483786,2147483789,2147483792,2147483795,2147483798,2147483803,2147483806,2147483809,2147483812,2147483815,2147483820,2147483823,2147483828,2147483831,1000000,2147483836,2147483839,2147483842,2147483845,2147483850,2147483853,2147483856,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1000000,1,449,0,1,450,3,1,451,4,1,452,2147483861,1,453,7,1,454,9,1,455,2147483865,1,457,12,1,458,13,1,459,14,1,460,15,1,461,16,1,462,17,1,463,18,1,464,19,1,465,20,1,466,21,1,467,22,1,468,23,2,405,46,469,24,1,470,25,1,471,26,1,472,27,1,473,28,2,410,47,474,29,1,475,30,2,412,33,476,31,1,477,32,2,414,34,478,36,1,480,39,1,481,40,1,482,41,2,419,37,483,42,1,484,44,1,485,45,2,422,38,486,49,71,6,73,8,71,10,73,11], key:51}}, clause_keys:[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48, 49, 50, 51], next_key:52, key:426, index: 51}, 429: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,60,15,60,16,60,17,16,0,17,0,16,0,13,1,16,0,15,2,16,0,3,3,16,0,4,4,16,0,5,5,16,0,9,6,16,0,7,7,8,0,8,0,12,198,1,25,0,15,27,1,3,34,18,8,0,16,0,12,431,5,25,0,17,12,431,6,27,2,12,217,4,24,1,5,24,1,6,12,432,3,24,1,4,12,198,2,24,1,3,27,1,12,433,1,24,1,2,27,8,3,34,18,8,0,11,0,12,434,1,27,1,25,0,16,3,34,17,9,15,0,8,0,11,1,3,87,16,8,0,14,0,12,195,1,25,0,11,27,1,3,34,15,9,13,0,9,14,1,8,0,12,2,3,435,15,12,195,0,23,1,3,23,1,4,12,195,7,24,1,3,23,1,5,12,436,6,24,1,7,25,0,12,12,199,1,24,1,5,24,1,6,24,1,4,8,0,10,2,3,199,13,9,10,0,9,11,1,8,0,0,2,3,437,12,8,0,0,0,8,0,1,1,3,438,10,8,0,1,0,9,8,1,8,0,2,2,9,9,3,8,0,6,4,3,439,10,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,2,4,440], key:0}}, clause_keys:[0], next_key:1, key:429}, 430: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,15,1,8,0,16,0,0,1,16,0,5,2,16,0,2,3,16,0,3,4,16,0,4,5,8,0,5,0,8,1,8,1,3,87,6,8,0,1,0,12,198,1,25,0,5,27,1,3,34,6,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,446], key:0}}, clause_keys:[0], next_key:1, key:430}, 435: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,17,0,2,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,20,0,23,1,6,25,0,0,19,195,6,23,1,7,23,1,8,16,0,1,1,20,2,23,1,9,25,0,2,19,195,9,23,1,10,23,1,11,19,195,11,24,1,7,24,1,8,8,1,7,0,8,0,1,1,8,1,10,2,3,441,3,9,0,0,9,1,1,9,2,2,2,4,435], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:435, index: 2}, 437: {is_public:false, clauses:{0:{code:[254,0,15,1,7,0,15,1,8,1,15,1,9,2,8,1,7,0,14,0,1,8,1,8,2,8,1,9,3,4,443], key:0}}, clause_keys:[0], next_key:1, key:437}, 438: {is_public:false, clauses:{0:{code:[28,1,17,0,0,17,0,1,5], key:0}, 1:{code:[30,0,1,60,0,60,1,20,0,23,1,4,25,0,0,19,195,4,23,1,5,23,1,6,20,1,23,1,7,25,0,1,8,1,6,0,8,1,7,1,40,53,2,2,9,0,0,9,1,1,2,4,438,28,2147483739,1,60,0,60,1,33,1,15,1,5,0,16,0,0,1,8,1,5,0,10,0,1,3,9,2,32,1,9,0,0,10,126,1,2,4,9,30,0,15,1,2,0,15,1,3,1,4,62], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:438, index: 2}, 439: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,5,1,16,1,5,2,15,1,6,3,16,1,6,4,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,20,0,23,1,7,25,0,0,16,0,5,1,16,0,2,2,15,1,8,3,16,0,4,4,8,1,7,0,8,1,8,1,8,0,5,2,8,0,3,3,7,9,4,40,95,5,6,8,0,1,0,12,198,1,25,0,5,27,1,3,34,6,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,439,28,2147483810,1,60,0,60,1,60,2,60,3,60,4,33,4,16,0,1,0,16,0,0,1,16,0,2,2,16,0,3,3,15,1,7,4,8,0,1,0,3,3,5,32,4,9,0,0,12,395,2,25,0,1,25,0,2,13,1,24,1,2,25,0,3,2,4,9,30,0,1,60,0,60,1,15,1,7,0,15,1,8,1,16,0,0,2,15,1,9,3,16,0,1,4,8,1,8,0,8,1,9,1,3,9,2,8,0,1,0,3,402,2,9,0,0,9,1,1,2,4,87], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:439, index: 2}, 440: {is_public:false, clauses:{0:{code:[28,1,17,0,0,17,0,1,15,1,8,2,16,1,8,3,15,1,9,4,16,1,9,5,15,1,10,6,16,1,10,7,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,33,12,20,0,23,1,9,25,0,0,19,195,9,23,1,10,25,0,9,20,1,23,1,11,25,0,1,16,0,10,2,16,0,3,3,20,4,23,1,12,25,0,4,19,390,12,24,1,11,25,0,10,16,0,5,5,16,0,11,6,16,0,7,7,8,1,11,0,3,3,13,32,12,8,0,9,0,8,0,8,1,3,215,12,9,8,0,9,9,1,9,10,2,8,0,2,3,9,11,4,8,0,6,5,3,430,12,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,2,4,440], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,33,8,20,0,23,1,10,25,0,0,20,1,23,1,11,25,0,1,16,0,2,2,16,0,3,3,16,0,4,4,16,0,5,5,16,0,6,6,16,0,7,7,8,1,11,0,10,126,1,3,10,9,32,8,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,2,4,440], key:2}, 3:{code:[254,0,44,0,2147483657,1000000,1000000,2147483662,1000000,45,0,1,0,0,71,1,73,2], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:440, index: 3}, 441: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,7,0,12,442,8,25,1,5,25,1,6,12,198,1,24,1,8,27,1,4,34], key:0}}, clause_keys:[0], next_key:1, key:441}, 443: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,7,1,15,1,8,2,15,1,9,3,8,1,7,0,8,1,8,1,8,1,9,2,4,444], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,20,0,23,1,6,25,0,7,19,195,6,25,0,4,25,0,6,15,1,7,1,16,0,2,2,16,0,5,3,8,0,1,0,12,198,1,25,1,7,27,1,3,34,8,9,4,0,8,0,1,1,9,5,2,9,6,3,8,0,3,4,8,0,0,5,9,7,6,7,9,7,7,10,8,40,112,9,8,9,0,0,9,1,1,9,2,2,9,3,3,2,4,443,28,2147483860,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,16,0,3,0,15,1,12,1,16,0,2,2,16,0,4,3,16,0,5,4,16,0,0,5,16,0,1,6,15,1,13,7,15,1,14,8,8,0,3,0,8,1,12,1,3,9,7,32,6,9,2,0,12,195,2,25,0,3,25,0,4,13,1,24,1,2,25,0,5,3,9,6,9,0,0,9,1,1,2,4,9,30,0,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,15,1,15,4,15,1,16,5,15,1,17,6,15,1,18,7,15,1,19,8,8,1,11,0,8,1,12,1,8,1,13,2,8,1,15,3,8,1,14,4,8,1,17,5,8,1,16,6,8,1,19,7,41,285,8,28,2147484044,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,33,7,16,0,0,0,16,0,5,1,16,0,4,2,16,0,6,3,16,0,1,4,16,0,2,5,16,0,3,6,15,1,11,7,8,0,0,0,8,0,5,1,3,35,8,32,7,9,4,0,12,195,2,25,0,5,26,0,13,1,24,1,2,25,0,6,3,9,7,12,195,2,25,0,0,25,0,1,13,0,24,1,2,25,0,2,9,3,1,2,4,9,30,0,15,1,10,0,15,1,11,1,15,1,12,2,15,1,13,3,15,1,14,4,15,1,15,5,15,1,16,6,15,1,17,7,8,1,10,0,8,1,11,1,41,441,2,254,0,1,60,0,60,1,33,1,16,0,0,0,15,1,5,1,8,0,0,0,8,1,5,1,3,36,2,32,1,12,445,0,25,0,0,2,4,143], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:443, index: 2}, 444: {is_public:false, clauses:{0:{code:[28,1,15,1,3,0,16,1,3,1,17,0,2,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,16,0,3,0,16,0,1,1,20,2,23,1,5,25,0,2,19,195,5,25,0,0,26,0,8,0,3,0,8,0,1,1,3,36,4,8,0,0,0,12,198,1,25,0,3,27,1,3,34,4,9,0,0,9,1,1,9,2,2,2,4,444], key:1}}, clause_keys:[0, 1], next_key:2, key:444}, 446: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,5,1,16,1,5,2,15,1,6,3,16,1,6,4,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,20,0,23,1,10,25,0,0,15,1,11,1,16,0,2,2,15,1,12,3,16,0,4,4,8,1,10,0,8,1,11,1,8,0,1,2,8,1,12,3,8,0,3,4,3,447,5,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,446], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:446, index: 2}, 447: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,19,195,0,25,0,4,25,0,0,16,0,6,1,16,0,5,2,16,0,1,3,16,0,3,4,8,0,6,0,8,0,4,1,3,87,7,8,0,2,0,12,198,1,25,0,6,27,1,3,34,7,9,5,0,12,198,1,25,0,2,27,1,3,34,6,9,0,0,9,1,1,9,2,2,9,3,3,7,7,4,7,8,5,9,4,6,2,41,107,7,28,2147483830,1,60,0,60,1,60,2,60,3,60,4,33,4,16,0,1,0,16,0,0,1,16,0,2,2,16,0,3,3,15,1,9,4,15,1,10,5,15,1,11,6,8,0,1,0,3,3,5,32,4,9,0,0,12,395,2,25,0,1,25,0,2,13,1,24,1,2,25,0,3,2,4,9,30,0,15,1,9,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,15,1,14,5,15,1,15,6,8,1,9,0,8,1,14,1,8,1,11,2,8,1,10,3,8,1,12,4,8,1,15,5,41,239,6,28,2147483959,1,60,0,60,1,60,2,60,3,60,4,33,4,15,1,9,0,16,0,3,1,16,0,2,2,16,0,0,3,16,0,1,4,15,1,10,5,8,1,9,0,12,427,1,25,0,3,3,9,5,32,4,9,2,0,9,3,1,3,87,4,9,0,0,9,1,1,2,4,9,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,15,1,12,5,12,448,0,25,1,12,25,1,7,4,143], key:0}}, clause_keys:[0], next_key:1, key:447}, 487: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,15,1,6,0,16,0,3,1,16,0,4,2,16,0,0,3,8,1,6,0,8,0,2,1,3,24,5,9,2,0,13,1,25,0,3,25,0,4,8,0,1,2,3,303,5,9,0,0,9,1,1,2,4,24], key:0}}, clause_keys:[0], next_key:1, key:487}, 488: {is_public:false, clauses:{0:{code:[254,0,17,838,0,5], key:0}}, clause_keys:[0], next_key:1, key:488}, 489: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,1,0,16,0,2,1,8,0,0,0,3,488,3,9,0,0,9,1,1,9,2,2,2,4,20], key:0}}, clause_keys:[0], next_key:1, key:489}, 490: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,2,0,16,0,1,1,13,0,27,47,26,0,13,1,25,0,0,26,0,3,9,3,7,5,0,9,0,1,9,1,2,9,2,3,7,6,4,2,41,52,5,28,2147483767,1,60,0,60,1,60,2,60,3,33,0,15,1,9,0,16,0,2,1,16,0,3,2,15,1,10,3,16,0,1,4,8,1,9,0,13,1,25,0,2,25,0,3,8,1,10,2,3,303,4,9,2,0,9,3,1,40,151,2,4,32,0,2,4,62,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,10,0,8,1,9,1,4,9,28,2147483830,1,60,0,33,0,15,1,5,0,15,1,6,1,8,1,5,0,8,1,6,1,3,324,1,32,0,2,4,61,30,0,15,1,2,0,15,1,3,1,4,62], key:0}}, clause_keys:[0], next_key:1, key:490}, 491: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,7,0,8,1,6,1,8,1,5,2,41,29,3,28,2147483716,1,60,0,33,0,15,1,7,0,15,1,8,1,15,1,9,2,8,1,7,0,8,1,8,1,8,1,9,2,3,303,1,32,0,2,4,62,30,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,5,0,8,1,7,1,4,9], key:0}}, clause_keys:[0], next_key:1, key:491}, 492: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,15,1,4,0,16,0,0,1,8,1,4,0,8,0,3,1,3,24,4,9,3,0,8,0,2,1,3,490,4,9,2,0,13,4,27,108,26,0,13,3,27,112,24,1,4,13,1,27,46,24,1,3,8,0,1,2,3,491,3,9,0,0,9,1,1,2,4,24], key:0}}, clause_keys:[0], next_key:1, key:492}, 493: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,16,1,3,2,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,20,0,23,1,5,25,0,0,15,1,6,1,16,0,2,2,8,0,1,0,12,198,1,25,1,6,27,1,3,34,3,9,0,0,9,1,1,9,2,2,2,4,493], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:493, index: 2}, 494: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,8,1,5,0,14,0,1,8,1,6,2,4,493], key:0}}, clause_keys:[0], next_key:1, key:494}, 495: {is_public:false, clauses:{0:{code:[254,0,15,1,1,0,5], key:0}}, clause_keys:[0], next_key:1, key:495}, 496: {is_public:false, clauses:{0:{code:[28,1,17,0,0,5], key:0}, 1:{code:[30,0,1,60,0,20,0,23,1,2,25,0,0,8,1,2,0,3,498,1,9,0,0,2,4,496], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:496, index: 2}, 497: {is_public:false, clauses:{0:{code:[254,0,7,2,0,7,3,1,41,11,2,28,2147483695,1,60,0,60,1,33,1,15,1,5,0,16,0,0,1,40,59,0,2,32,1,8,0,0,0,3,112,1,9,0,0,2,4,496,30,0,15,1,2,0,15,1,3,1,4,62,28,2147483725,1,60,0,33,0,10,613,0,3,110,1,32,0,2,4,61,30,0,4,62], key:0}}, clause_keys:[0], next_key:1, key:497}, 498: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,16,0,1,0,8,0,2,0,8,0,3,1,8,0,1,2,3,90,4,10,857,0,3,495,4,12,217,1,25,0,2,25,0,3,12,499,0,24,1,1,3,495,4,8,0,1,0,8,0,0,1,3,114,2,9,0,0,9,1,1,2,4,500], key:0}}, clause_keys:[0], next_key:1, key:498}, 500: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,2,1,31,5], key:0}, 1:{code:[29,2,1,60,0,20,0,23,1,2,26,0,16,0,0,1,31,9,0,0,2,4,113], key:1}, 2:{code:[30,0,1,60,0,20,0,23,1,4,23,1,5,20,5,23,1,6,23,1,7,16,0,0,1,13,8,24,1,6,24,1,7,13,0,24,1,4,24,1,8,8,0,0,1,3,501,1,9,0,0,2,4,113], key:2}, 3:{code:[254,0,44,0,2147483657,1000000,1000000,2147483662,1000000,45,0,1,0,0,71,1,73,2], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:500, index: 3}, 501: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,16,0,3,0,16,0,1,1,8,0,2,0,13,1,25,0,4,25,0,5,3,9,6,9,3,0,9,4,1,9,5,2,3,502,6,9,2,0,8,0,0,1,3,503,3,9,0,0,7,6,1,7,7,2,7,8,3,7,9,4,9,1,5,2,41,82,6,28,2147483781,1,60,0,60,1,33,1,16,0,0,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,15,1,12,5,8,0,0,0,3,504,2,32,1,12,505,0,25,0,0,2,4,495,30,0,1,60,0,60,1,60,2,60,3,60,4,16,0,2,0,15,1,9,1,16,0,4,2,16,0,3,3,16,0,1,4,16,0,0,5,8,0,2,0,8,1,9,1,8,0,4,2,3,506,5,8,0,4,0,3,495,5,3,86,5,9,4,0,14,0,1,3,389,5,8,0,3,0,3,103,4,9,3,0,3,495,4,9,2,0,8,0,1,1,3,507,3,9,1,0,8,0,0,1,3,116,2,9,0,0,2,4,115], key:0}}, clause_keys:[0], next_key:1, key:501}, 502: {is_public:false, clauses:{0:{code:[28,1,17,0,0,17,0,1,17,0,2,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,20,0,23,1,8,25,0,0,15,1,9,1,15,1,10,2,8,1,8,0,8,1,9,1,8,0,1,2,8,1,10,3,8,0,2,4,3,509,3,9,0,0,9,1,1,9,2,2,2,4,502], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:502, index: 2}, 503: {is_public:false, clauses:{0:{code:[28,1,17,0,0,17,0,1,5], key:0}, 1:{code:[30,0,1,60,0,60,1,20,0,23,1,5,25,0,0,15,1,6,1,8,1,5,0,8,1,6,1,8,0,1,2,3,511,2,9,0,0,9,1,1,2,4,503], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:503, index: 2}, 504: {is_public:false, clauses:{0:{code:[28,1,17,0,0,5], key:0}, 1:{code:[30,0,20,0,23,1,2,23,1,3,20,2,23,1,4,26,0,8,1,3,0,4,504], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:504, index: 2}, 506: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,17,0,2,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,3,20,0,23,1,4,25,0,0,20,4,25,0,5,26,0,15,1,5,1,20,2,23,1,6,23,1,7,19,424,6,25,1,5,20,7,25,0,4,23,1,8,20,8,23,1,9,25,0,2,19,483,9,25,0,6,8,0,5,0,3,510,7,32,3,9,5,0,12,195,1,25,0,6,23,1,2,3,9,7,8,0,3,0,8,0,0,1,7,6,2,7,7,3,9,4,4,8,0,1,5,40,135,6,5,9,0,0,9,1,1,9,2,2,2,4,512,28,2147483847,1,60,0,60,1,60,2,33,2,15,1,9,0,15,1,10,1,15,1,11,2,15,1,12,3,16,0,0,4,16,0,1,5,8,1,10,0,13,1,25,1,11,25,1,12,3,9,3,32,2,9,0,0,12,412,1,25,0,1,2,4,9,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,12,0,10,830,1,4,9], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,20,0,25,0,3,25,0,0,15,1,5,1,20,2,23,1,6,23,1,7,19,424,6,25,1,5,20,7,23,1,8,23,1,9,20,9,23,1,10,25,0,10,19,416,10,25,0,4,25,0,5,25,0,6,25,0,7,25,0,8,25,0,9,8,0,0,0,7,11,1,7,12,2,8,1,8,3,8,0,1,4,40,153,5,11,9,3,0,9,4,1,9,5,2,9,6,3,9,7,4,9,8,5,9,9,6,9,10,7,8,0,2,8,3,513,11,9,0,0,9,1,1,9,2,2,2,4,512,28,2147483861,1,60,0,60,1,60,2,33,2,15,1,8,0,15,1,9,1,15,1,10,2,16,0,0,3,16,0,1,4,8,1,8,0,13,1,25,1,9,25,1,10,3,9,3,32,2,9,0,0,12,412,1,25,0,1,2,4,9,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,10,0,10,830,1,4,9], key:2}, 3:{code:[254,0,44,0,2147483657,1000000,1000000,2147483662,1000000,45,0,1,0,0,71,1,73,2], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:506, index: 3}, 507: {is_public:false, clauses:{0:{code:[28,1,17,0,0,17,0,1,5], key:0}, 1:{code:[30,0,1,60,0,60,1,20,0,23,1,4,25,0,0,20,1,23,1,5,25,0,1,8,1,4,0,8,1,5,1,3,508,2,9,0,0,9,1,1,2,4,507], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:507, index: 2}, 508: {is_public:false, clauses:{0:{code:[28,1,17,0,0,17,0,1,5], key:0}, 1:{code:[30,0,20,0,23,1,4,23,1,5,19,195,4,23,1,6,23,1,7,20,1,24,1,6,23,1,8,8,1,5,0,8,1,8,1,4,508], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:508, index: 2}, 509: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,6,0,17,0,1,15,1,7,2,20,3,23,1,8,23,1,9,20,8,25,1,6,26,0,20,9,25,1,7,23,1,10,16,1,10,4,8,1,6,0,3,510,1,32,0,2,5], key:0}, 1:{code:[30,0,15,1,6,0,20,1,25,1,6,23,1,7,16,1,7,2,15,1,8,3,16,1,8,4,8,1,6,0,41,33,1,28,2147483704,1,60,0,33,0,15,1,3,0,8,1,3,0,3,510,1,32,0,2,4,61,30,0,15,1,1,0,4,62], key:1}}, clause_keys:[0, 1], next_key:2, key:509}, 510: {is_public:false, clauses:{0:{code:[254,0,1,60,0,19,195,0,23,1,4,23,1,5,19,217,5,23,1,6,23,1,7,14,1,0,8,1,6,1,8,0,0,2,3,12,1,9,0,0,2,4,3], key:0}}, clause_keys:[0], next_key:1, key:510}, 511: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,16,1,3,2,5], key:0}, 1:{code:[30,0,20,0,23,1,3,23,1,4,20,1,23,1,5,23,1,6,20,5,24,1,3,24,1,4,16,1,6,2,5], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:511, index: 2}, 512: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,17,0,2,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,3,20,0,23,1,4,25,0,0,20,4,25,0,5,26,0,15,1,5,1,20,2,23,1,6,23,1,7,19,424,6,25,1,5,20,7,25,0,4,23,1,8,20,8,23,1,9,25,0,2,19,483,9,25,0,6,8,0,5,0,3,510,7,32,3,9,5,0,12,195,1,25,0,6,23,1,2,3,9,7,8,0,3,0,8,0,0,1,7,6,2,7,7,3,9,4,4,8,0,1,5,40,135,6,5,9,0,0,9,1,1,9,2,2,2,4,512,28,2147483847,1,60,0,60,1,60,2,33,2,15,1,9,0,15,1,10,1,15,1,11,2,15,1,12,3,16,0,0,4,16,0,1,5,8,1,10,0,13,1,25,1,11,25,1,12,3,9,3,32,2,9,0,0,12,414,1,25,0,1,2,4,9,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,12,0,10,819,1,4,9], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,20,0,25,0,3,25,0,0,15,1,5,1,20,2,23,1,6,23,1,7,19,424,6,25,1,5,20,7,23,1,8,23,1,9,20,9,23,1,10,25,0,10,19,416,10,25,0,4,25,0,5,25,0,6,25,0,7,25,0,8,25,0,9,8,0,0,0,7,11,1,7,12,2,8,1,8,3,8,0,1,4,40,153,5,11,9,3,0,9,4,1,9,5,2,9,6,3,9,7,4,9,8,5,9,9,6,9,10,7,8,0,2,8,3,513,11,9,0,0,9,1,1,9,2,2,2,4,512,28,2147483861,1,60,0,60,1,60,2,33,2,15,1,8,0,15,1,9,1,15,1,10,2,16,0,0,3,16,0,1,4,8,1,8,0,13,1,25,1,9,25,1,10,3,9,3,32,2,9,0,0,12,414,1,25,0,1,2,4,9,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,10,0,10,819,1,4,9], key:2}, 3:{code:[254,0,44,0,2147483657,1000000,1000000,2147483662,1000000,45,0,1,0,0,71,1,73,2], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:512, index: 3}, 513: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,60,15,60,16,60,17,60,18,60,19,60,20,60,21,60,22,60,23,60,24,60,25,20,0,25,0,24,25,0,25,15,1,11,1,16,0,11,2,16,0,8,3,16,0,5,4,16,0,17,5,16,0,1,6,16,0,12,7,16,0,15,8,8,0,24,0,12,195,1,25,1,11,23,1,12,3,9,26,13,0,25,0,24,25,0,25,8,0,23,1,8,0,21,2,8,0,19,3,8,0,16,4,8,0,13,5,3,514,26,9,23,0,10,11,1,8,0,10,2,8,0,3,3,8,0,22,4,3,515,24,9,21,0,10,13,1,8,0,7,2,9,22,3,8,0,20,4,3,515,23,9,19,0,10,15,1,8,0,4,2,9,20,3,8,0,18,4,3,515,21,9,16,0,10,890,1,13,2,25,0,17,26,0,9,18,3,8,0,14,4,3,515,19,9,13,0,10,891,1,8,0,0,2,9,14,3,9,15,4,3,515,16,9,10,0,10,892,1,9,11,2,9,12,3,8,0,9,4,3,516,13,9,7,0,10,892,1,9,8,2,9,9,3,8,0,6,4,3,516,10,9,4,0,10,892,1,9,5,2,9,6,3,8,0,2,4,3,516,7,9,0,0,10,891,1,9,1,2,9,2,3,9,3,4,2,4,516], key:0}}, clause_keys:[0], next_key:1, key:513}, 514: {is_public:false, clauses:{0:{code:[28,1,17,0,0,17,0,1,17,0,2,17,0,3,17,0,4,17,0,5,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,20,0,23,1,7,25,0,0,20,1,24,1,7,25,0,1,16,0,2,2,16,0,3,3,16,0,4,4,16,0,5,5,8,1,7,0,3,517,7,32,6,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,514], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,20,0,23,1,7,25,0,0,16,0,1,1,20,2,24,1,7,25,0,2,16,0,3,3,16,0,4,4,16,0,5,5,8,1,7,0,3,518,7,32,6,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,514], key:2}, 3:{code:[29,4,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,20,0,23,1,7,25,0,0,16,0,1,1,16,0,2,2,20,3,24,1,7,25,0,3,16,0,4,4,16,0,5,5,8,1,7,0,3,519,7,32,6,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,514], key:3}, 4:{code:[29,5,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,20,0,23,1,7,25,0,0,16,0,1,1,16,0,2,2,16,0,3,3,20,4,24,1,7,25,0,4,16,0,5,5,8,1,7,0,3,520,7,32,6,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,514], key:4}, 5:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,20,0,23,1,7,25,0,0,16,0,1,1,16,0,2,2,16,0,3,3,16,0,4,4,20,5,24,1,7,25,0,5,8,1,7,0,3,521,7,32,6,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,514], key:5}, 6:{code:[254,0,44,0,2147483657,1000000,1000000,2147483662,1000000,45,0,1,0,0,71,1,72,2,72,3,72,4,73,5], key:6}}, clause_keys:[0, 1, 2, 3, 4, 5, 6], next_key:7, key:514, index: 6}, 515: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,16,0,0,0,17,890,1,16,0,1,2,16,0,2,3,16,0,3,4,31,9,0,0,9,1,1,9,2,2,9,3,3,2,4,522], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,15,1,7,0,15,1,8,1,16,0,1,2,16,0,2,3,16,0,3,4,7,9,0,8,1,8,1,8,1,7,2,8,0,0,3,7,10,4,40,75,5,4,9,0,0,7,6,1,7,7,2,9,1,3,9,2,4,9,3,5,2,41,264,6,28,2147483773,1,60,0,60,1,60,2,33,2,15,1,8,0,15,1,9,1,16,0,0,2,16,0,1,3,15,1,10,4,8,1,9,0,40,162,1,3,32,2,9,0,0,9,1,1,2,4,523,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,8,0,8,1,9,1,8,1,10,2,41,220,3,28,2147483825,15,1,3,0,8,1,3,0,10,11,1,4,9,30,0,15,1,3,0,8,1,3,0,41,190,1,28,2147483853,15,1,3,0,8,1,3,0,10,13,1,4,9,30,0,15,1,3,0,8,1,3,0,10,15,1,4,9,254,0,1,60,0,60,1,60,2,33,2,15,1,6,0,16,0,0,1,16,0,1,2,8,1,6,0,10,891,1,3,9,3,32,2,9,0,0,9,1,1,2,4,524,28,2147483994,1,60,0,60,1,60,2,60,3,60,4,60,5,33,4,15,1,10,0,16,0,5,1,16,0,0,2,16,0,1,3,16,0,2,4,16,0,3,5,8,1,10,0,10,0,1,8,0,5,2,3,525,6,9,5,0,8,0,0,1,3,247,6,32,4,9,0,0,9,1,1,9,2,2,9,3,3,2,4,526,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,12,0,8,1,13,1,4,9], key:1}}, clause_keys:[0, 1], next_key:2, key:515}, 516: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,5,1,17,126,2,15,1,6,3,16,1,6,4,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,16,0,2,0,16,0,0,1,15,1,7,2,20,3,23,1,8,23,1,9,19,424,8,25,1,7,20,9,25,0,3,23,1,10,16,1,10,4,8,0,2,0,8,0,1,1,3,215,4,9,0,0,9,1,1,9,2,2,9,3,3,2,4,534], key:1}}, clause_keys:[0, 1], next_key:2, key:516}, 517: {is_public:false, clauses:{0:{code:[254,0,1,60,0,19,195,0,23,1,4,23,1,5,19,217,5,23,1,6,23,1,7,14,1,0,8,1,6,1,8,0,0,2,3,12,1,9,0,0,2,4,4], key:0}}, clause_keys:[0], next_key:1, key:517}, 518: {is_public:false, clauses:{0:{code:[254,0,1,60,0,19,195,0,23,1,4,23,1,5,19,217,5,23,1,6,23,1,7,14,1,0,8,1,6,1,8,0,0,2,3,12,1,9,0,0,2,4,5], key:0}}, clause_keys:[0], next_key:1, key:518}, 519: {is_public:false, clauses:{0:{code:[254,0,1,60,0,19,195,0,23,1,4,23,1,5,19,217,5,23,1,6,23,1,7,14,1,0,8,1,6,1,8,0,0,2,3,12,1,9,0,0,2,4,6], key:0}}, clause_keys:[0], next_key:1, key:519}, 520: {is_public:false, clauses:{0:{code:[254,0,19,195,0,23,1,3,23,1,4,19,217,4,23,1,5,23,1,6,8,1,5,0,13,9,23,1,10,23,1,11,13,8,24,1,9,23,1,12,13,1,23,1,7,24,1,8,4,13], key:0}}, clause_keys:[0], next_key:1, key:520}, 521: {is_public:false, clauses:{0:{code:[254,0,19,195,0,23,1,3,23,1,4,19,217,4,23,1,5,23,1,6,8,1,5,0,7,7,1,4,530], key:0}}, clause_keys:[0], next_key:1, key:521}, 522: {is_public:false, clauses:{0:{code:[28,1,17,0,0,20,1,26,126,26,0,15,1,4,2,16,1,4,3,31,5], key:0}, 1:{code:[29,2,20,0,23,1,4,26,0,19,195,4,23,1,5,23,1,6,20,1,23,1,7,26,0,19,427,7,24,1,5,15,1,8,2,16,1,8,3,31,5], key:1}, 2:{code:[30,0,20,0,23,1,7,23,1,8,20,8,23,1,9,23,1,10,20,1,23,1,11,26,0,20,2,23,1,12,23,1,13,19,424,12,24,1,11,15,1,14,3,13,15,24,1,9,24,1,10,13,0,24,1,7,24,1,15,8,1,13,1,8,1,14,2,4,531], key:2}, 3:{code:[254,0,44,0,2147483657,1000000,1000000,2147483662,1000000,45,0,1,0,0,71,1,73,2], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:522, index: 3}, 523: {is_public:false, clauses:{0:{code:[28,1,17,0,0,17,0,1,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,20,0,23,1,4,25,0,0,20,1,23,1,5,25,0,1,19,195,5,25,0,3,24,1,4,8,1,4,0,12,217,7,25,0,2,23,1,8,12,195,1,23,1,6,24,1,7,3,9,4,9,2,0,9,3,1,3,529,4,9,0,0,9,1,1,2,4,523], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:523, index: 2}, 524: {is_public:false, clauses:{0:{code:[28,1,17,0,0,17,0,1,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,20,0,23,1,4,25,0,0,20,1,23,1,5,25,0,1,19,195,5,25,0,3,24,1,4,8,1,4,0,12,217,7,25,0,2,23,1,8,12,195,1,23,1,6,24,1,7,3,9,4,9,2,0,9,3,1,3,530,4,9,0,0,9,1,1,2,4,524], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:524, index: 2}, 525: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,16,1,3,2,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,20,0,23,1,7,25,0,0,19,195,7,23,1,8,23,1,9,15,1,10,1,16,0,2,2,8,1,8,0,8,1,9,1,8,1,10,2,8,0,1,3,3,527,3,9,0,0,9,1,1,9,2,2,2,4,525], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:525, index: 2}, 526: {is_public:false, clauses:{0:{code:[28,1,17,0,0,17,0,1,15,1,4,2,16,1,4,3,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,20,0,23,1,8,25,0,0,20,1,23,1,9,25,0,1,15,1,10,2,16,0,3,3,8,1,8,0,8,1,9,1,8,1,10,2,8,0,2,3,3,532,4,9,0,0,9,1,1,9,2,2,9,3,3,2,4,526], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:526, index: 2}, 527: {is_public:false, clauses:{0:{code:[254,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,8,1,6,0,7,10,1,8,1,8,2,7,11,3,8,1,9,4,8,1,7,5,41,43,6,28,2147483773,1,60,0,60,1,60,2,60,3,33,3,15,1,11,0,16,0,1,1,15,1,12,2,16,0,2,3,15,1,13,4,16,0,0,5,12,195,0,25,1,11,25,0,1,8,1,12,1,12,195,2,24,1,11,25,0,2,8,1,13,3,3,528,4,32,3,13,0,25,0,0,25,0,1,9,2,1,2,4,247,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,12,0,13,15,25,1,13,26,0,12,195,14,25,1,8,24,1,15,13,1,24,1,14,25,1,10,4,9], key:0}}, clause_keys:[0], next_key:1, key:527}, 528: {is_public:false, clauses:{0:{code:[28,1,15,1,4,0,20,1,25,1,4,23,1,5,15,1,6,2,20,3,25,1,6,24,1,5,5], key:0}, 1:{code:[30,0,15,1,8,0,20,1,23,1,9,23,1,10,15,1,11,2,20,3,24,1,9,23,1,12,8,1,8,0,8,1,10,1,8,1,11,2,8,1,12,3,4,528], key:1}}, clause_keys:[0, 1], next_key:2, key:528}, 529: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,33,0,15,1,5,0,16,0,2,1,14,1,0,8,1,5,1,8,0,1,2,3,12,3,9,1,0,9,2,1,7,5,2,7,6,3,7,7,4,40,56,5,3,32,0,2,5,28,2147483753,1,60,0,60,1,60,2,33,2,16,0,0,0,16,0,1,1,15,1,7,2,15,1,8,3,15,1,9,4,8,0,0,0,3,4,3,32,2,9,0,0,9,1,1,2,4,88,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,7,0,8,1,8,1,8,1,10,2,8,1,11,3,41,146,4,28,2147483839,1,60,0,60,1,60,2,33,2,16,0,1,0,16,0,0,1,15,1,6,2,15,1,7,3,8,0,1,0,3,5,3,32,2,9,0,0,9,1,1,2,4,9,30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,8,1,6,0,8,1,7,1,8,1,9,2,41,224,3,28,2147483913,1,60,0,60,1,60,2,33,2,16,0,0,0,16,0,1,1,15,1,5,2,8,0,0,0,3,6,3,32,2,9,0,0,9,1,1,2,4,89,30,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,5,0,8,1,6,1,41,290,2,254,0,1,60,0,60,1,60,2,33,2,16,0,0,0,16,0,1,1,8,0,0,0,10,0,1,3,10,3,32,2,9,0,0,9,1,1,2,4,9], key:0}}, clause_keys:[0], next_key:1, key:529}, 530: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,15,1,5,0,16,0,2,1,14,1,0,8,1,5,1,8,0,3,2,3,12,4,9,3,0,8,0,0,1,8,0,1,2,3,11,4,9,0,0,9,1,1,9,2,2,2,4,90], key:0}}, clause_keys:[0], next_key:1, key:530}, 531: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,20,0,23,1,3,23,1,4,19,195,3,23,1,5,23,1,6,20,4,25,0,0,25,0,1,20,1,23,1,7,25,0,2,19,480,7,24,1,5,16,0,3,2,31,13,0,25,0,0,25,0,1,9,2,1,9,3,2,2,4,533], key:0}, 1:{code:[30,0,20,0,23,1,3,26,0,19,195,3,23,1,4,23,1,5,20,1,23,1,6,23,1,7,19,482,6,24,1,4,16,1,7,2,5], key:1}, 2:{code:[254,0,44,0,1000000,1000000,1000000,2147483657,1000000,71,0,73,1], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:531, index: 2}, 532: {is_public:false, clauses:{0:{code:[28,1,19,195,0,23,1,4,23,1,5,20,5,23,1,6,26,0,19,195,6,23,1,7,23,1,8,19,195,1,24,1,4,23,1,9,19,427,9,24,1,7,15,1,10,2,16,1,10,3,31,5], key:0}, 1:{code:[30,0,19,195,0,23,1,7,23,1,8,20,8,23,1,9,23,1,10,20,10,23,1,11,23,1,12,19,195,1,24,1,7,23,1,13,20,2,23,1,14,23,1,15,19,424,14,24,1,13,15,1,16,3,13,17,24,1,11,24,1,12,13,0,24,1,9,24,1,17,8,1,15,1,8,1,16,2,4,531], key:1}, 2:{code:[254,0,44,0,1000000,1000000,1000000,1000000,2147483657,46,0,1,195,2147483662,71,0,73,1], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:532, index: 2}, 533: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,20,0,23,1,3,23,1,4,19,195,3,23,1,5,23,1,6,20,4,25,0,0,25,0,1,20,1,23,1,7,25,0,2,19,481,7,24,1,5,16,0,3,2,31,13,0,25,0,0,25,0,1,9,2,1,9,3,2,2,4,533], key:0}, 1:{code:[30,0,20,0,23,1,3,26,0,19,195,3,23,1,4,23,1,5,20,1,23,1,6,23,1,7,19,482,6,24,1,4,16,1,7,2,5], key:1}, 2:{code:[254,0,44,0,1000000,1000000,1000000,2147483657,1000000,71,0,73,1], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:533, index: 2}, 534: {is_public:false, clauses:{0:{code:[28,1,17,892,0,15,1,6,1,15,1,7,2,19,419,3,25,1,6,23,1,8,8,1,7,0,8,1,8,1,4,247], key:0}, 1:{code:[30,0,17,891,0,15,1,6,1,15,1,7,2,19,422,3,25,1,6,23,1,8,8,1,7,0,8,1,8,1,4,247], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1000000,1000000,45,0,2,891,1,892,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:534, index: 2}, 535: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:535}, 536: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:536}, 538: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,33,0,16,0,3,0,16,0,4,1,8,0,3,0,7,4,1,7,5,2,7,6,3,7,7,4,7,8,5,7,9,6,7,10,7,7,11,8,40,116,9,5,8,0,2,0,3,218,5,12,217,1,26,933,27,2,12,208,0,25,0,2,24,1,1,3,29,5,8,0,1,0,12,537,2,25,0,3,25,0,4,12,208,1,25,0,2,24,1,2,3,9,5,9,1,0,3,222,2,32,0,2,5,28,2147483836,1,60,0,60,1,60,2,33,2,15,1,12,0,15,1,13,1,16,0,1,2,16,0,0,3,15,1,14,4,15,1,15,5,15,1,16,6,15,1,17,7,15,1,18,8,8,1,12,0,12,208,1,25,1,13,25,0,1,3,9,3,32,2,9,1,0,2,41,267,1,30,0,1,60,0,60,1,60,2,60,3,60,4,15,1,12,0,15,1,13,1,15,1,14,2,15,1,15,3,16,0,1,4,16,0,0,5,16,0,2,6,16,0,3,7,16,0,4,8,8,1,12,0,8,0,1,1,14,2,2,3,11,5,9,1,0,9,2,1,9,3,2,9,4,3,2,41,304,4,28,2147483944,1,60,0,33,0,15,1,5,0,8,1,5,0,10,933,1,14,2,2,3,11,1,32,0,2,4,61,30,0,15,1,1,0,4,62,28,2147484002,1,60,0,33,0,15,1,10,0,15,1,11,1,15,1,12,2,15,1,13,3,8,1,10,0,8,1,11,1,8,1,12,2,8,1,13,3,10,933,4,3,21,1,32,0,2,4,61,30,0,15,1,4,0,15,1,5,1,15,1,6,2,15,1,7,3,4,62], key:0}, 1:{code:[30,0,15,1,2,0,16,1,2,1,5], key:1}}, clause_keys:[0, 1], next_key:2, key:538}, 539: {is_public:false, clauses:{0:{code:[254,0,15,1,4,0,8,1,4,0,12,541,5,27,0,12,542,1,27,0,24,1,5,7,6,2,4,543], key:0}}, clause_keys:[0], next_key:1, key:539}, 540: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,33,0,16,0,1,0,10,367,0,3,64,2,12,212,0,26,928,25,0,1,3,539,2,32,0,2,4,535], key:0}}, clause_keys:[0], next_key:1, key:540}, 543: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,16,0,3,0,16,0,1,1,16,0,2,2,13,4,25,0,3,26,0,13,0,26,941,24,1,4,3,234,4,8,0,3,0,8,0,0,1,3,538,4,3,151,4,10,942,0,3,234,4,9,0,0,9,1,1,9,2,2,7,5,3,9,3,4,2,41,79,5,28,2147483774,1,60,0,33,0,15,1,9,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,8,1,9,0,8,1,10,1,8,1,11,2,3,544,1,32,0,2,4,62,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,11,0,41,155,1,254,0,1,60,0,60,1,33,0,16,0,1,0,10,943,0,3,287,2,9,1,0,3,64,2,32,0,2,4,61], key:0}}, clause_keys:[0], next_key:1, key:543}, 544: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,16,1,3,2,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,20,0,25,0,3,25,0,0,16,0,4,1,16,0,2,2,31,9,3,0,9,4,1,8,0,1,2,3,545,5,9,0,0,9,1,1,9,2,2,2,4,544], key:1}, 2:{code:[254,0,15,1,6,0,15,1,7,1,15,1,8,2,8,1,6,0,8,1,7,1,8,1,8,2,4,545], key:2}, 3:{code:[28,2147483662,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0,30,0,74,2], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:544, index: 3}, 545: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,19,546,0,25,0,0,16,0,1,1,16,0,2,2,31,9,0,0,9,1,1,9,2,2,2,4,547], key:0}, 1:{code:[30,0,15,1,5,0,15,1,6,1,16,1,6,2,8,1,6,0,8,1,5,1,41,25,2,28,2147483700,1,60,0,33,0,15,1,4,0,15,1,5,1,8,1,4,0,3,548,1,32,0,2,4,62,30,0,1,60,0,15,1,3,0,16,0,0,1,8,0,0,0,3,225,1,9,0,0,2,4,263], key:1}}, clause_keys:[0, 1], next_key:2, key:545}, 547: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,5,0,8,1,6,1,8,1,7,2,7,8,3,41,32,4,28,2147483723,1,60,0,33,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,8,1,8,0,8,1,9,1,8,1,10,2,3,550,1,32,0,2,4,62,30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,8,1,7,0,8,1,8,1,8,1,6,2,41,108,3,28,2147483797,1,60,0,60,1,60,2,33,2,16,0,0,0,16,0,1,1,15,1,5,2,8,0,0,0,3,548,3,32,2,9,0,0,9,1,1,2,4,9,30,0,15,1,6,0,15,1,7,1,15,1,8,2,8,1,8,0,8,1,6,1,8,1,7,2,4,551], key:0}}, clause_keys:[0], next_key:1, key:547}, 548: {is_public:false, clauses:{0:{code:[254,0,19,542,0,23,1,1,23,1,2,19,549,2,23,1,3,5], key:0}}, clause_keys:[0], next_key:1, key:548}, 550: {is_public:false, clauses:{0:{code:[28,1,19,564,0,23,1,6,15,1,7,1,15,1,8,2,12,564,0,24,1,6,8,1,7,1,8,1,8,2,4,555], key:0}, 1:{code:[29,2,17,965,0,15,1,6,1,15,1,7,2,10,965,0,8,1,6,1,8,1,7,2,4,555], key:1}, 2:{code:[29,3,19,565,0,23,1,6,15,1,7,1,15,1,8,2,12,565,0,24,1,6,8,1,7,1,8,1,8,2,4,555], key:2}, 3:{code:[30,0,17,967,0,15,1,6,1,15,1,7,2,10,967,0,8,1,6,1,8,1,7,2,4,555], key:3}, 4:{code:[254,0,44,0,2147483657,1000000,1000000,1000000,2147483664,45,0,2,965,1,967,3,46,0,2,564,0,565,2], key:4}}, clause_keys:[0, 1, 2, 3, 4], next_key:5, key:550, index: 4}, 551: {is_public:false, clauses:{0:{code:[28,1,19,552,0,23,1,4,23,1,5,23,1,6,15,1,7,1,16,1,7,2,12,73,0,24,1,4,24,1,5,24,1,6,4,553], key:0}, 1:{code:[29,2,19,554,0,23,1,6,15,1,7,1,16,1,7,2,12,554,0,24,1,6,8,1,7,1,8,1,7,2,4,555], key:1}, 2:{code:[29,3,19,556,0,23,1,4,15,1,5,1,16,1,5,2,8,1,4,0,4,557], key:2}, 3:{code:[29,4,19,558,0,23,1,6,15,1,7,1,16,1,7,2,12,558,0,24,1,6,8,1,7,1,8,1,7,2,4,555], key:3}, 4:{code:[29,5,19,559,0,23,1,3,15,1,4,1,16,1,4,2,5], key:4}, 5:{code:[29,6,19,560,0,23,1,6,23,1,7,15,1,8,1,16,1,8,2,12,560,0,24,1,6,24,1,7,8,1,8,1,8,1,8,2,4,555], key:5}, 6:{code:[29,7,19,561,0,23,1,6,15,1,7,1,16,1,7,2,12,561,0,24,1,6,8,1,7,1,8,1,7,2,4,555], key:6}, 7:{code:[29,8,19,562,0,23,1,6,15,1,7,1,16,1,7,2,12,562,0,24,1,6,8,1,7,1,8,1,7,2,4,555], key:7}, 8:{code:[30,0,19,563,0,23,1,6,15,1,7,1,16,1,7,2,12,563,0,24,1,6,8,1,7,1,8,1,7,2,4,555], key:8}, 9:{code:[254,0,44,0,1000000,1000000,1000000,1000000,2147483657,46,0,9,552,0,554,1,556,2,558,3,559,4,560,5,561,6,562,7,563,8], key:9}}, clause_keys:[0, 1, 2, 3, 4, 5, 6, 7, 8, 9], next_key:10, key:551, index: 9}, 553: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,16,0,2,0,8,0,2,0,3,222,4,8,0,3,0,3,218,4,9,3,0,8,0,1,1,3,99,4,8,0,0,0,12,212,1,25,0,1,25,0,2,3,9,3,8,0,0,0,3,225,1,9,0,0,2,4,263], key:0}}, clause_keys:[0], next_key:1, key:553}, 555: {is_public:false, clauses:{0:{code:[28,1,19,554,0,23,1,4,15,1,5,1,16,1,5,2,8,1,4,0,4,566], key:0}, 1:{code:[29,2,19,558,0,23,1,4,15,1,5,1,16,1,5,2,8,1,4,0,4,232], key:1}, 2:{code:[29,3,19,560,0,23,1,5,23,1,6,15,1,7,1,16,1,7,2,8,1,5,0,8,1,6,1,4,204], key:2}, 3:{code:[29,4,19,561,0,23,1,4,15,1,5,1,16,1,5,2,8,1,4,0,4,206], key:3}, 4:{code:[29,5,19,562,0,23,1,4,15,1,5,1,16,1,5,2,8,1,4,0,4,567], key:4}, 5:{code:[29,6,19,563,0,23,1,4,15,1,5,1,16,1,5,2,8,1,4,0,4,568], key:5}, 6:{code:[29,7,19,564,0,23,1,6,15,1,7,1,15,1,8,2,8,1,6,0,8,1,7,1,8,1,8,2,4,569], key:6}, 7:{code:[29,8,17,965,0,15,1,5,1,15,1,6,2,8,1,5,0,8,1,6,1,4,570], key:7}, 8:{code:[29,9,19,565,0,23,1,6,15,1,7,1,15,1,8,2,8,1,6,0,8,1,7,1,8,1,8,2,4,571], key:8}, 9:{code:[30,0,17,967,0,15,1,5,1,15,1,6,2,8,1,5,0,8,1,6,1,4,572], key:9}, 10:{code:[254,0,44,0,2147483657,1000000,1000000,1000000,2147483664,45,0,2,965,7,967,9,46,0,8,554,0,558,1,560,2,561,3,562,4,563,5,564,6,565,8], key:10}}, clause_keys:[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10], next_key:11, key:555, index: 10}, 557: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,16,0,4,0,8,0,3,0,3,218,5,9,4,0,8,0,3,1,8,0,2,2,3,213,5,12,592,1,25,0,2,12,208,0,26,445,24,1,1,3,209,4,9,3,0,8,0,1,1,3,98,4,8,0,0,0,12,212,1,25,0,1,25,0,2,3,9,3,8,0,0,0,3,225,1,9,0,0,2,4,263], key:0}}, clause_keys:[0], next_key:1, key:557}, 566: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,19,208,0,25,0,0,23,1,1,19,217,1,25,0,1,25,0,2,31,12,217,1,25,0,1,25,0,2,12,208,0,25,0,0,24,1,1,2,4,573], key:0}, 1:{code:[29,2,1,60,0,60,1,19,217,0,25,0,0,25,0,1,31,12,217,0,25,0,0,25,0,1,2,4,573], key:1}, 2:{code:[29,3,19,208,0,23,1,1,26,0,31,5], key:2}, 3:{code:[29,4,1,60,0,60,1,60,2,19,208,0,25,0,0,23,1,1,20,1,25,0,2,25,0,1,31,12,208,0,25,0,0,25,0,2,3,566,3,12,208,0,25,0,0,25,0,1,2,4,566], key:3}, 4:{code:[29,5,17,0,0,31,5], key:4}, 5:{code:[29,6,1,60,0,60,1,20,0,25,0,1,25,0,0,31,9,1,0,3,566,2,9,0,0,2,4,566], key:5}, 6:{code:[30,0,1,60,0,19,229,0,23,1,2,25,0,0,8,1,2,0,3,566,1,9,0,0,2,4,566], key:6}, 7:{code:[254,0,44,0,2147483657,1000000,1000000,5,2147483662,45,0,1,0,4,46,0,3,208,2147483671,217,1,229,6,71,0,72,2,73,3], key:7}}, clause_keys:[0, 1, 2, 3, 4, 5, 6, 7], next_key:8, key:566, index: 7}, 567: {is_public:false, clauses:{0:{code:[28,1,17,0,0,5], key:0}, 1:{code:[30,0,1,60,0,20,0,23,1,3,25,0,0,8,1,3,0,10,625,1,3,582,1,9,0,0,2,4,567], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:567, index: 2}, 568: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,19,229,0,25,0,1,25,0,0,31,9,1,0,3,568,2,9,0,0,2,4,568], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,15,1,3,0,8,1,3,0,13,1,25,0,3,25,0,2,3,13,5,8,0,2,0,8,0,1,1,3,494,5,8,0,4,0,3,218,5,9,3,0,8,0,1,1,9,4,2,8,0,0,3,3,216,5,9,0,0,9,1,1,9,2,2,2,4,108], key:1}}, clause_keys:[0, 1], next_key:2, key:568}, 569: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,16,0,4,0,15,1,5,1,16,0,2,2,8,1,5,0,12,542,1,25,0,3,25,0,0,3,9,5,8,0,1,0,12,198,1,25,0,3,27,1,3,34,5,9,0,0,7,8,1,9,1,2,9,2,3,9,3,4,7,9,5,7,10,6,9,4,7,2,41,84,8,28,2147483825,1,60,0,60,1,60,2,60,3,60,4,33,3,15,1,11,0,16,0,2,1,16,0,4,2,16,0,0,3,16,0,1,4,15,1,12,5,15,1,13,6,15,1,14,7,8,1,11,0,12,549,1,25,0,2,3,9,5,8,0,2,0,9,4,1,3,36,5,32,3,9,0,0,12,549,2,25,0,2,12,542,1,25,0,1,24,1,2,2,4,9,30,0,15,1,10,0,15,1,11,1,15,1,12,2,15,1,13,3,15,1,14,4,15,1,15,5,15,1,16,6,15,1,17,7,8,1,10,0,8,1,11,1,8,1,12,2,8,1,16,3,8,1,17,4,8,1,13,5,41,238,6,28,2147483951,1,60,0,60,1,60,2,33,0,15,1,9,0,16,0,1,1,16,0,2,2,15,1,10,3,15,1,11,4,15,1,12,5,8,1,9,0,12,549,1,25,0,1,3,9,3,9,1,0,9,2,1,3,38,3,32,0,10,984,0,2,4,143,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,12,0,8,1,13,1,8,1,10,2,41,344,3,28,2147484045,1,60,0,60,1,60,2,33,2,15,1,5,0,16,0,0,1,16,0,1,2,8,1,5,0,3,222,3,32,2,9,0,0,12,541,2,25,0,1,12,542,1,25,0,1,24,1,2,2,4,9,30,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,6,0,12,549,8,24,1,7,12,542,1,25,1,7,24,1,8,4,9], key:0}}, clause_keys:[0], next_key:1, key:569}, 570: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,16,0,3,0,16,0,2,1,8,0,3,0,12,542,1,25,0,1,25,0,0,3,9,4,9,0,0,7,11,1,9,1,2,9,2,3,9,3,4,7,12,5,7,13,6,7,14,7,7,15,8,7,16,9,7,17,10,2,41,72,11,28,2147483812,1,60,0,60,1,60,2,60,3,60,4,33,2,15,1,14,0,16,0,4,1,16,0,3,2,16,0,0,3,16,0,1,4,15,1,15,5,15,1,16,6,15,1,17,7,15,1,18,8,15,1,19,9,15,1,20,10,8,1,14,0,12,549,1,25,0,4,3,9,5,9,3,0,9,4,1,3,35,5,32,2,9,0,0,9,1,1,2,4,9,30,0,15,1,13,0,15,1,14,1,15,1,15,2,15,1,16,3,15,1,17,4,15,1,18,5,15,1,19,6,15,1,20,7,15,1,21,8,15,1,22,9,15,1,23,10,8,1,13,0,8,1,14,1,8,1,15,2,8,1,16,3,8,1,19,4,8,1,20,5,8,1,21,6,8,1,22,7,8,1,23,8,41,249,9,28,2147483992,1,60,0,60,1,60,2,60,3,33,2,15,1,12,0,16,0,3,1,16,0,1,2,16,0,0,3,15,1,13,4,15,1,14,5,15,1,15,6,15,1,16,7,15,1,17,8,8,1,12,0,12,549,1,25,0,3,3,9,4,8,0,1,0,9,3,1,3,9,4,32,2,9,0,0,12,541,2,25,0,1,12,542,1,25,0,1,24,1,2,2,4,9,30,0,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,15,1,15,4,15,1,16,5,15,1,17,6,15,1,18,7,15,1,19,8,8,1,11,0,8,1,12,1,8,1,13,2,8,1,16,3,8,1,17,4,8,1,18,5,8,1,14,6,8,1,19,7,41,417,8,28,2147484138,1,60,0,60,1,60,2,33,0,15,1,11,0,16,0,2,1,16,0,1,2,15,1,12,3,15,1,13,4,15,1,14,5,15,1,15,6,15,1,16,7,8,1,11,0,12,549,1,25,0,2,3,9,3,9,1,0,9,2,1,3,36,3,32,0,10,985,0,2,4,143,30,0,15,1,10,0,15,1,11,1,15,1,12,2,15,1,13,3,15,1,14,4,15,1,15,5,15,1,16,6,15,1,17,7,8,1,10,0,8,1,14,1,8,1,12,2,8,1,15,3,8,1,16,4,8,1,17,5,41,551,6,28,2147484264,1,60,0,60,1,60,2,33,0,15,1,9,0,16,0,2,1,16,0,1,2,15,1,10,3,15,1,11,4,15,1,12,5,8,1,9,0,12,541,1,25,0,2,3,9,3,9,1,0,9,2,1,3,35,3,32,0,10,986,0,2,4,143,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,8,0,8,1,9,1,8,1,10,2,8,1,12,3,8,1,13,4,41,665,5,28,2147484392,1,60,0,60,1,60,2,60,3,33,2,15,1,8,0,16,0,3,1,16,0,1,2,16,0,0,3,15,1,9,4,8,1,8,0,12,541,1,25,0,3,3,9,4,8,0,1,0,9,3,1,3,9,4,32,2,9,0,0,12,549,2,25,0,1,12,542,1,25,0,1,24,1,2,2,4,9,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,7,0,8,1,8,1,8,1,9,2,41,781,3,254,0,1,60,0,60,1,60,2,33,0,15,1,6,0,16,0,2,1,16,0,1,2,8,1,6,0,12,541,1,25,0,2,3,9,3,9,1,0,9,2,1,3,36,3,32,0,10,987,0,2,4,143], key:0}}, clause_keys:[0], next_key:1, key:570}, 571: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,16,0,3,0,15,1,5,1,16,0,2,2,8,1,5,0,12,542,1,25,0,1,25,0,0,3,9,4,9,0,0,7,11,1,9,1,2,7,12,3,9,2,4,7,13,5,7,14,6,7,15,7,7,16,8,7,17,9,9,3,10,2,41,76,11,28,2147483809,1,60,0,60,1,60,2,33,0,15,1,14,0,16,0,2,1,16,0,1,2,15,1,15,3,15,1,16,4,15,1,17,5,15,1,18,6,15,1,19,7,15,1,20,8,15,1,21,9,15,1,22,10,8,1,14,0,12,541,1,25,0,2,3,9,3,9,1,0,9,2,1,3,35,3,32,0,10,986,0,2,4,143,30,0,15,1,13,0,15,1,14,1,15,1,15,2,15,1,16,3,15,1,17,4,15,1,18,5,15,1,19,6,15,1,20,7,15,1,21,8,15,1,22,9,15,1,23,10,8,1,13,0,8,1,14,1,8,1,15,2,8,1,17,3,8,1,18,4,8,1,19,5,8,1,20,6,8,1,21,7,8,1,22,8,8,1,23,9,41,250,10,28,2147483997,1,60,0,60,1,60,2,60,3,33,2,15,1,13,0,16,0,3,1,16,0,1,2,16,0,0,3,15,1,14,4,15,1,15,5,15,1,16,6,15,1,17,7,15,1,18,8,15,1,19,9,8,1,13,0,12,541,1,25,0,3,3,9,4,8,0,1,0,9,3,1,3,9,4,32,2,9,0,0,12,549,2,25,0,1,12,542,1,25,0,1,24,1,2,2,4,9,30,0,15,1,12,0,15,1,13,1,15,1,14,2,15,1,15,3,15,1,16,4,15,1,17,5,15,1,18,6,15,1,19,7,15,1,20,8,15,1,21,9,8,1,12,0,8,1,13,1,8,1,14,2,8,1,17,3,8,1,18,4,8,1,15,5,8,1,19,6,8,1,20,7,8,1,21,8,41,430,9,28,2147484155,1,60,0,60,1,60,2,33,0,15,1,12,0,16,0,2,1,16,0,1,2,15,1,13,3,15,1,14,4,15,1,15,5,15,1,16,6,15,1,17,7,15,1,18,8,8,1,12,0,12,541,1,25,0,2,3,9,3,9,1,0,9,2,1,3,36,3,32,0,10,987,0,2,4,143,30,0,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,15,1,15,4,15,1,16,5,15,1,17,6,15,1,18,7,15,1,19,8,8,1,11,0,8,1,15,1,8,1,13,2,8,1,16,3,8,1,17,4,8,1,18,5,8,1,19,6,41,576,7,28,2147484312,1,60,0,60,1,60,2,60,3,33,3,15,1,10,0,16,0,2,1,16,0,1,2,16,0,0,3,15,1,11,4,15,1,12,5,15,1,13,6,8,1,10,0,12,549,1,25,0,2,3,9,4,8,0,2,0,8,0,1,1,3,36,4,32,3,9,0,0,12,549,2,25,0,2,12,542,1,25,0,1,24,1,2,2,4,9,30,0,15,1,9,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,15,1,14,5,15,1,15,6,8,1,9,0,8,1,10,1,8,1,11,2,8,1,14,3,8,1,15,4,8,1,12,5,41,721,6,28,2147484434,1,60,0,60,1,60,2,33,0,15,1,9,0,16,0,1,1,16,0,2,2,15,1,10,3,15,1,11,4,15,1,12,5,8,1,9,0,12,549,1,25,0,1,3,9,3,9,1,0,9,2,1,3,35,3,32,0,10,988,0,2,4,143,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,12,0,8,1,13,1,8,1,10,2,41,827,3,28,2147484528,1,60,0,60,1,60,2,33,2,15,1,5,0,16,0,0,1,16,0,1,2,8,1,5,0,3,222,3,32,2,9,0,0,12,541,2,25,0,1,12,542,1,25,0,1,24,1,2,2,4,9,30,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,6,0,12,549,8,24,1,7,12,542,1,25,1,7,24,1,8,4,9], key:0}}, clause_keys:[0], next_key:1, key:571}, 572: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,15,1,4,0,16,0,2,1,8,1,4,0,12,542,1,25,0,1,25,0,0,3,9,4,8,0,3,0,12,195,1,25,0,1,27,1,3,34,4,9,0,0,7,10,1,9,1,2,9,2,3,9,3,4,7,11,5,7,12,6,7,13,7,7,14,8,7,15,9,2,41,84,10,28,2147483832,1,60,0,60,1,60,2,60,3,60,4,33,2,15,1,13,0,16,0,3,1,16,0,4,2,16,0,0,3,16,0,1,4,15,1,14,5,15,1,15,6,15,1,16,7,15,1,17,8,15,1,18,9,8,1,13,0,12,549,1,25,0,3,3,9,5,9,3,0,9,4,1,3,9,5,32,2,9,0,0,12,541,2,25,0,1,12,542,1,25,0,1,24,1,2,2,4,9,30,0,15,1,12,0,15,1,13,1,15,1,14,2,15,1,15,3,15,1,16,4,15,1,17,5,15,1,18,6,15,1,19,7,15,1,20,8,15,1,21,9,8,1,12,0,8,1,13,1,8,1,14,2,8,1,18,3,8,1,19,4,8,1,15,5,8,1,16,6,8,1,20,7,8,1,21,8,41,265,9,28,2147483990,1,60,0,60,1,60,2,33,0,15,1,12,0,16,0,1,1,16,0,2,2,15,1,13,3,15,1,14,4,15,1,15,5,15,1,16,6,15,1,17,7,15,1,18,8,8,1,12,0,12,549,1,25,0,1,3,9,3,9,1,0,9,2,1,3,259,3,32,0,10,989,0,2,4,143,30,0,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,15,1,15,4,15,1,16,5,15,1,17,6,15,1,18,7,15,1,19,8,8,1,11,0,8,1,15,1,8,1,13,2,8,1,16,3,8,1,17,4,8,1,18,5,8,1,19,6,41,411,7,28,2147484147,1,60,0,60,1,60,2,60,3,60,4,33,2,15,1,10,0,16,0,3,1,16,0,4,2,16,0,0,3,16,0,1,4,15,1,11,5,15,1,12,6,8,1,10,0,12,541,1,25,0,3,3,9,5,9,3,0,9,4,1,3,9,5,32,2,9,0,0,12,541,2,25,0,1,12,542,1,25,0,1,24,1,2,2,4,9,30,0,15,1,9,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,15,1,14,5,15,1,15,6,8,1,9,0,8,1,10,1,8,1,11,2,8,1,15,3,41,548,4,28,2147484253,1,60,0,60,1,60,2,33,0,15,1,7,0,16,0,1,1,16,0,2,2,15,1,8,3,8,1,7,0,12,541,1,25,0,1,3,9,3,9,1,0,9,2,1,3,259,3,32,0,10,990,0,2,4,143,30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,41,626,0,254,0,1,60,0,33,0,3,62,1,32,0,10,991,0,2,4,143], key:0}}, clause_keys:[0], next_key:1, key:572}, 573: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,19,208,0,25,0,0,23,1,1,19,217,1,25,0,1,25,0,2,31,12,217,1,25,0,1,25,0,2,12,208,0,25,0,0,24,1,1,2,4,100], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,19,217,0,25,0,1,25,0,2,8,0,0,0,3,218,3,12,217,1,25,0,1,25,0,2,12,208,0,25,0,0,24,1,1,2,4,100], key:1}, 2:{code:[254,0,44,0,1000000,1000000,1000000,1000000,2147483657,46,0,2,208,0,217,1], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:573, index: 2}, 574: {is_public:false, clauses:{0:{code:[254,0,15,1,4,0,10,1178,0,13,1,25,1,4,23,1,5,7,6,2,4,148], key:0}}, clause_keys:[0], next_key:1, key:574}, 575: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,2,1,5], key:0}, 1:{code:[30,0,1,60,0,60,1,20,0,23,1,4,25,0,0,16,0,1,1,8,0,1,0,8,1,4,1,3,104,2,9,0,0,9,1,1,2,4,575], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:575, index: 2}, 577: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,5,0,15,1,6,1,10,1000,0,12,195,7,25,1,5,25,1,6,13,1,24,1,7,23,1,8,7,9,2,3,148,1,32,0,2,5], key:0}, 1:{code:[30,0,17,368,0,17,613,1,5], key:1}}, clause_keys:[0, 1], next_key:2, key:577}, 578: {is_public:false, clauses:{0:{code:[28,1,1,60,0,19,576,0,25,0,0,16,0,0,1,31,9,0,0,7,4,1,7,5,2,7,6,3,2,41,32,4,28,2147483719,1,60,0,33,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,8,1,7,0,8,1,8,1,3,105,1,32,0,2,4,62,30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,8,1,6,0,8,1,9,1,41,100,2,28,2147483778,1,60,0,33,0,15,1,5,0,15,1,6,1,8,1,5,0,10,2,1,3,9,1,32,0,2,4,62,30,0,1,60,0,15,1,4,0,16,0,0,1,8,1,4,0,8,0,0,1,3,489,1,9,0,0,2,4,580], key:0}, 1:{code:[30,0,1,60,0,60,1,16,0,1,0,15,1,4,1,8,0,1,0,8,1,4,1,3,492,2,9,1,0,8,0,0,1,3,230,2,9,0,0,2,41,43,1,28,2147483714,1,60,0,33,0,15,1,3,0,8,1,3,0,3,231,1,32,0,2,4,62,30,0,15,1,2,0,8,1,2,0,4,580], key:1}}, clause_keys:[0, 1], next_key:2, key:578}, 580: {is_public:false, clauses:{0:{code:[254,0,15,1,2,0,13,0,25,1,2,26,0,4,221], key:0}}, clause_keys:[0], next_key:1, key:580}, 581: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,19,576,0,25,0,0,16,0,1,1,31,7,3,0,12,105,1,25,0,0,24,1,3,9,1,2,2,4,199], key:0}, 1:{code:[30,0,1,60,0,60,1,15,1,4,0,16,0,1,1,8,1,4,0,8,0,0,1,3,492,2,7,3,0,12,105,1,25,0,0,24,1,3,9,1,2,2,4,199], key:1}}, clause_keys:[0, 1], next_key:2, key:581}, 582: {is_public:false, clauses:{0:{code:[254,0,1,60,0,16,0,0,0,17,625,1,8,0,0,0,3,206,1,9,0,0,7,3,1,7,4,2,2,41,32,3,28,2147483732,1,60,0,60,1,60,2,33,2,15,1,6,0,16,0,0,1,16,0,1,2,8,1,6,0,8,0,0,1,3,581,3,32,2,8,0,1,0,3,218,2,9,0,0,9,1,1,2,4,575,30,0,15,1,4,0,15,1,5,1,15,1,6,2,12,583,0,25,1,4,4,64], key:0}}, clause_keys:[0], next_key:1, key:582}, 584: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,2,1,5], key:0}, 1:{code:[30,0,20,0,23,1,4,23,1,5,16,1,4,1,8,1,5,0,8,1,4,1,4,584], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:584, index: 2}, 585: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,5,0,8,1,6,1,8,1,7,2,41,29,3,28,2147483705,15,1,6,0,15,1,7,1,15,1,8,2,8,1,6,0,8,1,7,1,8,1,8,2,4,109,30,0,1,60,0,60,1,60,2,16,0,2,0,16,0,1,1,16,0,0,2,8,0,2,0,40,120,1,3,9,2,0,8,0,1,1,8,0,0,2,40,151,3,3,8,0,0,0,9,1,1,3,494,2,9,0,0,10,140,1,2,4,584,28,2147483791,1,60,0,33,0,15,1,3,0,8,1,3,0,3,3,1,32,0,2,4,61,30,0,15,1,1,0,4,62,28,2147483838,1,60,0,33,0,15,1,7,0,15,1,8,1,15,1,9,2,8,1,7,0,8,1,8,1,8,1,9,2,3,109,1,32,0,2,4,61,30,0,15,1,3,0,15,1,4,1,15,1,5,2,4,62], key:0}}, clause_keys:[0], next_key:1, key:585}, 586: {is_public:false, clauses:{0:{code:[254,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,8,1,8,0,8,1,9,1,8,1,10,2,8,1,11,3,4,587], key:0}}, clause_keys:[0], next_key:1, key:586}, 587: {is_public:false, clauses:{0:{code:[254,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,7,10,0,8,1,8,1,7,11,2,8,1,6,3,8,1,7,4,7,12,5,8,1,9,6,41,46,7,28,2147483790,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,15,1,10,0,16,0,1,1,16,0,2,2,16,0,4,3,16,0,5,4,16,0,0,5,16,0,3,6,8,0,1,0,8,0,2,1,40,182,2,7,8,0,2,0,12,217,1,25,0,4,25,0,5,3,105,7,32,6,9,1,0,9,2,1,9,3,2,9,4,3,9,5,4,2,41,313,5,30,0,15,1,9,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,15,1,14,5,15,1,15,6,8,1,10,0,8,1,15,1,4,9,28,2147483850,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,4,107,30,0,15,1,4,0,15,1,5,1,8,1,4,0,7,6,1,8,1,5,2,41,226,3,254,0,1,60,0,60,1,60,2,60,3,33,1,16,0,3,0,16,0,2,1,16,0,0,2,8,0,3,0,10,368,1,3,10,4,9,3,0,40,279,1,4,32,1,9,0,0,10,2,1,2,4,9,28,2147483953,1,60,0,33,0,15,1,4,0,8,1,4,0,10,2,1,3,107,1,32,0,2,4,61,30,0,15,1,1,0,4,62,28,2147484013,1,60,0,60,1,60,2,33,2,15,1,8,0,16,0,0,1,16,0,1,2,15,1,9,3,15,1,10,4,8,1,8,0,10,2,1,3,9,3,32,2,9,0,0,9,1,1,2,4,9,30,0,15,1,9,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,8,1,12,0,8,1,13,1,8,1,10,2,8,1,11,3,4,586], key:0}}, clause_keys:[0], next_key:1, key:587}, 588: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,20,0,23,1,2,25,0,0,8,1,2,0,3,589,2,13,0,27,95,26,0,13,1,25,0,1,26,0,3,9,2,7,2,0,13,5,25,0,1,26,0,13,4,25,0,0,24,1,5,13,1,23,1,3,24,1,4,2,41,108,2,254,0,1,60,0,60,1,16,0,0,0,15,1,5,1,16,0,1,2,8,0,0,0,8,1,5,1,3,324,2,9,0,0,9,1,1,2,41,194,2,28,2147483811,1,60,0,33,0,15,1,5,0,20,1,23,1,6,23,1,7,20,7,23,1,8,23,1,9,20,9,23,1,10,26,0,8,1,6,0,8,1,8,1,8,1,10,2,40,68,3,1,32,0,2,4,61,30,0,15,1,2,0,20,1,23,1,3,23,1,4,20,4,23,1,5,23,1,6,20,6,23,1,7,26,0,4,62,28,2147483874,1,60,0,33,0,15,1,5,0,15,1,6,1,8,1,5,0,8,1,6,1,40,238,2,1,32,0,2,4,61,30,0,15,1,2,0,15,1,3,1,4,62,28,2147483902,15,1,3,0,15,1,4,1,8,1,3,0,4,589,30,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,41,275,2,28,2147483939,15,1,3,0,15,1,4,1,8,1,3,0,4,590,30,0,15,1,4,0,15,1,5,1,8,1,5,0,8,1,4,1,41,312,2,28,2147483980,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,4,9,30,0,15,1,3,0,15,1,4,1,8,1,4,0,4,591], key:0}}, clause_keys:[0], next_key:1, key:588}, 589: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,8,1,3,0,13,28,27,122,26,0,13,27,27,121,24,1,28,13,26,27,120,24,1,27,13,25,27,119,24,1,26,13,24,27,118,24,1,25,13,23,27,117,24,1,24,13,22,27,116,24,1,23,13,21,27,115,24,1,22,13,20,27,114,24,1,21,13,19,27,113,24,1,20,13,18,27,112,24,1,19,13,17,27,111,24,1,18,13,16,27,110,24,1,17,13,15,27,109,24,1,16,13,14,27,108,24,1,15,13,13,27,107,24,1,14,13,12,27,106,24,1,13,13,11,27,105,24,1,12,13,10,27,104,24,1,11,13,9,27,103,24,1,10,13,8,27,102,24,1,9,13,7,27,101,24,1,8,13,6,27,100,24,1,7,13,5,27,99,24,1,6,13,4,27,98,24,1,5,13,1,27,97,24,1,4,4,324], key:0}}, clause_keys:[0], next_key:1, key:589}, 590: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,8,1,3,0,13,28,27,90,26,0,13,27,27,89,24,1,28,13,26,27,88,24,1,27,13,25,27,87,24,1,26,13,24,27,86,24,1,25,13,23,27,85,24,1,24,13,22,27,84,24,1,23,13,21,27,83,24,1,22,13,20,27,82,24,1,21,13,19,27,81,24,1,20,13,18,27,80,24,1,19,13,17,27,79,24,1,18,13,16,27,78,24,1,17,13,15,27,77,24,1,16,13,14,27,76,24,1,15,13,13,27,75,24,1,14,13,12,27,74,24,1,13,13,11,27,73,24,1,12,13,10,27,72,24,1,11,13,9,27,71,24,1,10,13,8,27,70,24,1,9,13,7,27,69,24,1,8,13,6,27,68,24,1,7,13,5,27,67,24,1,6,13,4,27,66,24,1,5,13,1,27,65,24,1,4,4,324], key:0}}, clause_keys:[0], next_key:1, key:590}, 591: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,8,1,3,0,13,12,27,57,26,0,13,11,27,56,24,1,12,13,10,27,55,24,1,11,13,9,27,54,24,1,10,13,8,27,53,24,1,9,13,7,27,52,24,1,8,13,6,27,51,24,1,7,13,5,27,50,24,1,6,13,4,27,49,24,1,5,13,1,27,48,24,1,4,4,324], key:0}}, clause_keys:[0], next_key:1, key:591}, 594: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,19,229,0,25,0,5,25,0,0,16,0,6,1,19,229,2,25,0,7,25,0,1,16,0,8,3,16,0,3,4,16,0,4,5,31,9,5,0,9,6,1,9,7,2,9,8,3,8,0,2,4,8,0,4,5,3,594,9,9,0,0,10,1059,1,9,1,2,9,2,3,9,3,4,9,4,5,2,4,594], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,19,208,0,25,0,0,23,1,6,19,229,6,25,0,1,25,0,2,16,0,3,1,16,0,4,2,16,0,5,3,16,0,6,4,16,0,7,5,31,12,208,6,25,0,0,25,0,1,12,208,7,25,0,0,25,0,2,12,229,0,24,1,6,24,1,7,9,3,1,9,4,2,9,5,3,9,6,4,9,7,5,2,4,594], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,19,610,0,25,0,0,16,0,1,1,16,0,2,2,16,0,3,3,16,0,4,4,16,0,5,5,8,0,0,0,3,3,7,32,6,12,611,6,25,0,0,12,610,0,24,1,6,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,594], key:2}, 3:{code:[29,4,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,19,610,0,25,0,3,15,1,6,1,19,612,2,23,1,7,25,0,2,20,3,23,1,8,25,0,5,19,613,8,24,1,7,25,0,2,23,1,9,23,1,10,19,614,9,23,1,11,19,229,10,25,0,4,23,1,12,19,615,12,24,1,11,16,0,6,4,16,0,0,5,31,8,0,3,0,10,1059,1,9,4,2,9,5,3,9,6,4,8,0,0,5,3,594,7,9,3,0,8,0,1,1,3,63,4,9,0,0,9,1,1,9,2,2,2,4,616], key:3}, 4:{code:[29,5,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,19,208,0,25,0,0,23,1,6,19,610,6,25,0,1,16,0,2,1,16,0,3,2,16,0,4,3,16,0,5,4,16,0,6,5,31,12,208,6,25,0,0,25,0,1,12,610,0,24,1,6,9,2,1,9,3,2,9,4,3,9,5,4,9,6,5,2,4,594], key:4}, 5:{code:[29,6,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,60,15,60,16,60,17,60,18,60,19,60,20,60,21,19,617,0,25,0,16,25,0,7,25,0,3,25,0,4,15,1,6,1,19,229,2,25,0,17,23,1,7,19,229,7,23,1,8,23,1,9,19,609,8,23,1,10,19,618,10,23,1,11,19,612,9,25,0,20,23,1,12,20,12,24,1,11,25,0,6,16,0,21,3,16,0,10,4,16,0,0,5,31,9,21,0,13,3,23,1,4,25,0,6,12,619,7,25,0,19,23,1,8,12,609,6,24,1,7,12,612,10,23,1,11,26,0,12,620,14,23,1,15,12,609,13,24,1,14,12,612,17,23,1,18,25,0,11,12,621,20,27,0,23,1,21,12,622,24,24,1,4,24,1,15,12,609,23,24,1,24,13,29,24,1,8,25,0,2,13,28,24,1,21,24,1,29,13,27,24,1,15,24,1,28,12,612,25,23,1,26,24,1,27,12,229,22,24,1,23,24,1,25,12,229,19,24,1,20,24,1,22,12,229,16,24,1,17,24,1,19,12,229,12,24,1,13,24,1,16,12,229,9,24,1,10,24,1,12,12,229,5,24,1,6,24,1,9,12,613,2,25,0,20,24,1,3,26,1070,24,1,5,13,32,24,1,4,25,0,6,12,614,33,23,1,34,12,623,37,24,1,4,12,609,36,24,1,37,12,624,40,23,1,41,12,609,39,24,1,40,12,615,43,24,1,34,12,236,47,24,1,41,12,625,46,25,0,3,24,1,47,12,609,45,24,1,46,12,609,49,26,288,12,229,48,25,0,8,24,1,49,12,229,44,24,1,45,24,1,48,12,229,42,24,1,43,24,1,44,12,229,38,24,1,39,24,1,42,12,229,35,24,1,36,24,1,38,12,613,31,25,0,20,24,1,32,24,1,33,24,1,35,13,52,23,1,53,25,0,6,12,614,54,24,1,34,12,625,57,25,0,3,26,126,12,609,56,24,1,57,12,609,59,26,280,12,615,62,24,1,34,12,609,63,26,126,12,229,61,24,1,62,24,1,63,12,229,60,25,0,8,24,1,61,12,229,58,24,1,59,24,1,60,12,229,55,24,1,56,24,1,58,12,613,51,25,0,20,24,1,52,24,1,54,24,1,55,12,613,65,24,1,18,25,0,11,25,0,12,25,0,14,13,71,24,1,8,25,0,2,13,70,24,1,4,24,1,71,13,68,23,1,69,24,1,70,12,625,74,24,1,69,24,1,4,12,609,73,24,1,74,12,625,77,25,0,3,26,479,12,609,76,24,1,77,12,626,80,24,1,8,12,609,79,24,1,80,12,229,78,24,1,79,25,0,8,12,229,75,24,1,76,24,1,78,12,229,72,24,1,73,24,1,75,12,613,67,24,1,26,24,1,68,26,1070,24,1,72,12,609,84,26,128,12,613,82,24,1,26,23,1,83,26,1070,24,1,84,12,625,89,25,0,3,26,480,12,609,88,24,1,89,12,609,91,26,33,12,229,90,25,0,8,24,1,91,12,229,87,24,1,88,24,1,90,12,613,86,24,1,11,25,0,19,26,1070,24,1,87,13,85,24,1,86,25,0,18,13,81,24,1,82,24,1,85,13,66,24,1,67,24,1,81,13,64,24,1,65,24,1,66,13,50,24,1,51,24,1,64,13,30,24,1,31,24,1,50,13,1,24,1,2,24,1,30,3,9,22,12,627,0,25,0,20,3,234,21,12,195,0,25,0,3,25,0,4,9,19,1,3,63,20,9,16,0,10,1059,1,9,17,2,9,18,3,8,0,15,4,8,0,0,5,3,594,19,8,0,7,0,10,1059,1,9,14,2,9,15,3,8,0,9,4,8,0,13,5,3,594,16,9,12,0,9,13,1,3,628,14,8,0,7,0,9,11,1,3,63,12,8,0,4,0,10,1059,1,9,8,2,9,9,3,9,10,4,8,0,0,5,3,594,11,12,195,2,25,0,7,25,0,3,12,195,0,24,1,2,25,0,4,8,0,5,1,3,63,8,8,0,0,0,9,5,1,9,6,2,3,616,7,12,195,0,25,0,3,25,0,4,8,0,1,1,3,63,5,9,0,0,9,1,1,9,2,2,2,4,616], key:5}, 6:{code:[29,7,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,19,208,0,25,0,0,23,1,6,19,617,6,25,0,1,25,0,2,25,0,3,25,0,4,16,0,5,1,16,0,6,2,16,0,7,3,16,0,8,4,16,0,9,5,31,12,208,6,25,0,0,25,0,1,12,208,7,25,0,0,25,0,2,12,208,8,25,0,0,25,0,4,12,617,0,24,1,6,24,1,7,25,0,3,24,1,8,9,5,1,9,6,2,9,7,3,9,8,4,9,9,5,2,4,594], key:6}, 7:{code:[29,8,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,19,629,0,25,0,3,23,1,12,25,0,4,15,1,13,1,19,229,2,23,1,14,23,1,15,19,609,14,23,1,16,19,618,16,23,1,17,19,612,15,23,1,18,23,1,19,20,19,24,1,17,23,1,20,20,20,24,1,12,25,0,2,20,3,23,1,21,23,1,22,19,613,21,24,1,18,23,1,23,26,1070,23,1,24,20,23,23,1,25,23,1,26,20,26,23,1,27,25,0,2,19,229,24,23,1,28,23,1,29,19,609,28,23,1,30,19,620,30,23,1,31,19,229,29,23,1,32,23,1,33,19,609,33,23,1,34,19,622,34,24,1,25,24,1,31,20,22,23,1,35,23,1,36,19,613,35,24,1,18,23,1,37,26,1070,23,1,38,20,37,23,1,39,23,1,40,20,40,24,1,12,25,0,2,19,229,38,23,1,41,23,1,42,19,609,41,23,1,43,19,623,43,24,1,39,19,229,42,23,1,44,23,1,45,19,609,44,23,1,46,19,624,46,23,1,47,19,612,45,23,1,48,23,1,49,20,49,24,1,47,23,1,50,20,50,24,1,12,25,0,2,20,36,23,1,51,23,1,52,19,613,51,24,1,48,23,1,53,23,1,54,23,1,55,20,53,24,1,47,23,1,56,20,56,24,1,12,25,0,2,19,614,54,23,1,57,19,229,55,23,1,58,23,1,59,19,609,58,23,1,60,19,625,60,24,1,47,24,1,12,19,229,59,23,1,61,23,1,62,19,615,61,24,1,57,19,229,62,23,1,63,25,0,5,19,609,63,26,296,20,52,23,1,64,23,1,65,19,613,64,24,1,48,23,1,66,26,1070,23,1,67,20,66,23,1,68,23,1,69,20,69,23,1,70,25,0,2,19,609,67,26,288,16,0,7,4,16,0,0,5,8,0,3,0,10,1059,1,8,1,32,2,8,1,65,3,8,0,6,4,8,0,0,5,3,594,8,8,0,4,0,10,1059,1,9,5,2,9,6,3,9,7,4,8,0,0,5,3,594,8,12,195,0,25,0,3,25,0,4,8,0,1,1,3,63,5,9,0,0,9,1,1,9,2,2,2,4,616], key:7}, 8:{code:[29,9,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,19,208,0,25,0,0,23,1,6,19,629,6,25,0,1,25,0,2,25,0,3,16,0,4,1,16,0,5,2,16,0,6,3,16,0,7,4,16,0,8,5,31,12,208,6,25,0,0,25,0,1,12,208,7,25,0,0,25,0,3,12,629,0,24,1,6,25,0,2,24,1,7,9,4,1,9,5,2,9,6,3,9,7,4,9,8,5,2,4,594], key:8}, 9:{code:[29,10,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,19,630,0,25,0,3,25,0,4,16,0,9,1,16,0,10,2,20,3,23,1,6,25,0,11,19,613,6,25,0,8,25,0,2,26,1070,25,0,5,16,0,7,4,16,0,0,5,31,12,612,6,25,0,8,25,0,2,12,196,0,24,1,6,9,9,1,9,10,2,9,11,3,8,0,6,4,8,0,0,5,3,594,12,12,196,6,25,0,4,12,229,0,25,0,3,24,1,6,10,1059,1,9,5,2,9,6,3,9,7,4,8,0,0,5,3,594,8,12,195,0,25,0,3,25,0,4,8,0,1,1,3,63,5,9,0,0,9,1,1,9,2,2,2,4,616], key:9}, 10:{code:[29,11,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,19,208,0,25,0,0,23,1,6,19,630,6,25,0,1,25,0,2,16,0,3,1,16,0,4,2,16,0,5,3,16,0,6,4,16,0,7,5,31,12,208,6,25,0,0,25,0,1,12,208,7,25,0,0,25,0,2,12,630,0,24,1,6,24,1,7,9,3,1,9,4,2,9,5,3,9,6,4,9,7,5,2,4,594], key:10}, 11:{code:[29,12,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,19,631,0,23,1,6,25,0,6,19,632,6,25,0,11,25,0,9,15,1,7,1,19,612,2,23,1,8,25,0,2,20,3,23,1,9,23,1,10,19,613,9,24,1,8,25,0,2,23,1,11,23,1,12,19,614,11,23,1,13,19,229,12,25,0,3,23,1,14,19,229,14,23,1,15,25,0,4,19,615,15,24,1,13,20,10,23,1,16,25,0,12,19,613,16,24,1,8,25,0,2,26,1070,25,0,5,16,0,8,4,16,0,0,5,31,9,11,0,10,1059,1,8,0,3,2,9,12,3,8,0,10,4,8,0,0,5,3,594,13,9,9,0,10,1059,1,8,0,4,2,9,10,3,8,0,7,4,8,0,0,5,3,594,11,9,6,0,10,1059,1,8,0,5,2,9,7,3,9,8,4,8,0,0,5,3,594,9,12,195,2,25,0,3,25,0,4,12,195,0,24,1,2,25,0,5,8,0,1,1,3,63,6,9,0,0,9,1,1,9,2,2,2,4,616], key:11}, 12:{code:[29,13,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,19,208,0,25,0,0,23,1,6,19,631,6,23,1,7,25,0,3,19,632,7,25,0,1,25,0,2,16,0,4,1,16,0,5,2,16,0,6,3,16,0,7,4,16,0,8,5,31,12,208,7,25,0,0,25,0,1,12,208,8,25,0,0,25,0,2,12,632,6,24,1,7,24,1,8,12,208,9,25,0,0,25,0,3,12,631,0,24,1,6,24,1,9,9,4,1,9,5,2,9,6,3,9,7,4,9,8,5,2,4,594], key:12}, 13:{code:[29,14,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,19,632,0,25,0,8,25,0,5,15,1,6,1,19,612,2,23,1,7,25,0,2,20,3,23,1,8,25,0,9,19,613,8,24,1,7,25,0,2,23,1,9,23,1,10,19,614,9,23,1,11,19,229,10,25,0,3,23,1,12,19,229,12,23,1,13,25,0,4,19,615,13,24,1,11,16,0,7,4,16,0,0,5,31,9,8,0,10,1059,1,8,0,3,2,9,9,3,8,0,6,4,8,0,0,5,3,594,10,9,5,0,10,1059,1,8,0,4,2,9,6,3,9,7,4,8,0,0,5,3,594,8,12,195,0,25,0,3,25,0,4,8,0,1,1,3,63,5,9,0,0,9,1,1,9,2,2,2,4,616], key:13}, 14:{code:[29,15,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,19,208,0,25,0,0,23,1,6,19,632,6,25,0,1,25,0,2,16,0,3,1,16,0,4,2,16,0,5,3,16,0,6,4,16,0,7,5,31,12,208,6,25,0,0,25,0,1,12,208,7,25,0,0,25,0,2,12,632,0,24,1,6,24,1,7,9,3,1,9,4,2,9,5,3,9,6,4,9,7,5,2,4,594], key:14}, 15:{code:[29,16,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,19,631,0,25,0,3,25,0,4,15,1,6,1,19,612,2,23,1,7,25,0,2,20,3,23,1,8,23,1,9,19,613,8,24,1,7,25,0,2,26,1070,25,0,8,20,9,23,1,10,25,0,9,19,613,10,24,1,7,25,0,2,26,1070,25,0,5,16,0,7,4,16,0,0,5,31,8,0,3,0,10,1059,1,9,8,2,9,9,3,8,0,6,4,8,0,0,5,3,594,10,8,0,4,0,10,1059,1,9,5,2,9,6,3,9,7,4,8,0,0,5,3,594,8,12,195,0,25,0,3,25,0,4,8,0,1,1,3,63,5,9,0,0,9,1,1,9,2,2,2,4,616], key:15}, 16:{code:[29,17,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,19,208,0,25,0,0,23,1,6,19,631,6,25,0,1,25,0,2,16,0,3,1,16,0,4,2,16,0,5,3,16,0,6,4,16,0,7,5,31,12,208,6,25,0,0,25,0,1,12,208,7,25,0,0,25,0,2,12,631,0,24,1,6,24,1,7,9,3,1,9,4,2,9,5,3,9,6,4,9,7,5,2,4,594], key:16}, 17:{code:[29,18,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,19,196,0,25,0,3,15,1,6,1,19,612,2,23,1,7,25,0,2,20,3,23,1,8,23,1,9,19,613,8,24,1,7,25,0,2,23,1,10,23,1,11,19,614,10,23,1,12,19,229,11,25,0,4,23,1,13,19,229,13,23,1,14,23,1,15,19,615,14,24,1,12,19,609,15,26,126,20,9,23,1,16,25,0,5,19,613,16,24,1,7,25,0,2,26,1070,23,1,17,19,609,17,26,128,16,0,6,4,15,1,18,5,31,8,0,3,0,10,1059,1,9,4,2,9,5,3,9,6,4,8,0,0,5,3,594,7,9,3,0,8,0,1,1,3,63,4,9,0,0,9,1,1,9,2,2,2,4,616], key:17}, 18:{code:[29,19,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,19,208,0,25,0,0,23,1,6,19,196,6,25,0,1,16,0,2,1,16,0,3,2,16,0,4,3,16,0,5,4,16,0,6,5,31,12,208,6,25,0,0,25,0,1,12,196,0,24,1,6,9,2,1,9,3,2,9,4,3,9,5,4,9,6,5,2,4,594], key:18}, 19:{code:[29,20,17,480,0,17,1059,1,19,615,2,23,1,6,15,1,7,3,16,1,7,4,19,608,5,24,1,6,31,5], key:19}, 20:{code:[29,21,1,60,0,60,1,60,2,60,3,60,4,19,208,0,23,1,6,26,480,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,31,10,480,0,9,0,1,9,1,2,9,2,3,9,3,4,9,4,5,2,4,594], key:20}, 21:{code:[29,22,19,612,0,23,1,6,23,1,7,15,1,8,1,19,612,2,24,1,6,24,1,7,15,1,9,3,16,1,9,4,15,1,10,5,31,5], key:21}, 22:{code:[29,23,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,19,208,0,23,1,6,23,1,7,19,612,7,25,0,0,25,0,1,16,0,2,1,16,0,3,2,16,0,4,3,16,0,5,4,16,0,6,5,31,12,612,0,25,0,0,25,0,1,9,2,1,9,3,2,9,4,3,9,5,4,9,6,5,2,4,594], key:22}, 23:{code:[29,24,19,609,0,23,1,6,15,1,7,1,19,609,2,24,1,6,15,1,8,3,16,1,8,4,15,1,9,5,31,5], key:23}, 24:{code:[29,25,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,19,208,0,25,0,0,23,1,6,19,609,6,25,0,1,16,0,2,1,16,0,3,2,16,0,4,3,16,0,5,4,16,0,6,5,31,12,208,6,25,0,0,25,0,1,12,609,0,24,1,6,9,2,1,9,3,2,9,4,3,9,5,4,9,6,5,2,4,594], key:24}, 25:{code:[30,0,1,60,0,33,0,19,208,0,23,1,8,23,1,9,15,1,10,1,19,609,2,23,1,11,19,633,11,24,1,8,24,1,9,15,1,12,3,16,1,12,4,15,1,13,5,8,1,8,0,8,1,9,1,40,63,2,1,32,0,2,5,28,2147483728,15,1,4,0,15,1,5,1,8,1,4,0,41,96,1,30,0,15,1,3,0,15,1,4,1,8,1,4,0,4,3,28,2147483767,1,60,0,33,0,15,1,3,0,8,1,3,0,3,4,1,32,0,2,4,61,30,0,15,1,1,0,4,62], key:25}, 26:{code:[254,0,15,1,6,0,15,1,7,1,19,609,2,25,1,6,15,1,8,3,16,1,8,4,15,1,9,5,5], key:26}, 27:{code:[28,2147483721,44,0,2147483657,1000000,1000000,1000000,2147483662,45,0,1,480,19,46,0,11,196,17,208,2147483687,229,0,609,23,610,2147483713,612,21,617,5,629,7,630,9,631,2147483717,632,13,71,1,72,4,72,6,72,8,72,10,72,12,72,14,72,16,72,18,72,20,72,22,72,24,73,25,71,2,73,3,71,11,73,15,30,0,74,26], key:27}}, clause_keys:[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27], next_key:28, key:594, index: 27}, 595: {is_public:false, clauses:{0:{code:[28,1,17,1053,0,31,5], key:0}, 1:{code:[30,0,19,608,0,23,1,1,5], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1000000,2147483662,45,0,1,1053,0,46,0,1,608,1], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:595, index: 2}, 596: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,33,3,15,1,5,0,16,0,2,1,8,1,5,0,8,0,0,1,8,0,1,2,3,637,4,32,3,9,0,0,9,1,1,9,2,2,2,4,638], key:0}, 1:{code:[30,0,15,1,2,0,16,1,2,1,5], key:1}}, clause_keys:[0, 1], next_key:2, key:596}, 598: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,16,0,4,0,19,229,1,25,0,5,25,0,6,16,0,3,2,31,12,195,3,25,0,4,25,0,5,12,229,0,24,1,3,25,0,6,10,0,1,8,0,1,2,3,639,7,8,0,1,0,8,0,2,1,3,494,4,8,0,1,0,8,0,0,1,3,63,4,9,0,0,14,0,1,7,6,2,9,1,3,9,2,4,9,3,5,2,4,640], key:0}, 1:{code:[30,0,15,1,3,0,15,1,4,1,17,0,2,31,5], key:1}}, clause_keys:[0, 1], next_key:2, key:598}, 599: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,16,0,0,0,19,229,1,23,1,3,23,1,4,16,0,1,2,31,9,0,0,9,1,1,2,4,494], key:0}, 1:{code:[29,2,1,60,0,60,1,33,0,15,1,6,0,19,631,1,23,1,7,23,1,8,16,0,1,2,8,1,6,0,8,1,7,1,8,0,1,2,3,599,2,9,1,0,10,613,1,3,188,2,32,0,2,5], key:1}, 2:{code:[29,3,1,60,0,60,1,33,0,15,1,6,0,19,631,1,23,1,7,23,1,8,16,0,1,2,8,1,6,0,8,1,8,1,8,0,1,2,3,599,2,9,1,0,10,613,1,3,188,2,32,0,2,5], key:2}, 3:{code:[29,4,1,60,0,60,1,33,0,15,1,5,0,15,1,6,1,16,0,1,2,8,1,5,0,8,0,1,1,3,494,2,9,1,0,14,0,1,3,35,2,32,0,2,5], key:3}, 4:{code:[30,0,15,1,3,0,15,1,4,1,17,613,2,5], key:4}}, clause_keys:[0, 1, 2, 3, 4], next_key:5, key:599}, 601: {is_public:false, clauses:{0:{code:[28,1,17,613,0,15,1,7,1,15,1,8,2,15,1,9,3,16,1,9,4,15,1,10,5,16,1,10,6,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,15,1,7,0,17,1053,1,16,0,0,2,20,3,26,790,25,0,1,16,0,2,4,16,0,3,5,16,0,4,6,31,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,644], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,60,3,33,0,15,1,12,0,19,608,1,25,0,1,16,0,2,2,20,3,26,790,23,1,13,15,1,14,4,15,1,15,5,20,6,23,1,16,23,1,17,19,645,16,25,0,1,26,1096,25,0,3,8,0,2,0,8,1,13,1,12,485,18,25,0,3,13,2,24,1,18,25,1,14,8,1,15,3,8,1,17,4,3,644,4,9,1,0,9,2,1,9,3,2,3,646,4,32,0,2,5], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:601}, 602: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,19,229,0,25,0,0,23,1,2,16,0,1,1,31,9,0,0,9,1,1,2,4,602], key:0}, 1:{code:[30,0,1,60,0,60,1,19,609,0,25,0,0,16,0,1,1,31,9,0,0,7,3,1,9,1,2,2,4,11], key:1}, 2:{code:[254,0,15,1,5,0,15,1,6,1,8,1,5,0,7,7,1,8,1,6,2,4,11], key:2}, 3:{code:[28,2147483664,44,0,1000000,1000000,1000000,1000000,2147483657,46,0,2,229,0,609,1,30,0,74,2], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:602, index: 3}, 603: {is_public:false, clauses:{0:{code:[254,0,15,1,18,0,15,1,19,1,15,1,20,2,15,1,21,3,15,1,22,4,15,1,23,5,15,1,24,6,15,1,25,7,8,1,18,0,8,1,19,1,14,0,2,10,1131,3,8,1,20,4,8,1,21,5,8,1,22,6,8,1,23,7,8,1,24,8,8,1,25,9,4,664], key:0}}, clause_keys:[0], next_key:1, key:603}, 604: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,4,1,15,1,5,2,16,1,5,3,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,60,15,60,16,60,17,60,18,20,0,23,1,6,25,0,0,19,613,6,23,1,7,25,0,9,25,0,18,23,1,8,16,0,1,1,20,2,23,1,9,25,0,13,19,403,9,24,1,7,16,0,3,3,8,1,8,0,8,0,4,1,3,596,19,9,18,0,8,0,12,1,3,628,19,12,195,0,25,0,9,25,0,12,8,0,4,1,8,0,5,2,3,598,18,8,0,5,0,8,0,4,1,8,0,6,2,3,599,18,8,0,9,0,8,0,17,1,3,494,18,8,0,4,0,8,0,16,1,3,602,18,9,17,0,8,0,12,1,8,0,15,2,3,635,18,8,0,14,0,12,198,1,25,0,15,25,0,16,3,34,17,8,0,6,0,9,12,1,8,0,5,2,9,13,3,8,0,11,4,12,600,7,25,0,14,13,5,24,1,7,26,0,8,0,10,6,3,601,15,9,9,0,9,10,1,8,0,7,2,14,0,3,8,0,5,4,9,11,5,8,0,8,6,3,636,12,9,4,0,8,0,1,1,9,5,2,9,6,3,9,7,4,7,8,5,9,8,6,8,0,2,7,3,603,9,9,0,0,9,1,1,9,2,2,9,3,3,2,4,604], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:604, index: 2}, 605: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,33,8,16,0,9,0,16,0,1,1,16,0,2,2,16,0,3,3,16,0,4,4,16,0,5,5,16,0,6,6,16,0,7,7,8,0,9,0,40,109,1,10,9,9,0,12,208,1,23,1,2,25,0,0,3,9,10,32,8,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,2,4,605,28,2147483780,1,60,0,33,0,15,1,3,0,8,1,3,0,3,3,1,32,0,2,4,61,30,0,15,1,1,0,4,62], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,16,0,9,0,16,0,8,1,15,1,15,2,16,0,3,3,15,1,16,4,16,0,2,5,15,1,17,6,16,0,5,7,8,1,16,0,8,1,15,1,8,0,3,2,8,1,17,3,8,0,4,4,12,600,18,25,0,6,13,5,24,1,18,26,0,8,0,1,6,3,601,10,9,9,0,7,5,1,7,6,2,7,7,3,8,0,0,4,40,165,5,10,8,0,0,0,8,0,7,1,3,494,9,9,6,0,12,198,1,25,0,7,25,0,8,3,34,9,9,0,0,9,1,1,9,2,2,14,0,3,9,3,4,9,4,5,9,5,6,2,4,636,28,2147483878,1,60,0,60,1,60,2,60,3,33,3,15,1,8,0,15,1,9,1,16,0,0,2,16,0,1,3,16,0,2,4,8,1,8,0,12,208,1,25,1,9,25,0,0,3,9,4,32,3,9,0,0,13,1,25,0,1,25,0,2,2,4,13,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,7,0,13,1,25,1,10,25,1,11,4,13], key:1}}, clause_keys:[0, 1], next_key:2, key:605}, 607: {is_public:false, clauses:{0:{code:[28,1,20,0,23,1,2,23,1,3,19,600,2,23,1,4,16,1,4,1,31,5], key:0}, 1:{code:[30,0,20,0,23,1,4,23,1,5,15,1,6,1,8,1,5,0,8,1,6,1,4,607], key:1}, 2:{code:[254,0,44,0,1000000,1000000,1000000,2147483657,1000000,71,0,73,1], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:607, index: 2}, 616: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,33,2,15,1,4,0,16,0,1,1,16,0,0,2,8,1,4,0,3,3,3,32,2,9,0,0,9,1,1,2,4,9], key:0}, 1:{code:[28,2,1,60,0,33,0,19,608,0,23,1,5,15,1,6,1,16,1,6,2,8,1,5,0,8,1,6,1,3,634,1,32,0,2,5], key:1}, 2:{code:[30,0,19,608,0,23,1,3,15,1,4,1,20,2,24,1,3,25,1,4,5], key:2}, 3:{code:[28,2147483652,74,0,30,0,44,1,1000000,1000000,1000000,1000000,2147483661,46,0,1,608,2147483666,71,1,73,2], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:616, index: 3}, 628: {is_public:false, clauses:{0:{code:[28,1,17,1070,0,17,1053,1,31,5], key:0}, 1:{code:[30,0,19,614,0,23,1,2,19,608,1,24,1,2,5], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1000000,2147483662,45,0,1,1070,0,46,0,1,614,1], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:628, index: 2}, 634: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,4,0,20,1,23,1,5,23,1,6,8,1,5,0,8,1,4,1,3,10,1,32,0,2,5], key:0}, 1:{code:[30,0,15,1,4,0,20,1,23,1,5,23,1,6,8,1,4,0,8,1,6,1,4,634], key:1}}, clause_keys:[0, 1], next_key:2, key:634}, 635: {is_public:false, clauses:{0:{code:[28,1,15,1,3,0,17,1053,1,16,1,3,2,31,5], key:0}, 1:{code:[30,0,15,1,5,0,19,608,1,23,1,6,15,1,7,2,8,1,7,0,12,198,1,25,1,5,27,1,4,34], key:1}}, clause_keys:[0, 1], next_key:2, key:635}, 636: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,7,1,16,1,7,2,15,1,8,3,15,1,9,4,15,1,10,5,16,1,10,6,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,20,0,23,1,14,25,0,0,15,1,15,1,16,0,2,2,16,0,7,3,16,0,4,4,15,1,16,5,16,0,6,6,8,1,14,0,8,1,15,1,8,0,1,2,12,401,3,25,0,7,8,0,4,4,8,1,16,5,8,0,5,6,3,649,8,8,0,3,0,12,198,1,25,0,7,27,1,3,34,8,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,2,4,636], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:636, index: 2}, 637: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,19,229,0,23,1,6,23,1,7,19,229,1,24,1,6,23,1,8,15,1,9,2,8,1,7,0,8,1,8,1,8,1,9,2,3,637,1,32,0,2,5], key:0}, 1:{code:[30,0,19,229,0,23,1,3,23,1,4,16,1,3,1,16,1,4,2,31,5], key:1}, 2:{code:[254,0,44,0,1000000,1000000,1000000,1000000,2147483657,46,0,1,229,2147483662,71,0,73,1], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:637, index: 2}, 638: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,19,229,0,25,0,3,25,0,0,16,0,1,1,16,0,4,2,31,9,3,0,8,0,2,1,9,4,2,3,638,5,9,0,0,9,1,1,9,2,2,2,4,638], key:0}, 1:{code:[30,0,15,1,3,0,15,1,4,1,19,229,2,25,1,3,25,1,4,5], key:1}}, clause_keys:[0, 1], next_key:2, key:638}, 639: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,33,4,19,229,0,23,1,5,25,0,0,16,0,2,1,16,0,3,2,8,1,5,0,8,0,1,1,3,63,5,32,4,9,0,0,13,1,25,0,1,25,0,2,9,3,2,2,4,639], key:0}, 1:{code:[30,0,15,1,5,0,15,1,6,1,20,2,23,1,7,25,1,6,8,1,5,0,8,1,7,1,4,63], key:1}}, clause_keys:[0, 1], next_key:2, key:639}, 640: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,6,1,16,1,6,2,15,1,7,3,15,1,8,4,17,0,5,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,33,7,20,0,23,1,12,25,0,0,16,0,6,1,16,0,2,2,16,0,3,3,16,0,4,4,20,5,23,1,13,25,0,5,19,641,13,24,1,12,23,1,14,23,1,15,19,399,14,25,0,6,19,642,15,25,0,8,8,1,12,0,14,0,1,8,0,3,2,14,0,3,10,0,4,8,0,9,5,3,643,10,9,8,0,12,195,2,25,0,4,25,0,9,12,195,1,24,1,2,27,1,3,34,10,32,7,8,0,1,0,12,198,1,25,0,6,27,1,3,34,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,640], key:1}, 2:{code:[30,0,20,0,23,1,12,23,1,13,15,1,14,1,15,1,15,2,15,1,16,3,15,1,17,4,15,1,18,5,8,1,13,0,8,1,14,1,8,1,15,2,8,1,16,3,8,1,17,4,8,1,18,5,4,640], key:2}, 3:{code:[254,0,44,0,2147483657,1000000,1000000,2147483662,1000000,45,0,1,0,0,71,1,73,2], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:640, index: 3}, 643: {is_public:false, clauses:{0:{code:[28,1,15,1,6,0,21,2,1,15,1,7,2,15,1,8,3,20,4,23,1,9,23,1,10,20,10,23,1,11,26,0,16,1,11,5,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,33,8,16,0,0,0,16,0,7,1,20,2,23,1,8,25,0,2,16,0,4,3,16,0,5,4,16,0,6,5,8,0,0,0,8,1,8,1,3,634,9,32,8,8,0,1,0,12,198,1,25,0,7,27,1,3,34,8,8,0,3,0,12,198,1,25,0,4,27,1,3,34,7,9,0,0,9,1,1,9,2,2,9,3,3,13,4,25,0,4,25,0,5,9,6,5,2,4,643], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,16,0,0,0,16,0,1,1,20,2,23,1,8,25,0,2,15,1,9,3,16,0,4,4,16,0,5,5,8,0,3,0,12,198,1,25,1,9,27,1,3,34,6,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,643], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:643}, 644: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,5,1,16,1,5,2,15,1,6,3,16,1,6,4,31,5], key:0}, 1:{code:[30,0,20,0,23,1,10,23,1,11,19,641,10,23,1,12,23,1,13,23,1,14,19,399,13,23,1,15,20,1,23,1,16,23,1,17,19,453,16,23,1,18,19,399,18,24,1,15,15,1,19,2,15,1,20,3,15,1,21,4,8,1,11,0,8,1,17,1,8,1,19,2,12,399,23,24,1,15,12,645,22,24,1,12,26,1097,24,1,23,13,3,24,1,22,25,1,20,8,1,21,4,4,644], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:644, index: 2}, 646: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,5,0,20,1,23,1,6,23,1,7,19,641,6,23,1,8,23,1,9,23,1,10,16,1,9,2,8,1,5,0,8,1,8,1,3,10,1,32,0,2,5], key:0}, 1:{code:[30,0,15,1,6,0,20,1,23,1,7,23,1,8,15,1,9,2,8,1,6,0,8,1,8,1,8,1,9,2,4,646], key:1}}, clause_keys:[0, 1], next_key:2, key:646}, 647: {is_public:false, clauses:{0:{code:[28,1,17,613,0,15,1,3,1,16,1,3,2,31,5], key:0}, 1:{code:[30,0,15,1,3,0,20,1,26,791,23,1,4,16,1,4,2,5], key:1}}, clause_keys:[0, 1], next_key:2, key:647}, 648: {is_public:false, clauses:{0:{code:[28,1,15,1,3,0,8,1,3,0,10,0,1,4,10], key:0}, 1:{code:[30,0,15,1,2,0,8,1,2,0,4,4], key:1}}, clause_keys:[0, 1], next_key:2, key:648}, 649: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,8,0,15,1,9,1,16,1,9,2,15,1,10,3,15,1,11,4,20,5,23,1,12,23,1,13,19,465,12,25,1,8,25,1,10,16,1,13,6,8,1,8,0,3,648,1,32,0,2,5], key:0}, 1:{code:[29,2,1,60,0,33,0,15,1,8,0,15,1,9,1,16,1,9,2,15,1,10,3,15,1,11,4,20,5,23,1,12,23,1,13,19,469,12,25,1,8,25,1,10,16,1,13,6,8,1,8,0,3,5,1,32,0,2,5], key:1}, 2:{code:[29,3,1,60,0,33,0,15,1,8,0,15,1,9,1,16,1,9,2,15,1,10,3,15,1,11,4,20,5,23,1,12,23,1,13,19,470,12,25,1,8,25,1,10,16,1,13,6,8,1,8,0,3,6,1,32,0,2,5], key:2}, 3:{code:[29,4,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,33,7,16,0,0,0,16,0,3,1,16,0,4,2,16,0,1,3,16,0,2,4,16,0,5,5,16,0,6,6,8,0,0,0,3,3,8,32,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,2,4,650], key:3}, 4:{code:[29,5,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,20,0,25,0,6,25,0,7,16,0,8,1,16,0,2,2,15,1,7,3,16,0,3,4,20,5,23,1,8,25,0,9,19,468,8,25,1,7,16,0,5,6,31,13,7,25,0,7,26,0,13,0,25,0,6,24,1,7,9,8,1,8,0,1,2,8,0,3,3,9,9,4,8,0,4,5,8,0,0,6,3,651,10,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,652], key:4}, 5:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,15,1,9,0,16,0,7,1,16,0,2,2,15,1,10,3,16,0,3,4,20,5,23,1,11,25,0,8,19,467,11,23,1,12,25,1,10,19,217,12,23,1,13,25,0,9,16,0,5,6,8,1,9,0,13,1,24,1,13,25,0,6,3,13,10,8,0,6,0,9,9,1,3,494,10,9,6,0,9,7,1,8,0,1,2,8,0,3,3,9,8,4,8,0,4,5,8,0,0,6,3,651,9,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,652], key:5}}, clause_keys:[0, 1, 2, 3, 4, 5], next_key:6, key:649}, 650: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,16,1,14,4,20,5,23,1,15,23,1,16,19,464,15,23,1,17,25,1,12,16,1,16,6,8,1,11,0,8,1,14,1,8,1,17,2,7,18,3,3,656,1,32,0,2,5], key:0}, 1:{code:[29,2,1,60,0,33,0,15,1,10,0,15,1,11,1,15,1,12,2,15,1,13,3,20,4,23,1,14,25,1,13,19,645,14,25,1,10,26,1096,23,1,15,19,399,15,23,1,16,20,5,23,1,17,23,1,18,19,463,17,23,1,19,25,1,11,19,399,19,24,1,16,16,1,18,6,8,1,10,0,8,1,12,1,12,399,2,24,1,16,3,646,1,32,0,2,5], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,15,1,7,0,15,1,8,1,15,1,9,2,16,0,0,3,20,4,23,1,10,25,0,1,19,645,10,25,1,7,26,1096,25,0,2,20,5,23,1,11,23,1,12,19,463,11,25,0,2,25,1,8,16,1,12,6,31,9,0,0,9,1,1,9,2,2,2,4,655], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:650}, 651: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,7,1,16,1,7,2,15,1,8,3,15,1,9,4,16,1,9,5,17,0,6,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,33,7,20,0,23,1,8,25,0,0,16,0,1,1,16,0,2,2,16,0,3,3,20,4,23,1,9,25,0,4,19,475,9,24,1,8,16,0,5,5,16,0,6,6,8,1,8,0,3,648,8,32,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,2,4,651], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,33,7,20,0,23,1,8,25,0,0,16,0,1,1,16,0,2,2,16,0,3,3,20,4,23,1,9,25,0,4,19,476,9,24,1,8,16,0,5,5,16,0,6,6,8,1,8,0,3,5,8,32,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,2,4,651], key:2}, 3:{code:[29,4,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,33,7,20,0,23,1,8,25,0,0,16,0,1,1,16,0,2,2,16,0,3,3,20,4,23,1,9,25,0,4,19,477,9,24,1,8,16,0,5,5,16,0,6,6,8,1,8,0,3,6,8,32,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,2,4,651], key:3}, 4:{code:[29,5,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,33,10,20,0,25,0,7,25,0,0,16,0,8,1,16,0,2,2,16,0,3,3,16,0,9,4,16,0,5,5,16,0,6,6,8,0,7,0,3,3,11,32,10,9,7,0,8,0,3,1,9,8,2,8,0,1,3,9,9,4,8,0,4,5,3,653,10,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,2,4,651], key:4}, 5:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,20,0,23,1,7,25,0,0,16,0,7,1,16,0,2,2,16,0,3,3,20,4,23,1,8,25,0,4,19,472,8,25,0,8,16,0,5,5,20,6,23,1,9,25,0,6,19,654,9,25,0,8,24,1,7,31,9,7,0,8,0,1,1,9,8,2,3,655,9,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,2,4,651], key:5}, 6:{code:[254,0,44,0,2147483657,1000000,1000000,2147483662,1000000,45,0,1,0,0,71,1,72,2,72,3,72,4,73,5], key:6}}, clause_keys:[0, 1, 2, 3, 4, 5, 6], next_key:7, key:651, index: 6}, 652: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,6,1,16,1,6,2,15,1,7,3,15,1,8,4,16,1,8,5,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,20,0,23,1,13,25,0,0,19,654,13,23,1,14,23,1,15,15,1,16,1,16,0,2,2,16,0,3,3,15,1,17,4,16,0,5,5,8,1,15,0,8,1,16,1,8,0,1,2,8,1,14,3,8,0,3,4,8,1,17,5,8,0,4,6,3,649,6,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,652], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:652, index: 2}, 653: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,33,7,16,0,3,0,15,1,10,1,16,0,4,2,16,0,5,3,16,0,2,4,16,0,6,5,8,0,3,0,8,0,4,1,8,0,1,2,8,0,0,3,3,656,8,32,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,2,4,657], key:0}, 1:{code:[29,2,1,60,0,33,0,15,1,9,0,15,1,10,1,15,1,11,2,20,3,23,1,12,25,1,11,19,645,12,25,1,9,26,1112,23,1,13,19,399,13,23,1,14,20,4,23,1,15,23,1,16,19,472,15,23,1,17,19,399,17,24,1,14,16,1,16,5,8,1,9,0,8,1,10,1,12,399,2,24,1,14,3,646,1,32,0,2,5], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,15,1,6,0,15,1,7,1,16,0,0,2,20,3,23,1,8,25,0,1,19,645,8,25,1,6,26,1112,25,0,2,20,4,23,1,9,23,1,10,19,472,9,25,0,2,16,1,10,5,31,9,0,0,9,1,1,9,2,2,2,4,655], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:653}, 655: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,20,0,23,1,5,23,1,6,19,600,5,23,1,7,20,1,23,1,8,24,1,6,19,600,8,23,1,9,19,401,2,24,1,7,8,1,9,0,12,198,1,24,1,7,27,1,3,34,1,32,0,2,5], key:0}, 1:{code:[30,0,20,0,23,1,6,23,1,7,20,1,24,1,6,23,1,8,15,1,9,2,8,1,7,0,8,1,8,1,8,1,9,2,4,655], key:1}, 2:{code:[254,0,44,0,1000000,1000000,1000000,2147483657,1000000,71,0,73,1], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:655, index: 2}, 656: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,6,0,20,1,23,1,7,23,1,8,19,645,7,23,1,9,23,1,10,23,1,11,16,1,11,2,16,1,10,3,8,1,9,0,8,1,6,1,3,10,1,32,0,2,5], key:0}, 1:{code:[30,0,15,1,8,0,20,1,23,1,9,23,1,10,15,1,11,2,15,1,12,3,8,1,8,0,8,1,10,1,8,1,11,2,8,1,12,3,4,656], key:1}}, clause_keys:[0, 1], next_key:2, key:656}, 657: {is_public:false, clauses:{0:{code:[28,1,17,1112,0,15,1,7,1,20,2,23,1,8,23,1,9,19,473,8,25,1,7,15,1,10,3,15,1,11,4,16,1,11,5,16,1,9,6,31,5], key:0}, 1:{code:[29,2,17,1120,0,15,1,7,1,20,2,23,1,8,23,1,9,19,473,8,25,1,7,15,1,10,3,15,1,11,4,16,1,11,5,16,1,9,6,31,5], key:1}, 2:{code:[30,0,17,1097,0,19,401,1,23,1,7,20,2,23,1,8,23,1,9,19,473,8,23,1,10,19,401,10,24,1,7,15,1,11,3,15,1,12,4,16,1,12,5,16,1,9,6,31,5], key:2}, 3:{code:[254,0,15,1,10,0,15,1,11,1,20,2,23,1,12,23,1,13,19,474,12,25,1,11,15,1,14,3,15,1,15,4,15,1,16,5,16,1,13,6,8,1,14,0,8,1,15,1,8,1,16,2,4,660], key:3}, 4:{code:[28,2147483666,44,0,2147483657,1000000,1000000,1000000,1000000,45,0,3,1097,2,1112,0,1120,1,30,0,74,3], key:4}}, clause_keys:[0, 1, 2, 3, 4], next_key:5, key:657, index: 4}, 658: {is_public:false, clauses:{0:{code:[254,0,15,1,10,0,15,1,11,1,15,1,12,2,19,401,3,23,1,13,15,1,14,4,20,5,23,1,15,23,1,16,19,454,15,23,1,17,25,1,14,19,401,17,24,1,13,15,1,18,6,15,1,19,7,16,1,19,8,16,1,16,9,5], key:0}, 1:{code:[28,2,17,1096,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,20,5,23,1,14,23,1,15,19,454,14,25,1,12,25,1,13,15,1,16,6,15,1,17,7,16,1,17,8,16,1,15,9,5], key:1}, 2:{code:[29,3,17,1112,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,20,5,23,1,14,23,1,15,19,454,14,25,1,12,25,1,13,15,1,16,6,15,1,17,7,16,1,17,8,16,1,15,9,5], key:2}, 3:{code:[29,4,1,60,0,33,0,17,1097,0,15,1,13,1,15,1,14,2,15,1,15,3,15,1,16,4,20,5,23,1,17,23,1,18,19,454,17,25,1,15,25,1,16,15,1,19,6,15,1,20,7,16,1,20,8,16,1,18,9,8,1,19,0,8,1,14,1,8,1,13,2,3,661,1,32,0,2,5], key:3}, 4:{code:[30,0,17,1097,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,20,5,23,1,14,23,1,15,19,455,14,25,1,12,25,1,13,15,1,16,6,15,1,17,7,16,1,17,8,16,1,15,9,5], key:4}, 5:{code:[28,2147483652,74,0,30,0,44,1,2147483661,1000000,1000000,1000000,1000000,45,0,3,1096,1,1097,2147483670,1112,2,71,3,73,4], key:5}}, clause_keys:[0, 1, 2, 3, 4, 5], next_key:6, key:658, index: 5}, 659: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,33,10,16,0,6,0,16,0,2,1,16,0,4,2,16,0,1,3,16,0,7,4,16,0,8,5,16,0,5,6,16,0,9,7,8,0,6,0,8,0,7,1,8,0,3,2,8,0,0,3,3,656,11,32,10,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,9,8,8,9,9,9,2,4,658], key:0}, 1:{code:[29,2,1,60,0,33,0,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,15,1,15,4,20,5,23,1,16,25,1,15,19,645,16,25,1,11,26,1097,23,1,17,19,399,17,23,1,18,20,6,23,1,19,23,1,20,19,452,19,23,1,21,25,1,13,19,399,21,24,1,18,16,1,20,7,8,1,11,0,8,1,14,1,12,399,2,24,1,18,3,646,1,32,0,2,5], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,16,0,0,4,20,5,23,1,12,25,0,1,19,645,12,25,1,8,26,1097,25,0,2,20,6,23,1,13,23,1,14,19,452,13,25,0,2,25,1,10,16,1,14,7,31,9,0,0,9,1,1,9,2,2,2,4,655], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:659}, 660: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,5,0,20,1,23,1,6,23,1,7,19,645,6,23,1,8,26,1097,23,1,9,20,2,23,1,10,24,1,7,19,645,10,24,1,8,26,1097,24,1,9,8,1,5,0,8,1,8,1,3,10,1,32,0,2,5], key:0}, 1:{code:[29,2,1,60,0,33,0,15,1,5,0,20,1,23,1,6,23,1,7,19,645,6,23,1,8,23,1,9,23,1,10,20,2,23,1,11,24,1,7,19,645,11,24,1,8,26,1112,24,1,10,8,1,5,0,8,1,8,1,3,10,1,32,0,2,5], key:1}, 2:{code:[30,0,15,1,6,0,20,1,23,1,7,23,1,8,20,2,24,1,7,23,1,9,8,1,6,0,8,1,8,1,8,1,9,2,4,660], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:660}, 661: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,33,2,15,1,5,0,16,0,1,1,20,2,23,1,6,23,1,7,19,641,6,23,1,8,23,1,9,23,1,10,19,642,10,25,0,0,8,1,5,0,8,1,8,1,3,10,3,32,2,9,0,0,9,1,1,2,4,35], key:0}, 1:{code:[30,0,15,1,6,0,15,1,7,1,20,2,23,1,8,23,1,9,8,1,6,0,8,1,7,1,8,1,9,2,4,661], key:1}}, clause_keys:[0, 1], next_key:2, key:661}, 662: {is_public:false, clauses:{0:{code:[28,1,21,0,0,15,1,4,1,15,1,5,2,16,1,4,3,31,5], key:0}, 1:{code:[29,2,15,1,4,0,20,1,23,1,5,23,1,6,19,600,5,23,1,7,15,1,8,2,20,3,23,1,9,24,1,6,19,600,9,25,1,8,31,5], key:1}, 2:{code:[30,0,15,1,8,0,20,1,23,1,9,23,1,10,15,1,11,2,20,3,24,1,9,23,1,12,8,1,8,0,8,1,10,1,8,1,11,2,8,1,12,3,4,662], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:662}, 663: {is_public:false, clauses:{0:{code:[28,1,15,1,5,0,17,0,1,17,0,2,15,1,6,3,16,1,6,4,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,16,0,0,0,20,1,23,1,7,25,0,1,19,641,7,23,1,8,23,1,9,23,1,10,19,642,10,23,1,11,16,0,2,2,16,0,5,3,16,0,4,4,8,1,11,0,8,0,0,1,3,37,7,32,6,8,0,3,0,12,195,1,25,0,5,27,1,3,34,6,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,663], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,16,0,0,0,20,1,23,1,5,25,0,1,20,2,24,1,5,25,0,2,16,0,3,3,16,0,4,4,31,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,663], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:663}, 664: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,60,15,19,229,0,25,0,13,25,0,0,16,0,1,1,16,0,10,2,16,0,3,3,16,0,11,4,16,0,12,5,16,0,14,6,16,0,7,7,16,0,15,8,16,0,9,9,31,9,13,0,8,0,1,1,8,0,10,2,10,409,3,8,0,11,4,8,0,12,5,9,14,6,8,0,6,7,9,15,8,8,0,8,9,3,665,16,8,0,2,0,12,198,1,25,0,10,27,1,3,34,13,9,10,0,9,11,1,8,0,4,2,9,12,3,8,0,5,4,3,663,13,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,9,8,8,9,9,9,2,4,664], key:0}, 1:{code:[30,0,15,1,20,0,15,1,21,1,15,1,22,2,15,1,23,3,15,1,24,4,15,1,25,5,15,1,26,6,15,1,27,7,15,1,28,8,15,1,29,9,8,1,20,0,8,1,21,1,8,1,22,2,8,1,23,3,8,1,24,4,8,1,25,5,8,1,26,6,8,1,27,7,8,1,28,8,8,1,29,9,4,665], key:1}}, clause_keys:[0, 1], next_key:2, key:664}, 665: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,19,612,0,25,0,3,25,0,6,16,0,8,1,16,0,9,2,16,0,0,3,16,0,10,4,16,0,2,5,16,0,14,6,16,0,12,7,16,0,13,8,16,0,5,9,31,8,0,6,0,8,0,1,1,3,494,15,8,0,7,0,8,0,1,1,3,494,15,8,0,7,0,10,140,1,3,584,15,8,0,9,0,9,14,1,8,0,1,2,8,0,11,3,3,662,15,9,6,0,9,7,1,9,8,2,9,9,3,14,0,4,9,10,5,9,11,6,9,12,7,9,13,8,8,0,4,9,3,666,14,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,667], key:0}, 1:{code:[29,2,19,621,0,23,1,13,23,1,14,15,1,15,1,15,1,16,2,15,1,17,3,15,1,18,4,15,1,19,5,15,1,20,6,20,7,23,1,21,25,1,20,19,645,21,24,1,14,26,1096,23,1,22,19,399,22,23,1,23,20,8,23,1,24,23,1,25,19,486,24,24,1,13,23,1,26,19,399,26,24,1,23,16,1,25,9,8,1,14,0,8,1,18,1,12,399,2,24,1,23,4,646], key:1}, 2:{code:[29,3,19,609,0,26,480,15,1,10,1,21,0,2,17,409,3,15,1,11,4,15,1,12,5,15,1,13,6,16,1,13,7,20,8,26,825,23,1,14,16,1,14,9,31,5], key:2}, 3:{code:[29,4,19,609,0,26,480,15,1,10,1,21,0,2,17,1131,3,15,1,11,4,15,1,12,5,15,1,13,6,16,1,13,7,20,8,26,825,23,1,14,20,14,26,793,23,1,15,16,1,15,9,31,5], key:3}, 4:{code:[29,5,1,60,0,60,1,60,2,19,615,0,25,0,0,15,1,10,1,15,1,11,2,17,409,3,16,0,1,4,15,1,12,5,15,1,13,6,16,1,13,7,20,8,23,1,14,23,1,15,19,484,14,23,1,16,19,399,16,25,0,2,16,1,15,9,31,9,0,0,9,1,1,12,399,2,25,0,2,2,4,668], key:4}, 5:{code:[29,6,1,60,0,60,1,60,2,19,615,0,25,0,0,15,1,10,1,15,1,11,2,17,1131,3,16,0,1,4,15,1,12,5,15,1,13,6,16,1,13,7,20,8,23,1,14,23,1,15,19,484,14,23,1,16,19,399,16,25,0,2,20,15,26,791,23,1,17,20,17,26,793,23,1,18,16,1,18,9,31,9,0,0,9,1,1,12,399,2,25,0,2,2,4,668], key:5}, 6:{code:[29,7,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,60,15,60,16,19,609,0,23,1,10,19,208,10,25,0,8,25,0,16,15,1,11,1,16,0,9,2,16,0,0,3,16,0,10,4,16,0,1,5,16,0,15,6,16,0,12,7,16,0,13,8,16,0,5,9,31,9,16,0,13,1,25,0,14,25,0,6,3,13,17,8,0,6,0,8,0,3,1,3,494,16,8,0,9,0,9,15,1,8,0,3,2,8,0,11,3,3,662,16,9,14,0,8,0,3,1,8,0,8,2,8,0,2,3,3,216,15,8,0,2,0,8,0,3,1,8,0,7,2,3,585,14,9,6,0,9,7,1,9,8,2,9,9,3,14,0,4,9,10,5,9,11,6,9,12,7,9,13,8,8,0,4,9,3,666,14,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,669], key:6}, 7:{code:[29,8,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,60,15,60,16,19,609,0,25,0,16,16,0,8,1,16,0,9,2,16,0,0,3,16,0,10,4,16,0,1,5,16,0,15,6,16,0,12,7,16,0,13,8,16,0,5,9,31,9,16,0,13,1,25,0,14,25,0,6,3,13,17,8,0,6,0,8,0,3,1,3,494,16,8,0,9,0,9,15,1,8,0,3,2,8,0,11,3,3,662,16,9,14,0,8,0,3,1,8,0,8,2,8,0,2,3,3,216,15,8,0,2,0,8,0,3,1,8,0,7,2,3,585,14,9,6,0,9,7,1,9,8,2,9,9,3,14,0,4,9,10,5,9,11,6,9,12,7,9,13,8,8,0,4,9,3,666,14,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,669], key:7}, 8:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,19,229,0,25,0,0,25,0,1,16,0,2,1,16,0,3,2,16,0,4,3,16,0,5,4,16,0,6,5,16,0,7,6,16,0,8,7,16,0,9,8,16,0,10,9,31,12,229,0,25,0,0,25,0,1,9,2,1,9,3,2,9,4,3,9,5,4,9,6,5,9,7,6,9,8,7,9,9,8,9,10,9,2,4,664], key:8}, 9:{code:[254,0,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,15,1,15,4,15,1,16,5,15,1,17,6,15,1,18,7,15,1,19,8,15,1,20,9,12,670,0,26,1132,25,1,11,4,143], key:9}, 10:{code:[28,2147483682,44,0,1000000,1000000,1000000,1000000,2147483657,46,0,5,229,8,609,2147483670,612,0,615,2147483678,621,1,71,2,72,3,72,6,73,7,71,4,73,5,30,0,74,9], key:10}}, clause_keys:[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10], next_key:11, key:665, index: 10}, 666: {is_public:false, clauses:{0:{code:[28,1,17,0,0,17,0,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,15,1,14,6,16,1,14,7,15,1,15,8,16,1,15,9,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,20,0,23,1,21,25,0,0,20,1,23,1,22,25,0,1,16,0,2,2,16,0,3,3,16,0,10,4,16,0,5,5,15,1,23,6,16,0,7,7,16,0,13,8,16,0,9,9,8,1,21,0,8,1,22,1,8,0,2,2,8,0,3,3,12,401,4,25,0,10,8,0,5,5,8,1,23,6,8,0,6,7,8,0,12,8,8,0,8,9,8,0,11,10,3,672,14,12,673,0,25,0,11,3,234,14,9,11,0,9,12,1,9,13,2,3,674,14,8,0,4,0,12,198,1,25,0,10,27,1,3,34,11,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,9,8,8,9,9,9,2,4,666], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:666, index: 2}, 667: {is_public:false, clauses:{0:{code:[28,1,17,1131,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,10,0,8,1,12,1,12,410,14,25,1,11,25,1,9,13,2,24,1,14,25,1,13,4,647], key:0}, 1:{code:[30,0,17,409,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,15,1,12,5,8,1,11,0,12,405,13,25,1,10,25,1,8,25,1,9,13,1,24,1,13,25,1,12,4,9], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1000000,1000000,45,0,2,409,1,1131,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:667, index: 2}, 668: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,6,0,15,1,7,1,15,1,8,2,8,1,6,0,8,1,7,1,8,1,8,2,3,646,1,32,0,2,5], key:0}, 1:{code:[30,0,15,1,4,0,15,1,5,1,15,1,6,2,12,671,0,25,1,4,25,1,5,4,143], key:1}}, clause_keys:[0, 1], next_key:2, key:668}, 669: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,17,1131,0,16,0,0,1,16,0,2,2,16,0,3,3,16,0,1,4,16,0,4,5,31,9,0,0,9,1,1,12,217,4,25,0,2,25,0,3,12,451,3,24,1,4,13,2,24,1,3,25,0,4,2,4,647], key:0}, 1:{code:[30,0,17,409,0,15,1,6,1,15,1,7,2,15,1,8,3,20,4,23,1,9,23,1,10,19,450,9,23,1,11,25,1,6,19,217,11,25,1,7,25,1,8,16,1,10,5,5], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1000000,1000000,45,0,2,409,1,1131,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:669, index: 2}, 672: {is_public:false, clauses:{0:{code:[254,0,15,1,13,0,17,62,1,15,1,14,2,15,1,15,3,15,1,16,4,15,1,17,5,15,1,18,6,15,1,19,7,15,1,20,8,15,1,21,9,15,1,22,10,7,23,0,8,1,13,1,7,24,2,8,1,14,3,7,25,4,7,26,5,8,1,15,6,8,1,18,7,8,1,19,8,8,1,17,9,8,1,20,10,8,1,16,11,7,27,12,8,1,21,13,8,1,22,14,41,103,15,28,2147483957,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,33,14,15,1,18,0,16,0,13,1,16,0,0,2,16,0,1,3,16,0,7,4,16,0,8,5,16,0,2,6,16,0,3,7,16,0,4,8,16,0,5,9,16,0,6,10,16,0,9,11,16,0,10,12,16,0,11,13,16,0,12,14,8,0,13,0,40,416,1,15,32,14,8,0,0,0,13,2,25,0,13,26,0,13,1,25,0,1,24,1,2,3,9,14,8,0,7,0,10,62,1,3,9,13,8,0,8,0,14,2,1,3,9,13,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,12,217,12,25,0,7,25,0,8,12,459,11,24,1,12,25,0,9,13,7,24,1,11,25,0,10,9,10,8,9,11,9,9,12,10,2,4,676,30,0,15,1,26,0,15,1,27,1,15,1,28,2,15,1,29,3,15,1,30,4,15,1,31,5,15,1,32,6,15,1,33,7,15,1,34,8,15,1,35,9,15,1,36,10,15,1,37,11,15,1,38,12,15,1,39,13,15,1,40,14,8,1,27,0,10,140,1,8,1,29,2,8,1,32,3,8,1,37,4,8,1,35,5,8,1,33,6,8,1,34,7,8,1,36,8,8,1,39,9,8,1,40,10,4,672,28,2147484076,15,1,2,0,8,1,2,0,4,3,30,0,15,1,3,0,8,1,3,0,41,441,1,28,2147484112,1,60,0,33,0,15,1,3,0,8,1,3,0,3,677,1,32,0,2,4,61,30,0,15,1,1,0,4,62], key:0}, 1:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,33,4,16,0,0,0,16,0,1,1,16,0,3,2,15,1,12,3,15,1,13,4,15,1,14,5,15,1,15,6,16,1,15,7,20,8,23,1,16,23,1,17,19,457,16,25,0,2,25,1,13,16,1,17,9,15,1,18,10,8,0,0,0,3,648,5,32,4,9,0,0,9,1,1,9,2,2,9,3,3,7,11,4,8,0,4,5,7,12,6,7,13,7,7,14,8,7,15,9,7,16,10,2,41,119,11,28,2147483859,1,60,0,60,1,60,2,60,3,60,4,33,3,16,0,2,0,16,0,4,1,16,0,0,2,16,0,1,3,15,1,13,4,15,1,14,5,15,1,15,6,15,1,16,7,15,1,17,8,15,1,18,9,15,1,19,10,8,0,2,0,3,4,5,9,4,0,10,62,1,3,9,5,32,3,9,0,0,12,208,1,25,0,1,25,0,2,2,4,9,30,0,15,1,13,0,15,1,14,1,15,1,15,2,15,1,16,3,15,1,17,4,15,1,18,5,15,1,19,6,15,1,20,7,15,1,21,8,15,1,22,9,15,1,23,10,8,1,18,0,8,1,13,1,8,1,14,2,8,1,19,3,8,1,20,4,8,1,21,5,8,1,22,6,8,1,16,7,8,1,23,8,8,1,15,9,41,300,10,28,2147484100,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,33,9,15,1,12,0,16,0,8,1,16,0,7,2,16,0,3,3,16,0,2,4,16,0,6,5,16,0,4,6,16,0,5,7,16,0,1,8,16,0,0,9,8,0,8,0,3,4,10,8,0,7,0,3,5,10,32,9,9,8,0,13,1,25,0,3,25,0,2,3,13,9,8,0,2,0,8,0,6,1,3,215,8,8,0,4,0,12,198,1,25,0,6,25,0,7,3,34,8,9,3,0,9,4,1,9,5,2,8,0,1,3,3,216,6,9,0,0,13,1,25,0,1,25,0,2,2,4,13,30,0,15,1,12,0,15,1,13,1,15,1,14,2,15,1,15,3,15,1,16,4,15,1,17,5,15,1,18,6,15,1,19,7,15,1,20,8,15,1,21,9,8,1,13,0,8,1,21,1,4,9], key:1}, 2:{code:[254,0,1,60,0,33,0,15,1,12,0,15,1,13,1,15,1,14,2,15,1,15,3,15,1,16,4,15,1,17,5,15,1,18,6,16,1,18,7,20,8,23,1,19,23,1,20,19,461,19,25,1,12,25,1,16,16,1,20,9,15,1,21,10,8,1,12,0,3,5,1,32,0,2,5], key:2}, 3:{code:[254,0,1,60,0,33,0,15,1,12,0,15,1,13,1,15,1,14,2,15,1,15,3,15,1,16,4,15,1,17,5,15,1,18,6,16,1,18,7,20,8,23,1,19,23,1,20,19,462,19,25,1,12,25,1,16,16,1,20,9,15,1,21,10,8,1,12,0,3,6,1,32,0,2,5], key:3}, 4:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,33,8,16,0,0,0,15,1,12,1,15,1,13,2,16,0,1,3,16,0,2,4,16,0,3,5,16,0,4,6,16,0,5,7,16,0,6,8,16,0,7,9,15,1,14,10,8,0,0,0,3,3,9,32,8,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,2,4,659], key:4}, 5:{code:[28,6,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,20,0,25,0,0,25,0,1,15,1,11,1,16,0,2,2,16,0,3,3,16,0,8,4,16,0,6,5,16,0,4,6,16,0,5,7,16,0,7,8,16,0,9,9,16,0,10,10,31,13,11,25,0,1,26,0,13,0,25,0,0,24,1,11,9,2,1,9,3,2,9,4,3,9,5,4,9,6,5,9,7,6,12,460,12,25,0,8,13,7,24,1,12,23,1,13,8,1,13,8,9,9,9,9,10,10,2,4,676], key:5}, 6:{code:[29,7,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,33,11,19,212,0,25,0,12,25,0,2,15,1,12,1,15,1,13,2,16,0,3,3,16,0,8,4,16,0,6,5,16,0,4,6,16,0,5,7,16,0,7,8,16,0,9,9,16,0,10,10,8,0,12,0,40,184,1,13,12,208,0,25,0,0,25,0,1,9,12,1,3,9,13,32,11,12,208,11,25,0,0,25,0,1,13,12,25,0,2,26,0,13,0,24,1,11,24,1,12,9,0,1,9,3,2,9,4,3,9,5,4,9,6,5,9,7,6,12,217,14,26,452,27,2,12,459,13,24,1,14,25,0,8,13,7,24,1,13,23,1,15,8,1,15,8,9,9,9,9,10,10,2,4,676,28,2147483855,1,60,0,33,0,15,1,3,0,8,1,3,0,3,3,1,32,0,2,4,61,30,0,15,1,1,0,4,62], key:6}, 7:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,19,212,0,25,0,12,25,0,13,16,0,14,1,16,0,1,2,16,0,2,3,16,0,9,4,16,0,5,5,16,0,3,6,16,0,4,7,16,0,6,8,16,0,10,9,16,0,11,10,31,7,21,0,9,12,1,8,0,0,2,9,13,3,8,0,8,4,8,0,7,5,7,22,6,7,23,7,7,24,8,7,25,9,9,14,10,7,26,11,8,0,1,12,7,27,13,7,28,14,7,29,15,7,30,16,7,31,17,7,32,18,7,33,19,7,34,20,40,214,21,15,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,12,217,12,25,0,7,25,0,8,12,459,11,24,1,12,25,0,9,13,7,24,1,11,23,1,13,8,1,13,8,9,10,9,9,11,10,2,4,676,28,2147484012,1,60,0,60,1,60,2,60,3,60,4,60,5,33,5,15,1,24,0,16,0,3,1,16,0,2,2,16,0,4,3,16,0,1,4,16,0,0,5,15,1,25,6,15,1,26,7,15,1,27,8,15,1,28,9,15,1,29,10,15,1,30,11,15,1,31,12,15,1,32,13,15,1,33,14,15,1,34,15,15,1,35,16,15,1,36,17,15,1,37,18,15,1,38,19,15,1,39,20,8,0,3,0,40,578,1,6,32,5,9,2,0,13,2,25,0,4,26,0,13,1,25,0,3,24,1,2,3,9,5,9,1,0,14,2,1,3,9,2,9,0,0,10,452,1,2,4,9,30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,60,15,60,16,60,17,60,18,60,19,15,1,23,0,16,0,3,1,16,0,4,2,16,0,6,3,16,0,8,4,16,0,7,5,16,0,12,6,16,0,15,7,16,0,11,8,16,0,0,9,16,0,1,10,16,0,2,11,16,0,5,12,16,0,9,13,16,0,10,14,16,0,13,15,16,0,14,16,16,0,16,17,16,0,17,18,16,0,18,19,16,0,19,20,8,0,3,0,13,1,25,0,12,25,0,15,3,13,20,8,0,15,0,8,0,11,1,3,494,20,9,1,0,9,2,1,9,3,2,9,4,3,9,5,4,9,6,5,9,7,6,9,8,7,9,9,8,9,10,9,9,11,10,9,12,11,9,13,12,9,14,13,9,15,14,9,16,15,9,17,16,9,18,17,9,19,18,2,41,609,19,28,2147484249,1,60,0,33,0,15,1,3,0,8,1,3,0,3,293,1,32,0,2,4,61,30,0,15,1,1,0,4,62,28,2147484423,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,33,6,15,1,22,0,16,0,7,1,16,0,4,2,16,0,2,3,16,0,3,4,16,0,5,5,16,0,1,6,16,0,0,7,15,1,23,8,15,1,24,9,15,1,25,10,15,1,26,11,15,1,27,12,15,1,28,13,15,1,29,14,15,1,30,15,15,1,31,16,15,1,32,17,15,1,33,18,8,1,22,0,10,62,1,3,9,8,8,0,4,0,40,924,1,8,32,6,9,2,0,12,208,3,25,0,3,25,0,4,12,212,2,24,1,3,25,0,5,13,1,24,1,2,26,0,3,9,6,9,1,0,10,452,1,3,9,2,9,0,0,14,2,1,2,4,9,30,0,15,1,21,0,15,1,22,1,15,1,23,2,15,1,24,3,15,1,25,4,15,1,26,5,15,1,27,6,15,1,28,7,15,1,29,8,15,1,30,9,15,1,31,10,15,1,32,11,15,1,33,12,15,1,34,13,15,1,35,14,15,1,36,15,15,1,37,16,15,1,38,17,15,1,39,18,8,1,21,0,8,1,30,1,8,1,31,2,8,1,32,3,8,1,25,4,8,1,33,5,8,1,34,6,8,1,35,7,8,1,24,8,8,1,26,9,8,1,28,10,8,1,27,11,8,1,36,12,8,1,23,13,8,1,37,14,8,1,38,15,8,1,39,16,41,955,17,28,2147484595,1,60,0,33,0,15,1,3,0,8,1,3,0,3,677,1,32,0,2,4,61,30,0,15,1,1,0,4,62,28,2147484797,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,33,12,16,0,11,0,16,0,8,1,16,0,10,2,16,0,7,3,16,0,9,4,16,0,5,5,16,0,3,6,16,0,6,7,16,0,2,8,16,0,4,9,16,0,1,10,16,0,0,11,15,1,19,12,15,1,20,13,15,1,21,14,15,1,22,15,15,1,23,16,8,0,11,0,3,5,13,32,12,8,0,8,0,12,198,1,25,0,10,25,0,11,3,34,12,9,7,0,9,8,1,9,9,2,8,0,5,3,3,216,10,8,0,3,0,13,1,25,0,5,25,0,6,3,13,7,9,2,0,13,2,25,0,4,26,0,13,1,25,0,3,24,1,2,3,9,5,9,1,0,14,2,1,3,9,2,9,0,0,10,452,1,2,4,9,30,0,15,1,19,0,15,1,20,1,15,1,21,2,15,1,22,3,15,1,23,4,15,1,24,5,15,1,25,6,15,1,26,7,15,1,27,8,15,1,28,9,15,1,29,10,15,1,30,11,15,1,31,12,15,1,32,13,15,1,33,14,15,1,34,15,15,1,35,16,8,1,19,0,8,1,32,1,8,1,33,2,8,1,34,3,8,1,23,4,8,1,35,5,8,1,25,6,8,1,27,7,8,1,28,8,8,1,29,9,8,1,30,10,41,1266,11,28,2147485076,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,33,10,15,1,14,0,16,0,9,1,16,0,5,2,16,0,7,3,16,0,8,4,16,0,6,5,16,0,3,6,16,0,2,7,16,0,4,8,16,0,1,9,16,0,0,10,8,1,14,0,10,425,1,3,9,11,32,10,9,9,0,8,0,5,1,8,0,7,2,3,251,10,9,7,0,9,8,1,8,0,6,2,3,213,9,8,0,3,0,9,5,1,9,6,2,3,251,7,9,2,0,13,2,25,0,4,26,0,13,1,25,0,3,24,1,2,3,9,5,9,1,0,14,2,1,3,9,2,9,0,0,10,452,1,2,4,9,30,0,1,60,0,60,1,15,1,13,0,15,1,14,1,15,1,15,2,15,1,16,3,15,1,17,4,15,1,18,5,15,1,19,6,15,1,20,7,15,1,21,8,16,0,1,9,16,0,0,10,8,1,20,0,13,22,25,1,21,26,0,13,1,25,1,14,24,1,22,3,9,2,9,1,0,14,2,1,3,9,2,9,0,0,10,452,1,2,4,9], key:7}, 8:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,16,0,3,0,16,0,0,1,16,0,2,2,16,0,7,3,16,0,8,4,16,0,9,5,16,0,10,6,16,0,11,7,16,0,12,8,16,0,13,9,16,0,14,10,8,0,4,0,12,678,1,25,0,7,25,0,8,25,0,9,25,0,10,25,0,11,25,0,12,25,0,13,25,0,14,3,9,15,8,0,3,0,13,1,25,0,1,25,0,6,3,13,15,8,0,6,0,8,0,5,1,3,494,15,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,7,29,5,7,30,6,9,5,7,9,6,8,7,31,9,7,32,10,7,33,11,7,34,12,9,7,13,9,8,14,9,9,15,9,10,16,9,11,17,9,12,18,9,13,19,9,14,20,7,35,21,7,36,22,7,37,23,7,38,24,7,39,25,7,40,26,7,41,27,7,42,28,2,41,228,29,28,2147484058,1,60,0,60,1,60,2,60,3,60,4,33,3,15,1,32,0,16,0,4,1,16,0,0,2,16,0,1,3,16,0,2,4,15,1,33,5,15,1,34,6,15,1,35,7,15,1,36,8,15,1,37,9,15,1,38,10,15,1,39,11,15,1,40,12,15,1,41,13,15,1,42,14,15,1,43,15,15,1,44,16,15,1,45,17,15,1,46,18,15,1,47,19,15,1,48,20,15,1,49,21,15,1,50,22,15,1,51,23,15,1,52,24,15,1,53,25,15,1,54,26,15,1,55,27,15,1,56,28,8,1,32,0,10,62,1,3,9,5,9,4,0,10,62,1,3,259,5,32,3,10,62,0,14,2,1,13,5,25,0,1,26,0,13,2,25,0,0,24,1,5,9,0,3,9,2,4,2,4,679,30,0,15,1,31,0,15,1,32,1,15,1,33,2,15,1,34,3,15,1,35,4,15,1,36,5,15,1,37,6,15,1,38,7,15,1,39,8,15,1,40,9,15,1,41,10,15,1,42,11,15,1,43,12,15,1,44,13,15,1,45,14,15,1,46,15,15,1,47,16,15,1,48,17,15,1,49,18,15,1,50,19,15,1,51,20,15,1,52,21,15,1,53,22,15,1,54,23,15,1,55,24,15,1,56,25,15,1,57,26,15,1,58,27,15,1,59,28,8,1,31,0,8,1,37,1,8,1,32,2,8,1,38,3,8,1,39,4,8,1,40,5,8,1,41,6,8,1,42,7,8,1,43,8,8,1,44,9,8,1,45,10,8,1,46,11,8,1,47,12,8,1,48,13,8,1,49,14,8,1,50,15,8,1,51,16,8,1,52,17,8,1,53,18,8,1,54,19,8,1,35,20,8,1,33,21,8,1,55,22,8,1,56,23,8,1,57,24,8,1,58,25,8,1,59,26,8,1,34,27,41,643,28,28,2147484530,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,60,15,60,16,60,17,60,18,60,19,60,20,60,21,60,22,33,22,16,0,19,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,16,0,5,6,16,0,6,7,16,0,7,8,16,0,8,9,16,0,9,10,16,0,10,11,16,0,11,12,16,0,12,13,16,0,13,14,16,0,14,15,16,0,15,16,16,0,16,17,16,0,17,18,16,0,18,19,16,0,20,20,16,0,21,21,15,1,30,22,15,1,31,23,15,1,32,24,15,1,33,25,15,1,34,26,15,1,35,27,8,0,19,0,3,5,23,32,22,9,1,0,9,2,1,9,3,2,9,4,3,9,5,4,9,6,5,9,7,6,9,8,7,9,9,8,9,10,9,9,11,10,9,12,11,9,13,12,9,14,13,9,15,14,9,16,15,9,17,16,9,18,17,9,19,18,9,20,19,9,21,20,2,41,1079,21,30,0,15,1,30,0,15,1,31,1,15,1,32,2,15,1,33,3,15,1,34,4,15,1,35,5,15,1,36,6,15,1,37,7,15,1,38,8,15,1,39,9,15,1,40,10,15,1,41,11,15,1,42,12,15,1,43,13,15,1,44,14,15,1,45,15,15,1,46,16,15,1,47,17,15,1,48,18,15,1,49,19,15,1,50,20,15,1,51,21,15,1,52,22,15,1,53,23,15,1,54,24,15,1,55,25,15,1,56,26,15,1,57,27,8,1,30,0,8,1,53,1,8,1,32,2,8,1,33,3,8,1,34,4,8,1,35,5,8,1,54,6,8,1,55,7,8,1,56,8,8,1,39,9,8,1,40,10,8,1,41,11,8,1,42,12,8,1,43,13,8,1,44,14,8,1,45,15,8,1,46,16,8,1,50,17,8,1,57,18,8,1,51,19,41,1708,20,28,2147484950,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,60,15,60,16,60,17,60,18,60,19,33,18,15,1,24,0,16,0,19,1,16,0,17,2,16,0,3,3,16,0,2,4,16,0,0,5,16,0,1,6,16,0,4,7,16,0,5,8,16,0,6,9,16,0,7,10,16,0,8,11,16,0,9,12,16,0,10,13,16,0,11,14,16,0,12,15,16,0,13,16,16,0,14,17,16,0,15,18,16,0,16,19,15,1,25,20,8,1,24,0,10,62,1,3,9,20,9,19,0,14,2,1,3,9,20,32,18,9,17,0,13,2,25,0,2,26,0,13,1,25,0,3,24,1,2,3,9,18,9,1,0,9,2,1,9,3,2,9,4,3,9,5,4,9,6,5,9,7,6,9,8,7,9,9,8,9,10,9,9,11,10,9,12,11,9,13,12,9,14,13,9,15,14,9,16,15,2,41,1414,16,30,0,15,1,27,0,15,1,28,1,15,1,29,2,15,1,30,3,15,1,31,4,15,1,32,5,15,1,33,6,15,1,34,7,15,1,35,8,15,1,36,9,15,1,37,10,15,1,38,11,15,1,39,12,15,1,40,13,15,1,41,14,15,1,42,15,15,1,43,16,15,1,44,17,15,1,45,18,15,1,46,19,15,1,47,20,8,1,27,0,8,1,28,1,8,1,29,2,8,1,47,3,8,1,45,4,8,1,46,5,4,680,28,2147485199,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,33,10,15,1,19,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,16,0,5,6,16,0,6,7,16,0,7,8,16,0,8,9,16,0,9,10,15,1,20,11,15,1,21,12,15,1,22,13,15,1,23,14,15,1,24,15,8,0,0,0,40,1677,1,11,32,10,9,0,0,10,62,1,9,1,2,9,2,3,9,3,4,9,4,5,9,5,6,9,6,7,9,7,8,9,8,9,9,9,10,2,4,672,30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,15,1,18,0,15,1,19,1,16,0,3,2,15,1,20,3,15,1,21,4,15,1,22,5,15,1,23,6,15,1,24,7,15,1,25,8,15,1,26,9,15,1,27,10,16,0,0,11,16,0,2,12,16,0,1,13,16,0,4,14,16,0,5,15,8,1,19,0,13,1,25,0,0,25,0,2,3,13,6,8,0,2,0,8,0,1,1,3,494,6,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,680,28,2147485348,1,60,0,33,0,15,1,3,0,8,1,3,0,3,293,1,32,0,2,4,61,30,0,15,1,1,0,4,62,28,2147485551,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,60,15,60,16,60,17,60,18,60,19,33,19,15,1,23,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,16,0,5,6,16,0,6,7,16,0,7,8,16,0,8,9,16,0,9,10,16,0,10,11,16,0,11,12,16,0,12,13,16,0,13,14,16,0,14,15,16,0,15,16,16,0,16,17,16,0,17,18,16,0,18,19,8,1,23,0,10,425,1,3,9,20,32,19,9,1,0,9,2,1,9,3,2,9,4,3,9,5,4,9,6,5,9,7,6,9,8,7,9,9,8,9,10,9,9,11,10,9,12,11,9,13,12,9,14,13,9,15,14,9,16,15,9,17,16,9,18,17,2,41,2007,18,30,0,15,1,25,0,15,1,26,1,15,1,27,2,15,1,28,3,15,1,29,4,15,1,30,5,15,1,31,6,15,1,32,7,15,1,33,8,15,1,34,9,15,1,35,10,15,1,36,11,15,1,37,12,15,1,38,13,15,1,39,14,15,1,40,15,15,1,41,16,15,1,42,17,15,1,43,18,15,1,44,19,8,1,27,0,8,1,28,1,8,1,29,2,8,1,44,3,8,1,42,4,4,679,28,2147485846,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,60,15,33,14,15,1,21,0,16,0,15,1,16,0,13,2,16,0,3,3,16,0,2,4,16,0,0,5,16,0,1,6,16,0,4,7,16,0,5,8,16,0,6,9,16,0,7,10,16,0,8,11,16,0,9,12,16,0,10,13,16,0,11,14,16,0,12,15,15,1,22,16,15,1,23,17,8,1,21,0,10,62,1,3,9,16,9,15,0,14,2,1,3,9,16,32,14,9,13,0,13,2,25,0,2,26,0,13,1,25,0,3,24,1,2,3,9,14,9,1,0,9,2,1,9,3,2,9,4,3,9,5,4,9,6,5,9,7,6,9,8,7,9,9,8,9,10,9,9,11,10,9,12,11,2,41,2286,12,30,0,15,1,21,0,15,1,22,1,15,1,23,2,15,1,24,3,15,1,25,4,15,1,26,5,15,1,27,6,15,1,28,7,15,1,29,8,15,1,30,9,15,1,31,10,15,1,32,11,15,1,33,12,15,1,34,13,15,1,35,14,15,1,36,15,15,1,37,16,15,1,38,17,8,1,37,0,8,1,38,1,8,1,36,2,4,681,28,2147486055,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,33,10,15,1,15,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,16,0,5,6,16,0,6,7,16,0,7,8,16,0,8,9,16,0,9,10,15,1,16,11,8,0,0,0,40,2471,1,11,32,10,9,0,0,10,62,1,9,1,2,9,2,3,9,3,4,9,4,5,9,5,6,9,6,7,9,7,8,9,8,9,9,9,10,2,4,672,30,0,15,1,15,0,15,1,16,1,15,1,17,2,15,1,18,3,15,1,19,4,15,1,20,5,15,1,21,6,15,1,22,7,15,1,23,8,15,1,24,9,15,1,25,10,15,1,26,11,8,1,16,0,8,1,17,1,8,1,26,2,4,681,28,2147486142,1,60,0,33,0,15,1,3,0,8,1,3,0,3,293,1,32,0,2,4,61,30,0,15,1,1,0,4,62], key:8}, 9:{code:[28,2147483652,74,0,29,2147483656,74,1,29,2147483660,74,2,29,2147483664,74,3,29,2147483668,74,4,29,2147483686,44,5,1000000,1000000,1000000,5,2147483677,46,0,1,212,2147483682,71,6,73,7,30,0,74,8], key:9}}, clause_keys:[0, 1, 2, 3, 4, 5, 6, 7, 8, 9], next_key:10, key:672, index: 9}, 674: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,33,2,15,1,4,0,16,0,0,1,16,0,1,2,8,1,4,0,3,3,3,32,2,9,0,0,9,1,1,2,4,9], key:0}, 1:{code:[30,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,6,0,8,1,7,1,4,675], key:1}}, clause_keys:[0, 1], next_key:2, key:674}, 675: {is_public:false, clauses:{0:{code:[254,0,15,1,6,0,15,1,7,1,8,1,6,0,8,1,7,1,10,0,2,10,0,3,4,682], key:0}}, clause_keys:[0], next_key:1, key:675}, 676: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,11,1,15,1,12,2,15,1,13,3,16,1,13,4,15,1,14,5,15,1,15,6,16,1,15,7,15,1,16,8,16,1,16,9,15,1,17,10,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,33,11,20,0,23,1,12,25,0,0,16,0,1,1,16,0,2,2,16,0,3,3,16,0,4,4,16,0,5,5,16,0,6,6,16,0,7,7,20,8,23,1,13,25,0,8,19,475,13,24,1,12,16,0,9,9,16,0,10,10,8,1,12,0,3,648,12,32,11,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,9,8,8,9,9,9,9,10,10,2,4,676], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,33,11,20,0,23,1,12,25,0,0,16,0,1,1,16,0,2,2,16,0,3,3,16,0,4,4,16,0,5,5,16,0,6,6,16,0,7,7,20,8,23,1,13,25,0,8,19,476,13,24,1,12,16,0,9,9,16,0,10,10,8,1,12,0,3,5,12,32,11,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,9,8,8,9,9,9,9,10,10,2,4,676], key:2}, 3:{code:[29,4,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,33,13,20,0,25,0,10,25,0,0,16,0,1,1,16,0,2,2,16,0,11,3,16,0,4,4,16,0,5,5,16,0,6,6,16,0,7,7,16,0,12,8,16,0,9,9,17,1162,10,8,0,10,0,3,3,14,32,13,9,10,0,8,0,5,1,9,11,2,8,0,3,3,9,12,4,8,0,8,5,3,653,13,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,9,8,8,9,9,9,7,11,10,2,4,676], key:3}, 4:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,20,0,25,0,11,25,0,0,16,0,1,1,16,0,2,2,15,1,14,3,16,0,4,4,16,0,5,5,16,0,14,6,16,0,7,7,20,8,23,1,15,25,0,8,19,473,15,25,0,12,16,0,9,9,16,0,10,10,8,1,14,0,8,0,13,1,8,0,12,2,3,655,15,9,11,0,10,140,1,8,0,1,2,8,0,2,3,9,12,4,8,0,5,5,9,13,6,8,0,3,7,9,14,8,8,0,6,9,8,0,10,10,3,672,15,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,9,8,8,9,9,9,9,10,10,2,4,676], key:4}, 5:{code:[254,0,44,0,2147483657,1000000,1000000,2147483662,1000000,45,0,1,0,0,71,1,72,2,72,3,73,4], key:5}}, clause_keys:[0, 1, 2, 3, 4, 5], next_key:6, key:676, index: 5}, 677: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,15,1,4,0,8,1,4,0,8,0,0,1,8,0,1,2,3,11,2,9,0,0,9,1,1,7,7,2,7,8,3,7,9,4,7,10,5,7,11,6,2,41,51,7,28,2147483760,1,60,0,60,1,33,0,15,1,10,0,16,0,1,1,15,1,11,2,15,1,12,3,15,1,13,4,15,1,14,5,15,1,15,6,8,1,10,0,10,62,1,3,9,2,9,1,0,14,2,1,3,9,2,32,0,2,4,62,30,0,15,1,9,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,15,1,14,5,15,1,15,6,8,1,9,0,8,1,12,1,8,1,13,2,8,1,14,3,8,1,15,4,41,165,5,254,0,1,60,0,60,1,60,2,60,3,60,4,33,0,15,1,8,0,16,0,4,1,16,0,2,2,16,0,1,3,16,0,3,4,8,1,8,0,8,0,4,1,3,24,5,13,0,27,58,26,0,13,1,25,0,2,26,0,3,9,5,8,0,1,0,13,1,25,0,2,25,0,3,9,4,2,3,303,5,9,1,0,3,588,2,32,0,2,4,62], key:0}}, clause_keys:[0], next_key:1, key:677}, 679: {is_public:false, clauses:{0:{code:[254,0,15,1,16,0,15,1,17,1,15,1,18,2,15,1,19,3,19,678,4,23,1,20,23,1,21,23,1,22,23,1,23,23,1,24,23,1,25,23,1,26,23,1,27,8,1,18,0,8,1,19,1,8,1,20,2,8,1,23,3,8,1,24,4,8,1,22,5,8,1,25,6,12,217,29,25,1,16,25,1,17,12,459,28,24,1,29,24,1,21,13,7,24,1,28,23,1,30,8,1,30,8,8,1,26,9,8,1,27,10,4,676], key:0}}, clause_keys:[0], next_key:1, key:679}, 680: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,16,0,5,0,16,0,1,1,16,0,2,2,16,0,3,3,15,1,8,4,16,0,4,5,8,0,6,0,12,198,1,25,0,1,25,1,8,3,34,7,9,5,0,9,6,1,8,0,3,2,8,0,0,3,3,216,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,679], key:0}}, clause_keys:[0], next_key:1, key:680}, 681: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,15,1,6,0,16,0,3,1,16,0,4,2,8,1,6,0,8,0,6,1,8,0,8,2,3,251,9,9,8,0,8,0,3,1,8,0,7,2,3,213,9,8,0,5,0,9,6,1,9,7,2,3,251,8,9,5,0,13,1,25,0,0,25,0,2,3,13,6,8,0,2,0,8,0,1,1,3,494,5,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,679], key:0}}, clause_keys:[0], next_key:1, key:681}, 682: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,33,2,15,1,5,0,16,1,5,1,16,0,0,2,16,0,1,3,8,1,5,0,3,3,3,32,2,12,683,0,25,0,0,25,0,1,3,234,2,9,0,0,9,1,1,2,4,684], key:0}, 1:{code:[28,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,33,7,20,0,23,1,6,25,0,0,19,473,6,23,1,7,19,401,7,25,0,4,20,1,25,0,3,25,0,1,16,0,5,2,16,0,6,3,8,0,2,0,12,401,8,25,0,4,12,473,1,24,1,8,3,9,8,7,4,0,7,5,1,8,0,4,2,8,0,6,3,40,152,4,8,8,0,2,0,7,4,1,7,5,2,8,0,5,3,40,225,4,8,32,7,9,0,0,9,1,1,12,217,5,25,0,3,25,0,4,12,195,4,25,0,2,24,1,5,13,2,24,1,4,25,0,5,9,6,3,2,4,682,28,2147483853,1,60,0,33,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,12,217,11,25,1,8,25,1,9,12,195,0,25,1,7,24,1,11,8,1,10,1,3,324,1,32,0,2,4,61,30,0,15,1,4,0,15,1,5,1,15,1,6,2,15,1,7,3,4,62,28,2147483926,1,60,0,33,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,12,217,11,25,1,8,25,1,9,12,195,0,25,1,7,24,1,11,8,1,10,1,3,324,1,32,0,2,4,61,30,0,15,1,4,0,15,1,5,1,15,1,6,2,15,1,7,3,4,62], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,33,7,20,0,23,1,6,25,0,0,19,472,6,23,1,7,19,401,7,25,0,5,20,1,25,0,4,25,0,1,16,0,2,2,16,0,6,3,8,0,3,0,12,401,8,25,0,5,12,472,1,24,1,8,3,9,8,32,7,9,0,0,9,1,1,9,2,2,12,217,5,25,0,4,25,0,5,12,195,4,25,0,3,24,1,5,13,3,24,1,4,25,0,6,2,4,682], key:2}, 3:{code:[30,0,20,0,23,1,8,23,1,9,20,1,24,1,8,23,1,10,15,1,11,2,15,1,12,3,8,1,9,0,8,1,10,1,8,1,11,2,8,1,12,3,4,682], key:3}, 4:{code:[28,2147483652,74,0,30,0,44,1,1000000,1000000,1000000,2147483661,1000000,71,1,72,2,73,3], key:4}}, clause_keys:[0, 1, 2, 3, 4], next_key:5, key:682, index: 4}, 684: {is_public:false, clauses:{0:{code:[28,1,1,60,0,16,0,0,0,17,0,1,31,9,0,0,2,4,685], key:0}, 1:{code:[30,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,4,686], key:1}}, clause_keys:[0, 1], next_key:2, key:684}, 685: {is_public:false, clauses:{0:{code:[28,1,17,0,0,5], key:0}, 1:{code:[30,0,20,0,23,1,2,23,1,3,19,195,2,23,1,4,23,1,5,19,217,5,24,1,4,23,1,6,8,1,3,0,4,685], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:685, index: 2}, 686: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,8,1,3,0,4,687], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,33,2,20,0,23,1,4,25,0,0,19,195,4,23,1,5,23,1,6,19,217,6,23,1,7,23,1,8,16,0,1,1,12,217,9,24,1,5,24,1,8,12,195,0,24,1,7,24,1,9,8,0,1,1,3,324,3,32,2,9,0,0,9,1,1,2,4,686], key:1}, 2:{code:[30,0,20,0,23,1,4,23,1,5,19,195,4,23,1,6,23,1,7,19,217,7,24,1,6,23,1,8,15,1,9,1,8,1,5,0,8,1,9,1,4,686], key:2}, 3:{code:[254,0,44,0,2147483657,1000000,1000000,2147483662,1000000,45,0,1,0,0,71,1,73,2], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:686, index: 3}, 687: {is_public:false, clauses:{0:{code:[28,1,17,0,0,5], key:0}, 1:{code:[29,2,1,60,0,20,0,23,1,1,25,0,0,19,195,1,23,1,2,23,1,3,19,217,3,24,1,2,23,1,4,31,9,0,0,2,4,687], key:1}, 2:{code:[30,0,20,0,23,1,2,23,1,3,8,1,3,0,4,687], key:2}, 3:{code:[254,0,44,0,2147483657,1000000,1000000,2147483662,1000000,45,0,1,0,0,71,1,73,2], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:687, index: 3}, 688: {is_public:false, clauses:{0:{code:[28,1,17,0,0,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,33,1,20,0,23,1,2,25,0,0,8,1,2,0,3,689,2,32,1,9,0,0,2,4,688], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:688, index: 2}, 689: {is_public:false, clauses:{0:{code:[28,1,1,60,0,19,690,0,25,0,0,31,9,0,0,2,4,111], key:0}, 1:{code:[30,0,1,60,0,60,1,15,1,3,0,8,1,3,0,8,0,0,1,3,230,2,8,0,0,0,3,377,2,8,0,0,0,10,398,1,8,0,1,2,3,266,2,8,0,1,0,3,691,2,9,1,0,3,267,2,8,0,0,0,3,378,1,12,692,1,25,0,0,12,208,0,26,445,24,1,1,3,295,1,12,692,1,25,0,0,12,208,0,26,445,24,1,1,2,4,209], key:1}}, clause_keys:[0, 1], next_key:2, key:689}, 691: {is_public:false, clauses:{0:{code:[254,0,1,60,0,16,0,0,0,8,0,0,0,3,693,1,8,0,0,0,12,541,2,27,0,12,542,1,27,0,24,1,2,3,695,1,3,497,1,8,0,0,0,3,694,1,7,2,0,9,0,1,2,41,56,2,28,2147483745,1,60,0,60,1,60,2,33,2,16,0,0,0,16,0,1,1,8,0,0,0,8,0,1,1,3,577,3,32,2,9,0,0,9,1,1,2,4,227,30,0,15,1,2,0,15,1,3,1,4,62], key:0}}, clause_keys:[0], next_key:1, key:691}, 693: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,33,0,16,0,1,0,8,0,2,0,7,3,1,40,47,2,3,10,1178,0,13,1,25,0,1,25,0,2,7,3,2,3,146,3,32,0,2,5,28,2147483734,1,60,0,60,1,33,1,15,1,6,0,16,0,0,1,10,1178,0,8,1,6,1,8,0,0,2,3,148,2,32,1,9,0,0,2,4,149,30,0,15,1,4,0,15,1,5,1,8,1,4,0,10,0,1,4,9], key:0}}, clause_keys:[0], next_key:1, key:693}, 694: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,33,0,15,1,4,0,10,1178,0,13,1,25,1,4,25,0,1,8,0,2,2,3,148,3,9,2,0,3,149,3,10,1178,0,9,1,1,7,3,2,3,146,2,32,0,2,5], key:0}}, clause_keys:[0], next_key:1, key:694}, 695: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,16,0,0,0,16,0,2,1,8,0,0,0,8,0,5,1,12,696,5,25,0,3,12,697,7,25,0,4,13,6,24,1,7,26,0,13,2,24,1,5,24,1,6,3,70,6,9,3,0,9,4,1,9,5,2,8,0,1,3,3,698,6,9,0,0,9,1,1,9,2,2,2,4,699], key:0}}, clause_keys:[0], next_key:1, key:695}, 698: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,4,1,15,1,5,2,16,1,5,3,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,20,0,25,0,3,25,0,4,16,0,5,1,16,0,2,2,15,1,6,3,8,0,2,0,8,1,6,1,3,14,6,8,0,1,0,7,2,1,40,111,2,6,9,5,0,3,700,6,13,0,25,0,3,25,0,4,8,0,0,1,3,701,5,12,702,1,25,0,0,12,208,3,25,0,1,25,0,2,12,703,2,24,1,3,12,704,0,24,1,1,24,1,2,2,4,64,28,2147483790,1,60,0,33,0,15,1,5,0,15,1,6,1,8,1,5,0,8,1,6,1,3,577,1,32,0,2,4,62,30,0,15,1,4,0,15,1,5,1,8,1,4,0,10,613,1,4,9], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:698, index: 2}, 699: {is_public:false, clauses:{0:{code:[28,1,1,60,0,15,1,3,0,17,1196,1,16,0,0,2,31,9,0,0,2,41,24,1,28,2147483708,1,60,0,33,0,15,1,4,0,8,1,4,0,12,541,5,27,0,12,542,1,27,0,24,1,5,3,9,1,32,0,2,4,62,30,0,15,1,2,0,10,1197,0,4,143], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,33,2,16,0,0,0,16,0,3,1,16,0,4,2,8,0,3,0,40,59,1,5,9,3,0,9,4,1,8,0,1,2,3,543,5,32,2,9,0,0,9,1,1,2,4,695,28,2147483743,1,60,0,60,1,33,1,16,0,0,0,10,196,0,10,1198,1,3,16,2,32,1,12,705,0,25,0,0,3,64,1,2,4,323,30,0,15,1,1,0,4,62], key:1}}, clause_keys:[0, 1], next_key:2, key:699}, 700: {is_public:false, clauses:{0:{code:[28,1,17,0,0,5], key:0}, 1:{code:[30,0,1,60,0,20,0,23,1,3,25,0,0,19,625,3,23,1,4,23,1,5,8,1,4,0,8,1,5,1,3,9,1,9,0,0,2,4,700], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:700, index: 2}, 701: {is_public:false, clauses:{0:{code:[28,1,17,0,0,17,0,1,5], key:0}, 1:{code:[30,0,20,0,23,1,4,23,1,5,19,625,4,23,1,6,23,1,7,20,1,24,1,6,23,1,8,8,1,5,0,8,1,8,1,4,701], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:701, index: 2}, 706: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,33,0,16,0,2,0,15,1,5,1,8,1,5,0,10,399,1,8,0,1,2,3,266,3,8,0,1,0,10,1200,1,10,0,2,3,79,3,8,0,1,0,10,1201,1,10,0,2,3,79,3,8,0,1,0,10,1202,1,10,0,2,3,79,3,8,0,1,0,10,1203,1,13,2,25,0,2,26,0,3,79,3,8,0,1,0,10,1204,1,10,0,2,3,79,2,8,0,1,0,3,102,2,8,0,1,0,10,1205,1,10,0,2,3,79,2,9,1,0,3,267,2,32,0,2,5], key:0}}, clause_keys:[0], next_key:1, key:706}, 707: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,101,2,9,0,0,9,1,1,2,4,706], key:0}}, clause_keys:[0], next_key:1, key:707}, 708: {is_public:false, clauses:{0:{code:[28,1,17,0,0,5], key:0}, 1:{code:[30,0,1,60,0,20,0,23,1,2,25,0,0,8,1,2,0,3,222,1,9,0,0,2,4,708], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:708, index: 2}, 709: {is_public:false, clauses:{0:{code:[28,1,17,0,0,5], key:0}, 1:{code:[30,0,1,60,0,20,0,23,1,2,25,0,0,8,1,2,0,3,710,1,9,0,0,2,4,709], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:709, index: 2}, 710: {is_public:false, clauses:{0:{code:[254,0,1,60,0,15,1,3,0,8,1,3,0,8,0,0,1,3,74,1,9,0,0,2,4,711], key:0}}, clause_keys:[0], next_key:1, key:710}, 711: {is_public:false, clauses:{0:{code:[254,0,1,60,0,16,0,0,0,8,0,0,0,3,712,1,9,0,0,2,4,78], key:0}}, clause_keys:[0], next_key:1, key:711}, 712: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,33,0,15,1,4,0,8,1,4,0,10,398,1,8,0,1,2,3,77,2,8,0,1,0,3,691,2,9,1,0,3,267,2,32,0,2,5], key:0}}, clause_keys:[0], next_key:1, key:712}, 713: {is_public:false, clauses:{0:{code:[254,0,7,2,0,41,8,1,28,2147483692,1,60,0,16,0,0,0,12,592,2,25,0,0,12,208,0,26,445,24,1,2,3,265,1,9,0,0,40,52,1,1,2,4,61,30,0,15,1,1,0,4,62,254,0,1,60,0,33,0,15,1,3,0,8,1,3,0,3,222,1,32,0,2,5], key:0}}, clause_keys:[0], next_key:1, key:713}, 714: {is_public:false, clauses:{0:{code:[254,0,1,15,1,2,0,8,1,2,0,3,710,0,2,4,713], key:0}}, clause_keys:[0], next_key:1, key:714}, 715: {is_public:false, clauses:{0:{code:[28,1,17,0,0,17,0,1,5], key:0}, 1:{code:[30,0,1,60,0,60,1,20,0,23,1,4,25,0,0,20,1,23,1,5,25,0,1,8,1,4,0,8,1,5,1,3,230,2,9,0,0,9,1,1,2,4,715], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:715, index: 2}, 716: {is_public:false, clauses:{0:{code:[28,1,17,0,0,17,0,1,5], key:0}, 1:{code:[30,0,1,60,0,60,1,20,0,23,1,4,25,0,0,20,1,23,1,5,25,0,1,19,195,5,24,1,4,23,1,6,8,1,4,0,8,1,6,1,3,120,2,9,0,0,9,1,1,2,4,716], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:716, index: 2}, 717: {is_public:false, clauses:{0:{code:[28,1,17,0,0,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,20,0,23,1,3,25,0,0,19,195,3,25,0,2,23,1,4,8,1,4,0,8,0,3,1,3,385,4,8,0,2,0,3,377,4,9,3,0,3,711,4,8,0,2,0,3,378,3,12,692,1,25,0,2,12,208,0,26,445,24,1,1,3,295,3,12,692,1,25,0,2,12,208,0,26,445,24,1,1,3,209,3,9,2,0,8,0,1,1,3,492,3,9,1,0,7,5,1,7,6,2,7,7,3,7,8,4,40,128,5,2,9,0,0,2,4,717,28,2147483844,1,60,0,60,1,60,2,60,3,60,4,33,4,16,0,3,0,15,1,8,1,16,0,2,2,16,0,0,3,16,0,1,4,8,0,3,0,8,1,8,1,3,105,5,32,4,8,0,2,0,3,218,4,9,1,0,9,2,1,9,3,2,2,41,220,3,30,0,15,1,5,0,15,1,6,1,15,1,7,2,15,1,8,3,15,1,9,4,4,62,28,2147483914,1,60,0,60,1,60,2,33,2,15,1,6,0,16,0,0,1,16,0,1,2,8,0,0,0,8,0,1,1,40,282,2,3,32,2,9,0,0,9,1,1,2,4,106,30,0,15,1,3,0,15,1,4,1,15,1,5,2,4,62,28,2147483961,1,60,0,33,0,15,1,5,0,15,1,6,1,8,1,5,0,8,1,6,1,3,107,1,32,0,2,4,61,30,0,15,1,2,0,15,1,3,1,4,62], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:717, index: 2}, 718: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,17,0,0,10,1224,0,3,64,1,32,0,2,5], key:0}, 1:{code:[30,0,15,1,2,0,8,1,2,0,4,719], key:1}}, clause_keys:[0, 1], next_key:2, key:718}, 719: {is_public:false, clauses:{0:{code:[28,1,17,0,0,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,20,0,23,1,2,25,0,0,19,625,2,23,1,3,25,0,1,8,1,3,0,3,287,2,10,21,0,3,287,2,9,1,0,12,358,2,27,5,12,284,4,26,128,12,285,6,26,530,12,286,8,26,128,13,7,24,1,8,26,0,13,5,24,1,6,24,1,7,13,3,24,1,4,24,1,5,13,1,24,1,2,24,1,3,3,283,2,10,679,0,3,64,1,9,0,0,2,4,719], key:1}, 2:{code:[254,0,44,0,2147483657,1000000,1000000,1,1000000,45,0,1,0,0], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:719, index: 2}, 720: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,15,1,4,0,8,1,4,0,8,0,4,1,8,0,3,2,3,66,5,8,0,2,0,3,153,5,9,4,0,3,222,5,3,337,4,8,0,1,0,3,153,4,3,94,4,9,3,0,3,718,4,8,0,0,0,12,195,3,25,0,1,25,0,2,12,479,2,27,1000,24,1,3,12,722,1,24,1,2,27,1000,3,34,3,13,3,26,1233,26,0,13,2,25,0,0,24,1,3,13,0,26,1234,24,1,2,10,679,1,3,326,1,10,679,0,2,4,64], key:0}}, clause_keys:[0], next_key:1, key:720}, 721: {is_public:false, clauses:{0:{code:[254,0,1,15,1,2,0,8,1,2,0,3,720,0,2,4,62], key:0}}, clause_keys:[0], next_key:1, key:721}, 723: {is_public:false, clauses:{0:{code:[254,0,1,60,0,15,1,5,0,15,1,6,1,8,1,5,0,8,0,0,1,8,1,6,2,3,66,1,9,0,0,2,4,222], key:0}}, clause_keys:[0], next_key:1, key:723}};
indexed_predicates = {189: true, 190: true, 191: true, 192: true, 193: true, 194: true, 203: true, 188: true, 199: true, 200: true, 201: true, 202: true, 205: true, 207: true, 209: true, 210: true, 211: true, 214: true, 215: true, 219: true, 220: true, 222: true, 223: true, 228: true, 232: true, 233: true, 234: true, 235: true, 240: true, 241: true, 242: true, 243: true, 245: true, 246: true, 247: true, 248: true, 249: true, 250: true, 252: true, 253: true, 254: true, 255: true, 256: true, 257: true, 258: true, 259: true, 260: true, 261: true, 262: true, 264: true, 265: true, 266: true, 267: true, 269: true, 271: true, 272: true, 273: true, 274: true, 275: true, 276: true, 277: true, 278: true, 279: true, 280: true, 281: true, 282: true, 283: true, 287: true, 288: true, 289: true, 290: true, 291: true, 292: true, 293: true, 294: true, 295: true, 296: true, 297: true, 298: true, 299: true, 300: true, 301: true, 302: true, 303: true, 304: true, 305: true, 306: true, 307: true, 308: true, 309: true, 310: true, 311: true, 312: true, 313: true, 314: true, 315: true, 316: true, 318: true, 319: true, 320: true, 324: true, 325: true, 326: true, 327: true, 328: true, 329: true, 330: true, 332: true, 339: true, 340: true, 341: true, 342: true, 345: true, 346: true, 347: true, 348: true, 349: true, 350: true, 351: true, 352: true, 353: true, 354: true, 355: true, 356: true, 357: true, 359: true, 360: true, 361: true, 362: true, 364: true, 366: true, 368: true, 369: true, 371: true, 372: true, 373: true, 375: true, 377: true, 378: true, 379: true, 380: true, 381: true, 382: true, 383: true, 384: true, 385: true, 386: true, 388: true, 389: true, 391: true, 398: true, 400: true, 402: true, 418: true, 421: true, 426: true, 429: true, 430: true, 435: true, 437: true, 438: true, 439: true, 440: true, 441: true, 443: true, 444: true, 446: true, 447: true, 251: true, 487: true, 488: true, 489: true, 490: true, 491: true, 492: true, 493: true, 494: true, 495: true, 496: true, 498: true, 500: true, 501: true, 502: true, 503: true, 504: true, 506: true, 507: true, 508: true, 509: true, 510: true, 511: true, 512: true, 513: true, 514: true, 515: true, 516: true, 517: true, 518: true, 519: true, 520: true, 521: true, 522: true, 523: true, 524: true, 525: true, 526: true, 527: true, 528: true, 529: true, 530: true, 531: true, 532: true, 533: true, 534: true, 204: true, 206: true, 213: true, 216: true, 218: true, 221: true, 225: true, 226: true, 227: true, 230: true, 263: true, 538: true, 539: true, 540: true, 543: true, 544: true, 545: true, 547: true, 548: true, 550: true, 551: true, 553: true, 555: true, 557: true, 566: true, 567: true, 568: true, 569: true, 570: true, 571: true, 572: true, 573: true, 574: true, 575: true, 577: true, 578: true, 580: true, 581: true, 582: true, 584: true, 585: true, 586: true, 587: true, 588: true, 589: true, 590: true, 591: true, 594: true, 595: true, 596: true, 598: true, 599: true, 601: true, 602: true, 603: true, 604: true, 605: true, 607: true, 616: true, 628: true, 634: true, 635: true, 636: true, 637: true, 638: true, 639: true, 640: true, 643: true, 644: true, 646: true, 647: true, 648: true, 649: true, 650: true, 651: true, 652: true, 653: true, 655: true, 656: true, 657: true, 658: true, 659: true, 660: true, 661: true, 662: true, 663: true, 664: true, 665: true, 666: true, 667: true, 668: true, 669: true, 672: true, 674: true, 675: true, 676: true, 677: true, 679: true, 680: true, 681: true, 682: true, 684: true, 685: true, 686: true, 687: true, 688: true, 689: true, 691: true, 693: true, 694: true, 695: true, 698: true, 699: true, 700: true, 701: true, 706: true, 707: true, 708: true, 709: true, 710: true, 711: true, 712: true, 714: true, 715: true, 716: true, 717: true, 718: true, 719: true, 720: true, 721: true, 723: true};
foreign_predicates = {0: predicate_acyclic_term, 1: predicate_subsumes_term, 2: predicate_compare, 3: predicate_var, 4: predicate_atom, 5: predicate_integer, 6: predicate_float, 7: predicate_compound, 8: predicate_ground, 9: predicate_unify, 10: predicate_match, 11: predicate_functor, 12: predicate_arg, 13: predicate_univ, 14: predicate_copy_term, 15: predicate_halt, 16: predicate_current_prolog_flag, 17: predicate_set_prolog_flag, 18: predicate_repeat, 19: predicate_atom_length, 20: predicate_atom_concat, 21: predicate_sub_atom, 22: predicate_char_code, 23: predicate_atom_chars, 24: predicate_atom_codes, 25: predicate_number_chars, 26: predicate_number_codes, 27: predicate_char_conversion, 28: predicate_current_char_conversion, 29: predicate_current_predicate, 30: predicate_term_gt, 31: predicate_term_egt, 32: predicate_term_lt, 33: predicate_term_elt, 34: predicate_is, 35: predicate_gt, 36: predicate_lt, 37: predicate_elt, 38: predicate_egt, 39: predicate_eq, 40: predicate_ne, 41: predicate_set_input, 42: predicate_set_output, 43: predicate_current_output, 44: predicate_current_input, 45: predicate_get_char, 46: predicate_get_code, 47: predicate_peek_char, 48: predicate_peek_code, 49: predicate_put_char, 50: predicate_put_code, 51: predicate_get_byte, 52: predicate_peek_byte, 53: predicate_put_byte, 54: predicate_flush_output, 55: predicate_at_end_of_stream, 56: predicate_set_stream_position, 57: predicate_stream_property, 58: predicate_current_stream, 59: predicate_write_term, 60: predicate_current_op, 61: predicate_fail, 62: predicate_true, 63: predicate_term_variables, 64: writeln, 65: predicate_gensym, 66: atom_to_term, 67: predicate_clause, 68: predicate_abolish, 69: predicate_retract_clause, 70: read_term, 71: predicate_open, 72: predicate_close, 73: predicate_op, 74: atom_to_memory_file, 75: memory_file_to_atom, 76: new_memory_file, 77: open_memory_file, 78: free_memory_file, 79: predicate_format, 80: predicate_flag, 81: predicate_memory_file_description, 82: predicate_copy_memory_file_to_local_storage, 83: predicate_copy_local_storage_to_memory_file, 84: predicate_absolute_file_name, 85: predicate_numbervars, 86: reset_compile_buffer, 87: emit_code, 88: predicate_lookup_atom, 89: predicate_lookup_float, 90: predicate_lookup_functor, 91: add_clause_to_predicate, 92: add_clause_to_aux, 93: prepend_clause_to_predicate, 94: predicate_flush_stdout, 95: predicate_debug, 96: predicate_nodebug, 97: predicate_jmp, 98: predicate_generate_initialization_goal, 99: predicate_generate_system_goal, 100: predicate_define_dynamic_predicate, 101: predicate_compiled_state_boot_code, 102: predicate_dump_tables, 103: predicate_compile_buffer_codes, 104: predicate_add_module_export, 105: predicate_module_export, 106: predicate_add_module_import, 107: predicate_module_import, 108: predicate_add_meta_predicate, 109: predicate_pls_meta_predicate, 110: predicate_indexing_mode, 111: predicate_set_indexing_mode, 112: predicate_indexable_compiled_predicates, 113: predicate_register_indexed_predicate, 114: predicate_compiled_clauses, 115: predicate_add_index_clause_to_predicate, 116: predicate_edit_clauses_for_index_sequences, 117: predicate_decode_instruction, 118: predicate_request_result, 119: predicate_handle_result, 120: predicate_fetch_promise, 121: predicate_trace_unify, 122: predicate_trace_set, 123: predicate_trace_value, 124: predicate_trace_set_info, 125: predicate_suspend_set, 126: predicate_get_terminal_char, 127: predicate_trace_set_retry, 128: predicate_trace_retry_value, 129: predicate_trace_set_prompt, 130: predicate_get_backtrack_frame, 131: predicate_set_backtrack_frame, 132: predicate_trace_instruction_set, 133: predicate_clear_local_storage_log, 134: predicate_member_test, 135: mark_top_choicepoint, 136: unmark_choicepoint, 137: unmark_top_choicepoint, 138: get_current_block, 139: install_new_block, 140: reset_block, 141: unwind_stack, 142: clean_up_block, 143: predicate_throw, 144: get_exception, 145: clear_exception, 146: recorda, 147: recordz, 148: recorded, 149: erase, 150: record_term, 151: predicate_gc, 152: predicate_statistics, 153: predicate_wam_duration, 154: predicate_statistics_heap_size, 155: predicate_statistics_max_stack, 156: predicate_statistics_max_heap, 157: predicate_eval_javascript, 158: predicate_eval_javascript, 159: predicate_remove_dom_element_class, 160: predicate_replace_dom_element_class, 161: predicate_toggle_dom_element_class, 162: predicate_set_dom_element_attribute_value, 163: predicate_dom_element_attribute_value, 164: predicate_create_dom_element, 165: predicate_create_dom_text_node, 166: predicate_append_dom_node_child, 167: predicate_insert_before_dom_node, 168: predicate_dom_select_element, 169: predicate_dom_select_all_elements, 170: predicate_dom_object_property, 171: predicate_dom_object_method, 172: predicate_dom_object_method, 173: predicate_dom_object_type, 174: predicate_dom_create_object, 175: predicate_dom_create_object, 176: predicate_dom_type_reference, 177: predicate_dom_release_object, 178: predicate_set_dom_object_property, 179: predicate_set_dom_object_property, 180: predicate_alert, 181: predicate_dom_window, 182: predicate_dom_type_property, 183: predicate_dom_type_method, 184: predicate_dom_type_parent};
system = [];
initialization = [];
module_exports = {2 : [[3,1],[5,2],[7,3],[9,1],[11,1],[13,1],[15,1],[17,1],[19,1],[21,2],[23,2],[25,3],[27,3],[29,2],[31,2],[33,0],[33,1],[35,2],[37,2],[39,0],[41,2],[43,3],[45,5],[47,2],[49,2],[51,2],[53,2],[55,2],[57,2],[59,2],[61,1],[64,2],[66,2],[68,2],[70,2],[72,2],[74,2],[76,2],[78,2],[80,2],[82,2],[84,2],[86,1],[88,1],[90,1],[92,1],[94,1],[94,2],[96,1],[96,2],[98,1],[98,2],[100,1],[100,2],[102,1],[102,2],[104,1],[104,2],[106,1],[106,2],[108,1],[108,2],[110,1],[110,2],[112,0],[112,1],[114,0],[114,1],[116,2],[118,2],[120,1],[122,2],[122,3],[124,3],[126,0],[128,0],[130,2],[132,1],[134,2],[136,3],[138,2],[140,1],[142,1],[144,2],[146,2],[146,3],[148,3],[148,4],[150,1],[150,2],[152,3],[154,2],[156,2],[158,1],[160,3],[162,1],[164,2],[164,3],[166,3],[168,2],[170,2],[172,2],[174,3],[176,3],[178,0],[180,2],[182,2],[184,2],[186,3],[188,3],[190,4],[192,3],[194,0],[196,0],[198,0],[200,1],[202,2],[204,2],[206,1],[208,1],[210,1],[212,1],[214,2],[216,2],[218,2],[220,2],[222,3],[224,3],[226,1],[228,1],[230,1],[232,1],[234,2],[236,1],[238,2],[240,4],[242,1],[244,2],[246,2],[248,2],[250,1],[252,1],[254,1],[256,1],[258,1],[260,1],[262,1],[264,1],[266,1],[268,1],[270,1],[272,0],[274,2],[276,2],[278,1],[280,0],[282,1],[284,1],[286,1],[288,0],[290,1],[292,1],[294,1],[296,0],[298,3],[300,3],[302,3],[304,1],[306,2],[308,0],[310,0],[312,1],[314,1],[316,1],[318,1],[320,1],[320,2],[322,2],[324,3],[326,3],[328,3],[330,3],[332,2],[334,2],[336,2],[338,3],[340,2],[342,2],[344,4],[346,2],[346,3],[348,2],[350,2],[350,3],[352,4],[354,1],[356,3],[356,4],[358,1],[360,1],[362,4],[364,5],[366,2],[371,3],[372,1],[373,2],[373,3],[374,1],[375,1],[376,1],[377,1],[378,1],[379,2],[380,1],[381,1],[382,1],[383,0],[384,2],[385,3],[386,3],[387,3],[388,1],[389,1],[390,1],[391,2],[392,2],[393,2],[394,1],[395,1],[396,1],[397,2],[398,1],[398,2],[399,1],[399,2],[400,1],[400,2],[401,1],[401,2],[402,1],[403,1],[404,2],[405,2],[406,2],[407,3],[408,2],[409,1],[409,2],[409,3],[409,4],[409,5],[409,6],[409,7],[409,8],[410,2],[411,0],[412,1],[413,4],[413,5],[568,0],[569,0],[570,0],[571,3],[573,2],[574,2],[574,3],[575,2],[576,1],[577,3],[682,1],[683,1],[684,1],[685,2],[686,2],[687,3],[704,2]], 370 : [[33,0],[94,1],[96,1],[98,1],[100,1],[102,1],[104,1],[106,1],[108,1],[110,1],[112,0],[114,0],[122,2],[140,1],[146,2],[148,3],[150,1],[164,2],[371,3],[372,1],[373,2],[373,3],[374,1],[375,1],[376,1],[377,1],[378,1],[379,2],[380,1],[381,1],[382,1],[383,0],[384,2],[385,3],[386,3],[387,3],[388,1],[389,1],[390,1],[391,2],[392,2],[393,2],[394,1],[395,1],[396,1],[397,2],[398,1],[398,2],[399,1],[399,2],[400,1],[400,2],[401,1],[401,2],[402,1],[403,1],[404,2],[405,2],[406,2],[407,3],[408,2],[409,1],[409,2],[409,3],[409,4],[409,5],[409,6],[409,7],[409,8],[410,2],[411,0]], 412 : [[412,1],[413,4],[413,5]], 445 : [[706,1],[707,1],[708,1],[709,1],[709,2],[710,1],[711,1],[712,1],[713,2]], 500 : [[831,4],[832,2],[833,2],[834,2],[835,1],[836,3]], 567 : [[568,0],[569,0],[570,0],[571,3]], 572 : [[573,2],[574,2],[574,3],[575,2],[576,1],[577,3]], 681 : [[682,1],[683,1],[684,1],[685,2],[686,2],[687,3]], 703 : [[704,2]], 717 : [[718,2]], 849 : [[849,0]]};
module_imports = {2 : [370,412,567,572,681,703], 370 : [2,412], 412 : [2], 445 : [2,500,717,849], 500 : [2], 567 : [2,370,572], 572 : [2], 681 : [2,370], 703 : [2,370], 717 : [2], 849 : [2,717]};
meta_predicate_signatures = {137 : {2 : [139,140]}, 345 : {2 : [140,62],3 : [140,62,140]}, 349 : {2 : [62,140],3 : [62,140,140]}, 427 : {3 : [140,425,140]}, 428 : {3 : [140,425,140]}, 431 : {1 : [139],2 : [432,140],3 : [433,140,140],4 : [434,140,140,140],5 : [435,140,140,140,140],6 : [436,140,140,140,140,140],7 : [437,140,140,140,140,140,140],8 : [438,140,140,140,140,140,140,140]}, 439 : {1 : [62]}, 440 : {1 : [62]}, 441 : {1 : [62]}, 442 : {1 : [62]}, 443 : {3 : [140,425,140]}, 444 : {1 : [62]}, 60 : {1 : [62]}, 715 : {1 : [139]}, 716 : {1 : [139]}};
}
