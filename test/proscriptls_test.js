// File foreign.js
var compile_buffer = [];

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
            if (TAG(deref(memory[VAL(list_term)])) !== TAG_ATM)
                return type_error("atom", deref(memory[VAL(list_term)]));
            var ftor_name = atable[VAL(deref(memory[VAL(list_term)]))];
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
            memory[tmp] = lookup_functor(ftor_name, arity);
            return unify(term, tmp ^ (TAG_STR << WORD_BITS));
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
    // Not checking. Lets assume we know what we are doing!
    compile_buffer[VAL(address)] = VAL(c);
    return true;
}

function predicate_lookup_atom(atom, index)
{
    return unify(VAL(atom) ^ (TAG_INT << WORD_BITS), index);
}

function predicate_lookup_float(f, index)
{
    return unify(VAL(f) ^ (TAG_INT << WORD_BITS), index);
}

function predicate_lookup_functor(fname, arity, index)
{
    if (atable[VAL(fname)] === undefined)
        abort("Atom out of range: " + hex(deref(fname)));
    var i;
    for (i = 0; i < ftable.length; i++)
    {
        if (ftable[i][0] === VAL(fname) && ftable[i][1] === VAL(arity))
        {
            return unify(index, i ^ (TAG_INT << WORD_BITS));            
        }
    }
    i = ftable.length;
    ftable[i] = [VAL(fname), VAL(arity)];
    return unify(index, i ^ (TAG_INT << WORD_BITS));
}

function predicate_generate_system_goal(Sys) {
    if (TAG(Sys) !== TAG_REF) {
        return instantiation_error(Sys);
    }
    let n = stable.length;
    let functor = lookup_functor('$sys_' + n, 0);
    stable.push(functor);
    let nameID = ftable[VAL(functor)][0];
    let namePL = PL_put_atom(nameID);
    return unify(namePL, Sys);
}

function predicate_generate_initialization_goal(Init) {
    if (TAG(Init) !== TAG_REF) {
        return instantiation_error(Init);
    }
    let n = itable.length;
    let functor = lookup_functor('$init_' + n, 0);
    itable.push(functor);
    let nameID = ftable[VAL(functor)][0];
    let namePL = PL_put_atom(nameID);
    return unify(namePL, Init);
 }

// dynamic implies public. In proscript, public also implies dynamic.
// the is_public flag will be set to true in the saved state
// for predicate Name/Arity.


function predicate_define_dynamic_predicate(indicator) {
    var slash2 = lookup_functor("/", 2);
    if (TAG(indicator) === TAG_STR && memory[VAL(indicator)] === slash2)
    {
        var name = deref(memory[VAL(indicator) + 1]);
        var arity = deref(memory[VAL(indicator) + 2]);
        if (TAG(name) === TAG_ATM && TAG(arity) === TAG_INT)
        {
            if (VAL(arity) < 0)
                return domain_error("not_less_than_zero", arity);
            var ftor = VAL(lookup_dynamic_functor(atable[VAL(name)], VAL(arity)));
            if (! predicates[ftor]){
                predicates[ftor] = {clauses: {},
                    key:ftor,
                    clause_keys: [],
                    is_public: true,
                    next_key: 0};
            } else if (! predicates[ftor].is_public)
                return permission_error("modify", "static_procedure", indicator);
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

// The predicateIndicator is a structure of Name/Arity, where Name
// is the unqualified name of a predicate (i.e. does not have a module
// name prefix).

function predicate_add_module_export(moduleName, predicateIndicator) {
  //     assertz('$module_export'(Name, F, A)).
    if (TAG(moduleName) === TAG_REF)
        return instantiation_error(moduleName);
    else if (TAG(moduleName) !== TAG_ATM)
        return type_error("atom", moduleName);

    var slash2 = lookup_functor("/", 2);
    if (TAG(predicateIndicator) === TAG_STR && memory[VAL(predicateIndicator)] === slash2)
    {
        var name = deref(memory[VAL(predicateIndicator) + 1]);
        var arity = deref(memory[VAL(predicateIndicator) + 2]);
        if (TAG(name) === TAG_ATM && TAG(arity) === TAG_INT)
        {
            if (VAL(arity) < 0)
                return domain_error("not_less_than_zero", arity);
            let namePL = PL_put_atom_chars(name);

            let exportedPredicates = module_exports[moduleName];
            if(exportedPredicates) {
                for (let entry of exportedPredicates) {
                    if (entry[0] === namePL && entry[1] === arity) {
                        return true; // do nothing, the predicate is already specified in the exports.
                    }
                }
            } else {
                exportedPredicates = [];
                module_exports[moduleName] = exportedPredicates;
            }

            // add namePL/arity to exports.
            exportedPredicates.push([namePL, arity]);
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
    else if (TAG(predicateIndicator) === TAG_REF)
        return instantiation_error(predicateIndicator);
    else
        return type_error("predicate_indicator", predicateIndicator);
}


function predicate_method_export(moduleName, predicateIndicator)
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
        } else if( unify(moduleName, cursor.module)) {
            let pair = moduleExports[cursor.exportIndex];
            if(!pair) {
                return false;
            } else {
                let indicatorPL = create_indicator_structure(pair[0], pair[1]);
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
        else
        {
            create_choicepoint();
            cursor = {module:moduleName, exportIndex:0};
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
        } else if( unify(moduleName, cursor.module)) {
            if(!pair) {
                return false;
            } else {
                let indicatorPL = create_indicator_structure(pair[0], pair[1]);
                return unify(predicateIndicator, indicatorPL);
            }
        } else {
            return false;
        }
    }
    else
        return type_error("atom", key);
}

function create_indicator_structure(name, arity) {
    var ftor = lookup_functor('/', 2);
    var structure = alloc_structure(ftor);
    memory[state.H++] = name;
    memory[state.H++] = arity;
    return structure;
}


function predicate_dump_tables(streamPL) {
    /*
            format(S, 'atable = [~w];~n', [AtomAtom]),
        format(S, 'floats = [~w];~n', [FloatAtom]),
       format(S, 'ftable = [~w];~n', [FunctorAtom]),
        format(S, 'dtable = [~w];~n', [DynamicFunctorAtom]),
         format(S, 'predicates = {~w};~n', [PredicatesAtom]),
        format(S, 'foreign_predicates = {~w};~n', [FPredicatesAtom]),
        format(S, 'system = [~w];~n', [SystemAtom]),
        format(S, 'initialization = [~w];~n', [InitializationAtom]).
        format(S, 'exports = [~w];~n', [ModuleExportsAtom]).
     */
    let streamContainer = {};
    if (!get_stream(streamPL, streamContainer))
        return false;
    let streamValue = streamContainer.value;
    write_to_stream(streamValue, 'atable =' + JSON.stringify(atable) + ';\n');
    write_to_stream(streamValue, 'floats =' + JSON.stringify(floats) + ';\n');
    write_to_stream(streamValue, 'ftable =' + JSON.stringify(ftable) + ';\n');
    write_to_stream(streamValue, 'dtable =' + JSON.stringify(dtable) + ';\n');
    write_to_stream(streamValue, 'predicates =' + JSON.stringify(predicates) + ';\n');
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
        compile_buffer[0] = 254;
        compile_buffer[1] = 0;
        predicates[predicateJS] = {clauses: {0:{code:compile_buffer.slice(0),
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
            compile_buffer[0] = 28;
            compile_buffer[1] = first_key;
            first_clause.code[0] = 30;
            first_clause.code[1] = 0;
            predicates[predicateJS].clauses[predicates[predicateJS].next_key] = {code:compile_buffer.slice(0),
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
            compile_buffer[0] = 28;
            compile_buffer[1] = first_key;
            first_clause.code[0] = 29;
            predicates[predicateJS].clauses[predicates[predicateJS].next_key] = {code:compile_buffer.slice(0),
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
    for (var z = 0; z < compile_buffer.length; z++)
    {
        if (compile_buffer[z] === null)
        {
            debug(term_to_string(head) + ":- " + term_to_string(body));
            debug(JSON.stringify(compile_buffer));
            abort("Illegal compile buffer: Address " + z + " is null!");
        }
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
        compile_buffer[0] = 254;
        compile_buffer[1] = 0;
        check_compile_buffer(head, body);
        predicates[predicate] = {clauses: {0:{code:compile_buffer.slice(0), 
                                              key:0, 
                                              head:record_term(head), 
                                              body:record_term(body)}},
                                 key:predicate,
                                 clause_keys: [0],
                                 is_public: true,
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
            compile_buffer[0] = 30;
            compile_buffer[1] = 0;
            check_compile_buffer(head, body);            
            predicates[predicate].clauses[predicates[predicate].next_key] = {code:compile_buffer.slice(0),
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
            compile_buffer[0] = 30;
            compile_buffer[1] = 0;
            //compile_buffer.unshift(predicates[predicate].next_key); WHAT?
            check_compile_buffer(head, body);            
            predicates[predicate].clauses[predicates[predicate].next_key] = {code:compile_buffer.slice(0),
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
        compile_buffer[VAL(n)] = 254;
        compile_buffer[VAL(n)+1] = 0;
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
        switch(compile_buffer[address])
        {
        case 254:
            // Change <NOP,0> -> try_me_else offset
            compile_buffer[address] = 28;
            compile_buffer[address+1] = offset;
            // Add <trust_me,0> for new clause
            compile_buffer[offset ^ 0x80000000] = 30;
            compile_buffer[(offset ^ 0x80000000)+1] = 0;
            return;
        case 30:
            // Change <trust_me,0> -> <retry_me_else, N>
            compile_buffer[address] = 29;
            compile_buffer[address+1] = offset;
            // Add <trust_me,0> for new clause
            compile_buffer[offset ^ 0x80000000] = 30;
            compile_buffer[(offset ^ 0x80000000)+1] = 0;
            return;
        case 28:
        case 29:
            address = compile_buffer[address+1] ^ 0x80000000;
            break;
        default:
            abort("Garbage in code array: " + compile_buffer[address]);
        }        
    }
}


function create_choicepoint()
{
    // Create a choicepoint
    var newB;
    if (state.E > state.B) {
        newB = state.E + state.CP.code[state.CP.offset - 1] + 2;
    } 
    else
    {
        newB = state.B + memory[state.B] + 8;
    }
    memory[newB] = state.num_of_args+2;
    var n = memory[newB];
    memory[newB + 1] = 0;
    memory[newB + 2] = {code: code,
                        offset: state.P};
    for (var i = 0; i < state.num_of_args; i++)
    {
        memory[newB + 3 + i] = register[i];
    }
    // Save the current context
    memory[newB+n+1] = state.E;
    memory[newB+n+2] = state.CP;
    memory[newB+n+3] = state.B;
//    memory[newB+n+4] = retry_foreign;
    memory[newB+n+4] = {code: bootstrap_code,                        
                        predicate:state.current_predicate,  // Suspect
                        offset:retry_foreign_offset};
    memory[newB+n+5] = state.TR;
    memory[newB+n+6] = state.H;
    memory[newB+n+7] = state.B0;
    state.B = newB;
    state.HB = state.H;
    return true;
}

function update_choicepoint_data(value)
{
    memory[state.B+1] = value;
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
    var n = memory[state.B];
    unwind_trail(memory[state.B + n + 5], state.TR);
    state.B = memory[state.B + n + 3];
    state.HB = memory[state.B+ memory[state.B] + 6];
}

// For testing only! Assumes -,+ mode
function member(element, list)
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
        var head = memory[VAL(list)];
        if (unify(head, element))
        {
            update_choicepoint_data(memory[VAL(list)+1]);
            return true;
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
    code = compile_buffer.slice(0);
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
    return unify(t2, recall_term(record_term(t1), {}));
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
        newB = state.B + memory[state.B] + 8;
    }
    memory[newB] = state.num_of_args+2;
    var n = memory[newB];
    memory[newB + 1] = 0;
    memory[newB + 2] = {code: code,
                        offset: state.P};
    for (var i = 0; i < state.num_of_args; i++)
    {
        memory[newB + 3 + i] = register[i];
    }
    // Save the current context
    memory[newB+n+1] = state.E;
    memory[newB+n+2] = state.CP;
    memory[newB+n+3] = state.B;
    memory[newB+n+4] = {code: code,
                        predicate: state.current_predicate, // suspect!
                        offset: state.P}; // Retry will just create the choicepoint again!
    memory[newB+n+5] = state.TR;
    memory[newB+n+6] = state.H;
    memory[newB+n+7] = state.B0;
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
    return unify(value, (268435455) ^ (TAG_INT<<WORD_BITS));
}

function flag_min_integer(set, value)
{
    if (set) return permission_error("prolog_flag");
    return unify(value, (536870911) ^ (TAG_INT<<WORD_BITS));
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
        let index = 0;
        for (let i = 0; i < prolog_flags.length; i++)
        {
            if (prolog_flags[index].name === keyname)
                return prolog_flags[index].fn(false, value);
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
    var slash2 = lookup_functor("/", 2);
    var index;
    if (!state.foreign_retry)
    {
        if (TAG(indicator) === TAG_STR)
        {
            if (memory[VAL(indicator)] === slash2)
            {
                var name = memory[VAL(indicator) + 1];
                var arity = memory[VAL(indicator) + 2];
                if (TAG(arity) !== TAG_INT && TAG(arity) !== TAG_REF)
                    return type_error("integer", arity);
                if (TAG(name) !== TAG_ATM && TAG(name) !== TAG_REF)
                    return type_error("atom", name);
                
                if (TAG(name) === TAG_ATM && TAG(arity) === TAG_INT)
                {
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
    else
        index = state.foreign_value + 1;
    // Horrific :(

    if (index >= Object.keys(predicates).length)
    {
        destroy_choicepoint();
        return false;
    }
    update_choicepoint_data(index);
    var key = Object.keys(predicates)[index];
    var result = state.H ^ (TAG_STR << WORD_BITS);
    memory[state.H++] = slash2;
    memory[state.H++] = ftable[key][0] ^ (TAG_ATM << WORD_BITS);
    memory[state.H++] = ftable[key][1] ^ (TAG_INT << WORD_BITS);
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

    var head_ref = recall_term(predicates[ftor].clauses[key].head, varmap);
    if (unify(head_ref, head))
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

/* errors */
function type_error(expected, got)
{
    var ftor = lookup_functor('type_error', 2);
    var ref = state.H ^ (TAG_STR << WORD_BITS);
    memory[state.H++] = ftor;
    memory[state.H++] = lookup_atom(expected);
    memory[state.H++] = got;
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
    var ftor = lookup_functor('existence_error', 2);
    var ref = state.H ^ (TAG_STR << WORD_BITS);
    memory[state.H++] = ftor;
    memory[state.H++] = lookup_atom(type);
    memory[state.H++] = instance;
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
// Promise object in javascript encapsulates asynchronous
// processing. The functions in this file support the integration
// of some Promise features with Proscript.
//
// The basic integration with Proscript relies on foreign predicates
// that create Promise objects, Prolog terms of the form
// '$promise'(N) to map to these Javascript Promise objects,
// and two foreign predicates to request results from a
// Promise object and to handle the event (callback) when
// the requested results are made available to the Javascript
// runtime environment; request_promise_results/1 and
// handle_promise_results/2, respectively.
//
// There is a utility Proscript predicate promise_result/2
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

    return(ref.type === 'promise');
}

function predicate_request_result(promise) {
    let promiseObject = {};
    if (!get_promise_object(promise, promiseObject)) {
        return representation_error('promise', promise);
    }
    let promiseJS = promiseObject.value;

    promise_requests.set(promise, '');
    // ignore promiseResultJS?
    let promiseResultJS = request_result(promise, promiseJS);
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
        if(backtrack()){
            if(! wam()) {
                throw 'promise_callback failed: callback ' + promise + ' result ' + result;
            }
        } else {
            throw 'promise_callback backtrack failed: promise ' + promise + ' result ' + result;
        }
    } else {
        // waiting on one or more requests.
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
    var head = memory[VAL(listPL)];
    var tail = memory[VAL(listPL)+1];
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
            head = memory[VAL(tail)];
            tail = memory[VAL(tail)+1];
        }
        else
            throw('Invalid atom list. Last item was not NIL.');
    }
}

function text_to_memory_file(text) {
    let index = memory_files.length;
    memory_files[index] = {data:toByteArray(text), ptr:0};
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
        var arg = memory[VAL(term) + 1];
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
    if (TAG(memfile) === TAG_REF)
        return instantiation_error(memfile);
    if (TAG(memfile) !== TAG_STR || memory[VAL(memfile)] !== lookup_functor("$memory_file", 1))
        return type_error("memory_file", memfile);
    var memindex = get_arg(memfile, 1);
    if (TAG(memindex) !== TAG_INT)
        return type_error("memory_file", memfile);
    memindex = VAL(memindex);
    if (TAG(mode) === TAG_REF)
        return instantiation_error(mode);
    else if (TAG(mode) !== TAG_ATM)
        return type_error("atom", mode);
    if (atable[VAL(mode)] === 'read')
    {
        streams[index] = new_stream(read_memory_file, null, null, close_memory_file, tell_memory_file, memindex);

    }
    else if (atable[VAL(mode)] === 'write')
    {
        streams[index] = new_stream(null, write_memory_file, null, close_memory_file, tell_memory_file, memindex);
    }
    else
        return type_error("io_mode", mode);
    var ftor = lookup_functor('$stream', 1);
    var ref = alloc_structure(ftor);
    memory[state.H++] = index ^ (TAG_INT << WORD_BITS);
    return unify(stream, ref);
}

function free_memory_file(memfile)
{
    var m = memory_files[VAL(get_arg(memfile, 1))];
    memory_files[m] = null;
    return true;
}
// File wam.js
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


let ftable = [];
let dtable = [];
let atable = ['[]']; // Reserve first atom as [].
let floats = [];
let predicates = {};
let exception = null;
let itable = [];
let stable = [];

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
    let trace_code = trace_predicate.clauses[trace_predicate.clause_keys[0]].code;

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
             suspended: false};
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

// noinspection JSUnusedGlobalSymbols
function explicit_deref(p)
{
    while(TAG(p) === TAG_REF && VAL(p) !== memory[VAL(p)])
    {
        let q = memory[VAL(p)];
        if (q === undefined)
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
    if(state.trace_call !== 'no_trace') {
        state.trace_call = memory[state.B + memory[state.B] + CP_TC];
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
    return tmp ^ (TAG_STR << WORD_BITS);
}

function alloc_var()
{
    let result = state.H ^ (TAG_REF << WORD_BITS);
    memory[state.H] = result;    
    state.H++;
    return result;
}

function alloc_list()
{
    let result = (state.H+1) ^ (TAG_LST << WORD_BITS);
    memory[state.H] = result;    
    state.H++;
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

        // Make the traceArgStructure the first argument.
        // The info term is the second argument. It is set
        // by '$trace' before evaluating call/.

        register[0] = traceArgStructure;
    }
    register[1] = state.trace_info;
    register[2] = PL_put_integer(state.trace_identifier);
    return traceArgArity;
}

function wam_complete_call_or_execute(predicate) {
   if (predicate.clauses && predicate.clause_keys && predicate.clause_keys.length > 0
           && predicate.clauses[predicate.clause_keys[0]]) {
        state.B0 = state.B;
        state.num_of_args = ftable[code[state.P + 1]][1];
        state.current_predicate = predicate;
        code = predicate.clauses[predicate.clause_keys[0]].code;
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
    state.num_of_args = ftable[code[state.P+1]][1];
    let args = new Array(state.num_of_args);
    for (let i = 0; i < state.num_of_args; i++)
        args[i] = deref(register[i]);
    let result = foreign_predicates[code[state.P+1]].apply(null, args);
    state.foreign_retry = false;
    return result;
}

function wam_trace_call_or_execute(functor) {
    return ! functor.startsWith('$trace') && functor !== 'true' && state.trace_predicate &&
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

function wam() {
    try {
        return wam1();
    } catch(e) {
        wamExit(e);
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
           //debugging = false;
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
                    tmpE = state.E + state.CP.code[state.CP.offset - 1] + 2;
                } else {
                    tmpE = state.B + memory[state.B] + CP_SIZE;
                }
                if (tmpE === undefined || isNaN(tmpE))
                    abort("Top of frame is garbage: " + tmpE);
                if (tmpE < HEAP_SIZE || tmpE > HEAP_SIZE + STACK_SIZE)
                    abort("Top of frame exceeds bounds in allocate: " + hex(tmpE));

                // Save old environment and continuation
                memory[tmpE] = state.E;
                memory[tmpE + 1] = state.CP;
                state.E = tmpE;
                state.P += 1;
            }
                continue;

            case 2: // deallocate
                state.CP = memory[state.E + E_CP];
                if (memory[state.E] < HEAP_SIZE || memory[state.E] > HEAP_SIZE + STACK_SIZE)
                    abort("Top of frame " + memory[state.E] + " exceeds bounds in deallocate. Environment is " + state.E + " P = " + state.P);

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
                code = state.CP.code;
                if (!code) {
                    throw 'code is undefined';
                }

                continue;

            case 6: // put_variable: Initialize a new variable in Yn, and also put it into Ai
            {
                let register_location = state.E + code[state.P + 1] + 2;
                memory[register_location] = register_location ^ (TAG_REF << WORD_BITS);
                register[code[state.P + 2]] = register_location ^ (TAG_REF << WORD_BITS);
                state.P += 3;
                // noinspection UnnecessaryContinueJS
                continue;
            }

            case 7: // put_variable: Put fresh var into registers Ai and Xn
            {

                let freshvar = state.H ^ (TAG_REF << WORD_BITS);
                memory[state.H] = freshvar;
                register[code[state.P + 1]] = freshvar;
                register[code[state.P + 2]] = freshvar;
                state.H++;
                state.P += 3;
            }
            continue;

        case 8: // put_value
            if (code[state.P+1] === 0) // Y-register
            {
                let register_location = state.E + code[state.P+2] + 2;
                if (memory[register_location] === undefined)
                    abort("Invalid memory access in put_value");
                register[code[state.P+3]] = memory[register_location];
            }
            else
            {
                register[code[state.P+3]] = register[code[state.P+2]];
            }
            state.P += 4;
            continue;

        case 9: // put_unsafe_value
        {
            let register_location = state.E + code[state.P + 1] + 2;
            // This is the unsafe bit. If the value now in register[code[state.P+2]] is on the stack (that is, it is > E) then we have to push a new variables
            // onto the stack to avoid dangling references to things that are about to be cleaned up
            if (memory[register_location] < state.E) {
                // No, so we can just behave like put_value
                register[code[state.P + 2]] = deref(memory[register_location])
            } else {
                // Yes, so we need to push a new variable instead
                let v = alloc_var();
                bind(v, memory[register_location]);
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
                let register_location = state.E + code[state.P+2] + 2;
                memory[register_location] = register[code[state.P+3]];
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
                let register_location = state.E + code[state.P + 2] + 2;
                source = memory[register_location];
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
                source = memory[state.S++]; 
            }
            else
            {
                source = alloc_var(); // If writing, create a new var
            }
            if (code[state.P+1] === 0) // Y-register
            {
                let register_location = state.E + code[state.P+2] + 2;
                // GC: This needs to be trailed if state.B is not 0, apparently
                bind(memory[register_location], source);
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
                source = memory[state.S++];
                if (code[state.P + 1] === 0) // Y-register
                {
                    let register_location = state.E + code[state.P + 2] + 2;
                    did_fail = !unify(memory[register_location], source);
                } else {
                    did_fail = !unify(register[code[state.P + 2]], source);
                }
            } else {
                if (code[state.P + 1] === 0) // Y-register
                {
                    let register_location = state.E + code[state.P + 2] + 2;
                    memory[state.H++] = memory[register_location];
                } else {
                    memory[state.H++] = register[code[state.P + 2]];
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
                source = memory[state.S++];
                if (code[state.P + 1] === 0) // Y-register
                {
                    let register_location = state.E + code[state.P + 2] + 2;
                    did_fail = !unify(memory[register_location], source);
                } else {
                    did_fail = !unify(register[code[state.P + 2]], source);
                }
            } else {
                let addr;
                if (code[state.P + 1] === 0) // Y-register;
                {
                    let register_location = state.E + code[state.P + 2] + 2;
                    addr = memory[register_location];
                } else {
                    addr = register[code[state.P + 2]];
                }
                addr = deref(addr);
                if (VAL(addr) < state.H) {
                    // Address is on the heap. Just push the value onto the top of the heap
                    memory[state.H++] = addr;
                } else {
                    // Address is on the stack. Push a new variable onto the heap and bind to the value
                    let fresh = state.H ^ (TAG_REF << WORD_BITS);
                    memory[state.H++] = fresh;
                    bind(fresh, addr);
                    if (code[state.P + 1] === 1)
                        register[code[state.P + 2]] = fresh; // also set X(i) if X-register
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
                let sym = deref(memory[state.S++]);
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
                memory[state.H++] = code[state.P+1] ^ (TAG_ATM << WORD_BITS);
                state.P += 2;
            }
            continue;
        case 27: // unify_integer
            if (state.mode === READ)
            {
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
                memory[state.H++] = (code[state.P+1] & ((1 << WORD_BITS)-1)) ^ (TAG_INT << WORD_BITS);
                state.P += 2;
            }
            continue;

        case 28: // try_me_else
        {
            // We need to allocate a new choicepoint, but first we need to determine /where/ to put it, since we do not keep a reference to the top of the stack.
            // The last item on the stack is either an environment, or a choicepoint.
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
                newB = state.E + state.CP.code[state.CP.offset - 1] + 2;
            } else {
                // In this case, the top frame is a choicepoint. This is a bit easier: A choicepoint contains 7 saved special-purpose registers, the N root arguments
                // for the goal, and, happily, the value of N as the very first argument. Therefore, we can read the 0th entry of the current frame (at state.B)
                // and add 9 to it to get the top of the stack.
                newB = state.B + memory[state.B] + CP_SIZE;
            }
            memory[newB] = state.num_of_args;
            let n = memory[newB];
            for (let i = 0; i < n; i++) {
                memory[newB + i + 1] = register[i];
            }
            // Save the current context
            memory[newB + n + CP_E] = state.E;
            memory[newB + n + CP_CP] = state.CP;
            memory[newB + n + CP_B] = state.B;
            let next = code[state.P + 1];
            if ((next & 0x80000000) === 0) {
                // next is a clause index in the current predicate
                memory[newB + n + CP_Next] = {
                    code: state.current_predicate.clauses[next].code,
                    predicate: state.current_predicate,
                    offset: 0
                };
            } else {

                // next is an absolute address in the current clause: Used for auxiliary clauses only
                memory[newB + n + CP_Next] = {
                    code: code,
                    predicate: state.current_predicate,
                    offset: next ^ 0x80000000
                };
            }
            //memory[newB+n+CP_Next] = {code: code, offset:code[state.P+1]};
            memory[newB + n + CP_TR] = state.TR;
            memory[newB + n + CP_H] = state.H;
            memory[newB + n + CP_B0] = state.B0;
            memory[newB + n + CP_TC] = state.trace_call;
            memory[newB + n + CP_TI] = state.trace_info;
            state.B = newB;
            state.HB = state.H;
            state.P += 2;
        }
            continue;
        case 29: // retry_me_else
        {
            // Unwind the last goal. The arity if the first thing on the stack, then the saved values for A1...An
            let arity = memory[state.B];
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
                state.trace_call = memory[state.B + arity + CP_TC];
                state.trace_info = memory[state.B + arity + CP_TI];
            }
            state.P += 2;
        }
            continue;
        case 30: // trust_me
        {
            // Unwind the last goal. The arity if the first thing on the stack, then the saved values for A1...An
            let n = memory[state.B];
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
                state.trace_call = memory[state.B + n + CP_TC];
                state.trace_info = memory[state.B + n + CP_TI];
            }
            state.B = memory[state.B + n + CP_B];
            state.HB = memory[state.B + memory[state.B] + CP_H];
            //state.HB = memory[state.B + n + CP_H];
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
                if (state.B > 0)
                    tidy_trail();
            }
            if (result)
                state.P += 1;
            else if (!backtrack())
                return wamExit(false);
        }
            continue;
        case 32: // cut(I)
        {
            let y = VAL(memory[state.E + 2 + code[state.P + 1]]);
            let result = true;
            if (state.B > y) {
                while (cleanups[0] !== undefined && cleanups[0].B > y && cleanups[0].B < state.B0) {
                    result = run_cleanup(cleanups[0]) && result;
                    cleanups.shift();
                }
                state.B = y;
                if (state.B > 0)
                    tidy_trail();
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
            memory[state.E + 2 + code[state.P+1]] = state.B0 ^ (TAG_INT << WORD_BITS);
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
            state.foreign_value = memory[state.B + 1];
            state.P = memory[state.B + 2].offset;
            code = memory[state.B + 2].code;
            if (!code) {
                throw 'code is undefined';
            }

            state.current_predicate = memory[state.B + 2].current_predicate;
            let n = memory[state.B];
            state.foreign_retry = true;
            for (let i = 0; i < n - 2; i++) {
                register[i] = memory[state.B + 3 + i];
            }
            state.E = memory[state.B + n + CP_E];
            state.CP = memory[state.B + n + CP_CP];
            unwind_trail(memory[state.B + n + CP_TR], state.TR);
            state.TR = memory[state.B + n + CP_TR];
            state.H = memory[state.B + n + CP_H];
            state.HB = state.H;
            if (state.trace_call !== 'no_trace') {
                state.trace_call = memory[state.B + n + CP_TC];
                state.trace_info = memory[state.B + n + CP_TI];
            }
        }
            continue;
        case 43: // get_choicepoint
        {
            let i = code[state.P + 1];
            let choice = state.B;
            while (i !== 0) {
                choice = memory[choice + memory[choice] + CP_B];
                i--;
            }

            memory[state.E + 2 + code[state.P + 2]] = (choice ^ TAG_INT << WORD_BITS);
            state.P += 3;
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
        case 51: // put_float I into Ai
            register[code[state.P+2]] = code[state.P+1] ^ (TAG_FLT << WORD_BITS);
            state.P += 3;
            continue;           
        case 52: // unify_float
            if (state.mode === READ)
            {
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
                memory[state.H++] = code[state.P+1] ^ (TAG_FLT << WORD_BITS);
                state.P += 2;
            }
            continue;

        case 60: // put_variable Yn
        {
            // Note that this is different from put_variable(Yn, Ai) in that it ONLY puts a fresh variable into Yn
            // This is needed to make garbage collection safe
            let register_location = state.E + code[state.P + 1] + 2;
            memory[register_location] = register_location ^ (TAG_REF << WORD_BITS);
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
            current_predicate: s.current_predicate,
            trace_call: s.trace_call,
            trace_predicate: s.trace_predicate,
            trace_code: s.trace_code};
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

// Exceptions are implement as per Bart Demoen's 1989 paper
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
    if (memory[state.B+memory[state.B]+CP_Next] === VAL(nb))
        state.B = VAL(memory[VAL(nb)+memory[VAL(nb)]+CP_Next]);
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

// End exceptions cod

function reset_compile_buffer()
{
    compile_buffer = [];
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

// File read.js
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
            else if (op.fixity === "yfx" && precedence >= op.precedence)
            {
                lhs = parse_infix(s, lhs, op.precedence);
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
        var head = memory[VAL(options)];
        if (TAG(head) !== TAG_STR)
            return type_error("option", head);
        var ftor = memory[VAL(head)];
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
            result.variables = memory[VAL(head)+1];
        }
        else if (ftor === lookup_functor("variable_names",1))
        {
            result.variable_names = memory[VAL(head)+1];
        }
        else if (ftor === lookup_functor("singletons",1))
        {
            result.singletons = memory[VAL(head)+1];
        }
        else
        {
            return type_error(options, head);
        }
        options =  memory[VAL(options)+1];
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
    var t1 = expression_to_term(expression, varmap, singletons);
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
            if (singletons[varname2] === 1)
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

function predicate_write_term(stream, term, options)
{
    if (!(options = parse_term_options(options)))
        return false;
    var value = format_term(term, options);
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
            if (is_punctuation(chars[j]) || chars[j] === ' ')
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


function format_term(value, options)
{
    var result;

    if (value === undefined)
        abort("Illegal memory access in format_term: " + hex(value) + ". Dumping...");
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
        if ((VAL(value) & (1 << (WORD_BITS-1))) === (1 << (WORD_BITS-1)))
            return (VAL(value) - (1 << WORD_BITS)) + "";
        else
            return VAL(value) + "";
        // fall-through
    case TAG_FLT:
        return floats[VAL(value)] + "";
    case TAG_STR:
        var ftor = VAL(memory[VAL(value)]);
        if (options.numbervars === true && ftor === lookup_functor('$VAR', 1) && TAG(memory[VAL(value)+1]) === TAG_INT)
        {
            var index = VAL(memory[VAL(value)+1]);
            result = String.fromCharCode(65 + (index % 26));
            if (index >= 26)
                result = result + Math.floor(index / 26);
            return result;
        }
        if (!is_operator(ftor) || options.ignore_ops === true)
        {
            // Print in canonical form functor(arg1, arg2, ...)
            result = format_term(ftable[ftor][0] ^ (TAG_ATM << WORD_BITS), options) + "(";
            for (var i = 0; i < ftable_arity(ftor); i++)
            {
                result += format_term(memory[VAL(value)+1+i], options);
                if (i+1 < ftable_arity(ftor))
                    result += ",";
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
                var lhs = format_term(memory[VAL(value)+1], options);
                if (is_punctuation_charAt(lhs, lhs.length-1) && !is_punctuation(fname.charAt(0)))
                    result = lhs + fname;
                else if (!is_punctuation_charAt(lhs, lhs.length-1) && is_punctuation(fname.charAt(0)))
                    result = lhs + fname;
                else
                {
                    result = lhs + " " + fname;
                }
                var rhs1 = format_term(memory[VAL(value)+2], options);

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
                var rhs2 = format_term(memory[VAL(value)+1], options);
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
            return "'.'(" + format_term(memory[VAL(value)], options) + "," + format_term(memory[VAL(value)+1], options) + ")";
        // Otherwise we need to print the list in list-form
        result = "[";
        var head = memory[VAL(value)];
        var tail = memory[VAL(value)+1];
        while (true)
        {
            result += format_term(head, options);
            if (tail === NIL)
                return result + "]";
            else if (TAG(tail) === TAG_LST)
            {
                head = memory[VAL(tail)];
                tail = memory[VAL(tail)+1];
                result += ",";
            }
            else 
                return result + "|" + format_term(tail, options) + "]";
        }        
    }
}

function is_punctuation_charAt(object, position) {
    return typeof object.chartAt === 'function' && is_punctuation(object.charAt(position));
}

function expression_to_term(s, varmap, singletons)
{
    if (typeof(s) === "string")
        return lookup_atom(s);
    else if (typeof(s) === "number")
    {
        if (s === ~~s)
        {
            return (s & ((1 << WORD_BITS)-1)) ^ (TAG_INT << WORD_BITS);
        }
        else
        {
            return lookup_float(s);
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
        return result;
    }
    else if (s.list !== undefined)
    {   
        // Special case for [], as usual, since we do not actually allocate any lists!
        if (s.list.length === 0)
            return NIL;

        var result = alloc_var();
        var tail = result;
        var head;
        for (var i = 0; i < s.list.length; i++)
        {
            unify(tail, state.H ^ (TAG_LST << WORD_BITS));
            head = alloc_var();
            tail = alloc_var();
            unify(head, expression_to_term(s.list[i], varmap, singletons));
        }
        unify(tail, expression_to_term(s.tail, varmap, singletons));
        return result;
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
            var z = expression_to_term(s.args[k], varmap, singletons);
            unify(z, var_args[k]);
        }
        return t;
    }
    else
        abort("Invalid expression: " + JSON.stringify(s));
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
    var t1 = expression_to_term(expression, b, {});
    var arglist = [];
    var keys = Object.keys(b);
    for (var i = 0 ; i < keys.length; i++)
        arglist.push({functor:"=", args:[keys[i], {variable_name:keys[i]}]});
    var t2 = expression_to_term({list:arglist, tail:{list: []}}, b, {});
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
                value: VAL(t)};
    case TAG_LST:
        var value = [];
        var list = {type: TAG_LST,
                    value: value};
        while (TAG(t) === TAG_LST)
        {
            value.push(record_term(VAL(t)));
            t = memory[VAL(t)+1];
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
        return e.value ^ (TAG_INT << WORD_BITS);
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
    if (TAG(term) === TAG_INT)
        return VAL(term);
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
    return integer ^ (TAG_INT << WORD_BITS);
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
        register[i] = memory[VAL(term) + 1 + i];
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
    return unify(c, (_get_code(s.value) & ((1 << (WORD_BITS-1))-1)) ^ (TAG_INT << WORD_BITS));
}

function predicate_get_byte(stream, c)
{
    var s = {};
    if (!get_stream(stream, s))
        return false;
    return unify(c, (getb(s.value) & ((1 << (WORD_BITS-1))-1)) ^ (TAG_INT << WORD_BITS));
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
    return unify(c, _peek_code(s.value) ^ (TAG_INT << WORD_BITS));
}

function predicate_peek_byte(stream, c)
{
    var s = {};
    if (!get_stream(stream, s))
        return false;
    return unify(c, (peekb(s.value) & ((1 << (WORD_BITS-1))-1)) ^ (TAG_INT << WORD_BITS));
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
function predicate_gc()
{
    debug("Before GC, heap is " + state.H);
    // WARNING: This assumes ONLY predicate_gc will mark things!
    total_marked = 0;

    // debugging only
/*
    var before = [];
    var e = state.E;
    var envsize = state.CP.code[state.CP.offset - 1];
    while (e != HEAP_SIZE)
    {
        for (var i = 0; i < envsize; i++)
        {
            before.push(record_term(memory[e+2 + i]));
        }
        var envcp = memory[e+1];
        envsize = envcp.code[envcp.offset-1];
        e = memory[e];
    }
*/
//    check_stacks(false);
    mark();
//    check_stacks(true);    
    push_registers();
    sweep_trail();
    sweep_stack(); 


    compact();
    pop_registers();
    state.H = total_marked;
    debug("After GC, heap is " + state.H);    

//    check_stacks(false);
/*
    var after = [];
    var e = state.E;
    var envsize = state.CP.code[state.CP.offset - 1];
    while (e != HEAP_SIZE)
    {
        for (var i = 0; i < envsize; i++)
        {
            after.push(record_term(memory[e+2 + i]));
        }
        var envcp = memory[e+1];
        envsize = envcp.code[envcp.offset-1];
        e = memory[e];
    }
*/
    if (total_marked !== 0)
    {
    }
/*
    while (before.length != 0)
    {
        var a = before.pop();
        var b = after.pop();
        at = recall_term(a, {});
        bt = recall_term(b, {});
        if (!predicate_unify(at, bt))
        {
            debug("Error: Terms in environment changed during GC!");
            debug("at = " + term_to_string(at));
            debug("bt = " + term_to_string(bt));
            abort("false");
        }
    }
*/

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
    // Upward
    for (current = state.H-1; current >= 0; current--)
    {
        if ((memory[current] & M_BIT) === M_BIT)
        {
            update_relocation_chain(current, dest);
            if (IS_HEAP_PTR(memory[current]))
            {
                if (VAL(memory[current]) < current)
                {
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
}

function update_relocation_chain(current, dest)
{
    var j;    
    while ((memory[current] & F_BIT) === F_BIT)
    {
        j = VAL(memory[current]);
        memory[current] = VAL(memory[j]) ^ (memory[current] & (NV_MASK ^ F_BIT)) | (memory[j] & F_BIT);
        memory[j] = dest ^ (memory[j] & NV_MASK);
        memory[j] &= ~F_BIT;                
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
    var aggregateDuration = statistics_wam_duration();
    var heapSize = statistics_heap_size();

    stdout("WAM duration: " + aggregateDuration + "\n");
    stdout("Heap size: " + heapSize + "\n");
    return true;
}

function predicate_wam_duration(duration) {
    var aggregateDuration = statistics_wam_duration();

    if(Number.isSafeInteger(aggregateDuration)) {
        return PL_unify_integer(duration, aggregateDuration);
    } else {
        return PL_unify_float(duration, aggregateDuration);
    }
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

    if(Number.isSafeInteger(heapSize)) {
        return PL_unify_integer(size, heapSize);
    } else {
        return PL_unify_float(size, heapSize);
    }
}

function statistics_heap_size() {
    return state.H;
}

function gc_check(t)
{
    // noinspection JSBitwiseOperatorUsage
    if (t & M_BIT)
        abort("GC exception: " + hex(t) + " has M_BIT set");
}

function check_stacks(m)
{
    check_environments(state.E, state.CP.code[state.CP.offset - 1], m);
}

function check_environments(initial, envsize, m)
{
    var e = initial;
    while (e !== HEAP_SIZE)
    {
        // Traversing backwards to ensure we do not stop prematurely
        for (var y = 0; y < envsize; y++)
        {            
            if (TAG(memory[e+2+y]) === TAG_STR ||
                TAG(memory[e+2+y]) === TAG_LST)
            {
                check_term(memory[e+2+y], m);
            }
            else 
            {
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
    if (!m)
    {
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
        stdout("Loaded " + Object.keys(predicates).length + " predicates\n");
        stdout("Loaded " + atable.length + " atoms\n");
        stdout("Loaded " + ftable.length + " functors\n");
    }

    initialize();

    call_directives();

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

function call_directives() {

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
}

// proscriptls calls the given query using the current predicates definitions.
// All other global runtime data is saved and restored.
// This allows the asserta/assertz clauses to persist across calls of proscriptls.

function proscriptls(queryJS, displaySucceededMsg) {
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

function proscript_apply(goalArguments, goal) {
    proscriptls_apply(goalArguments, goal);
}

function proscriptls_apply(goalArguments, goal) {
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
        let goalString = goal.substring(typedArgumentPrefix.length);

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
            if (convert_result(argument, {type: type}, resultContainer)) {
                let argumentPL = resultContainer.value;
                let argumentReconstituted = format_term(argumentPL, {quoted: true});
                unificationExpressions.push(variable + " = " + argumentReconstituted);
            }
        }

        let argumentUnificationsPrefix = unificationExpressions.join(", ");
        goalReconstituted = argumentUnificationsPrefix + ", " + goalString;
    } else {
        goalReconstituted = goal;
    }

    proscriptls(goalReconstituted);
}

function debug(msg) {
    if(debugging) {
        alert(msg);
    }
}
// File debugger.js
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

function decode_instruction(predicateID, codePosition) {
    let predicate = (predicateID == null) ? ("no predicate") : (atable[ftable[predicateID.key][0]] + "/" + ftable[predicateID.key][1]);
    let op = code[codePosition];
    let instruction = '';
    let instructionSize = -1;

    switch(op) {
        // Control instructions 1-5
        case 1: // allocate
            instruction = 'allocate';
            instructionSize = 1;
            break;
        case 2: // deallocate
            instruction = 'deallocate';
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

            instruction = 'call(' + functor + '/' + arity + ',' + N + ')';
            instructionSize = 3;
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
            instructionSize = 2;
            break;
        }
        case 5: // proceed
            instruction = 'proceed';
            instructionSize = 1;
            break;

        // Put instructions 6-15, 51, and 60
        case 6: // put_variable: [6, N, I]
        {
            let N = code[codePosition + 1];
            let I = code[codePosition + 2];
            instruction = 'put_variable(y(' + N + '), x(' + I + '))';
            instructionSize = 3;
           break;
        }
        case 60: // put_variable: [60, N]
        {
            let N = code[codePosition + 1];
            instruction = 'put_variable(y(' + N + '))';
            instructionSize = 2;
            break;
        }
        case 7: // put_variable: [7, N, I]
        {
            let N = code[codePosition + 1];
            let I = code[codePosition + 2];
            instruction = 'put_variable(x(' + N + '), x(' + I + '))';
            instructionSize = 3;
            break;
        }
        case 8: // put_value: [8, 0, N, I] or [8, 1, N, I]
        {
            let A = code[codePosition + 1];
            let N = code[codePosition + 2];
            let I = code[codePosition + 3];

            let V = (A === 0) ? 'y' : 'x';

            instruction = 'put_variable(' + V  + '(' + N + '), x(' + I + '))';
            instructionSize = 4;
            break;
        }
        case 9: // put_unsafe_value: [9, N, I]
        {
            let N = code[codePosition + 1];
            let I = code[codePosition + 2];
            instruction = 'put_unsafe_value(y(' + N + '), x(' + I + '))';
            instructionSize = 3;
            break;
        }
        case 10: // put_constant: [10, K, I]
        {
            let K = code[codePosition + 1];
            let I = code[codePosition + 2];

            let C = atable[VAL(K)];
            instruction = 'put_constant(' + C + ', x(' + I + '))';
            instructionSize = 3;
            break;
        }
        case 11: // put_nil: [I]
        {
            let I = code[codePosition + 1];
            instruction = 'put_nil(x(' + I + '))';
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
            instruction = 'put_structure('  + functor + '/' + arity +  ', x(' + I + '))';
            instructionSize = 3;
            break;
        }
        case 13: // put_list: [13, I]
        {
            let I = code[codePosition + 1];

            instruction = 'put_list(x(' + I + '))';
            instructionSize = 2;
            break;
        }
        case 14: // put_integer: [14, C, I]
        {
            let C = code[codePosition + 1];
            let I = code[codePosition + 2];

            instruction = 'put_integer(' + C + ', x(' + I + '))';
            instructionSize = 3;
            break;
        }
        case 51: // put_float: [51, N, I]
        {
            let N = code[codePosition + 1];
            let I = code[codePosition + 2];

            let C = floats[VAL(N)];
            instruction = 'put_float(' + C + ', x(' + I + '))';
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

            instruction = 'get_variable(' + V  + '(' + N + '), x(' + I + '))';
            instructionSize = 4;
            break;
        }
        case 16: // get_value: [16, 0, N, I] or [16, 1, N, I]
        {
            let A = code[codePosition + 1];
            let N = code[codePosition + 2];
            let I = code[codePosition + 3];

            let V = (A === 0) ? 'y' : 'x';

            instruction = 'get_value(' + V  + '(' + N + '), x(' + I + '))';
            instructionSize = 4;
            break;
        }
        case 17: // get_constant: [17, K, I]
        {
            let K = code[codePosition + 1];
            let I = code[codePosition + 2];

            let C = atable[VAL(K)];
            instruction = 'get_constant(' + C + ', x(' + I + '))';
            instructionSize = 3;
            break;
        }
        case 18: // get_nil: [18, I]
        {
            let I = code[codePosition + 1];
            instruction = 'get_nil(x(' + I + '))';
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
            instruction = 'get_structure('  + functor + '/' + arity +  ', x(' + I + '))';
            instructionSize = 3;
            break;
        }
        case 20: // get_list: [20, I]
        {
            let I = code[codePosition + 1];

            instruction = 'get_list(x(' + I + '))';
            instructionSize = 2;
            break;
        }
        case 21: // get_integer: [21, C, I]
        {
            let C = code[codePosition + 1];
            let I = code[codePosition + 2];

            instruction = 'get_integer(' + C + ', x(' + I + '))';
            instructionSize = 3;
            break;
        }
        case 50: // get_float: [50, N, I]
        {
            let N = code[codePosition + 1];
            let I = code[codePosition + 2];

            let C = floats[VAL(N)];
            instruction = 'get_float(' + C + ', x(' + I + '))';
            instructionSize = 3;
            break;
        }

        // Unify instructions 22-27 and 52
        case 22: // unify_void: [22, N]
        {
            let N = code[codePosition + 1];

            instruction = 'unify_void(' + N + ')';
            instructionSize = 2;
            break;
        }
        case 23: // unify_variable: [23, 0, N] or [23, 1, N]
        {
            let A = code[codePosition + 1];
            let N = code[codePosition + 2];

            let V = (A === 0) ? 'y' : 'x';

            instruction = 'unify_variable(' + V  + '(' + N + ')';
            instructionSize = 3;
            break;
        }
        case 24: // unify_value: [24, 0, N] or [24, 1, N]
        {
            let A = code[codePosition + 1];
            let N = code[codePosition + 2];

            let V = (A === 0) ? 'y' : 'x';

            instruction = 'unify_value(' + V  + '(' + N + ')';
            instructionSize = 3;
            break;
        }
        case 25: // unify_local_value: [25, 0, N] or [25, 1, N]
        {
            let A = code[codePosition + 1];
            let N = code[codePosition + 2];

            let V = (A === 0) ? 'y' : 'x';

            instruction = 'unify_local_value(' + V  + '(' + N + ')';
            instructionSize = 3;
            break;
        }
        case 26: // unify_constant: [26, K]
        {
            let K = code[codePosition + 1];

            let C = atable[VAL(K)];
            instruction = 'unify_constant(' + C + ')';
            instructionSize = 2;
            break;
        }
        case 27: // unify_integer: [27, C]
        {
            let C = code[codePosition + 1];

            instruction = 'unify_integer(' + C + ')';
            instructionSize = 2;
            break;
        }
        case 52: // unify_float: [52, N]
        {
            let N = code[codePosition + 1];

            let C = floats[VAL(N)];
            instruction = 'unify_float(' + C + ')';
            instructionSize = 2;
            break;
        }
            // Indexing instructions 28-30
        case 28: // try_me_else: [28, L]
        {
            let L = code[codePosition + 1];

            instruction = 'try_me_else(' + L + ')';
            instructionSize = 2;
            break;
        }
        case 29: // retry_me_else: [29, L]
        {
            let L = code[codePosition + 1];

            instruction = 'retry_me_else(' + L + ')';
            instructionSize = 2;
            break;
        }
        case 30: // trust_me: [30, 0]
        {
            instruction = 'trust_me(0)';
            instructionSize = 2;
            break;
        }

        // Cut instructions
        case 31: // neck_cut: [31]
        {
            instruction = 'neck_cut';
            instructionSize = 1;
            break;
        }
        case 32: // cut: [32, I]
        {
            let I = code[codePosition + 1];

            instruction = 'cut(y(' + I + '))';
            instructionSize = 2;
            break;
        }
        case 33: // get_level: [33, I]
        {
            let I = code[codePosition + 1];

            instruction = 'get_level(y(' + I + '))';
            instructionSize = 2;
            break;
        }

        // Aux instructions. Used for ; and ->. Basically just call with an offset rather than a functor to look up
        case 40: // call_aux: [40, P, A, N]
        {
            let P = code[codePosition + 1];
            let A = code[codePosition + 2];
            let N = code[codePosition + 3];

            instruction = 'call_aux(' + P + ',' + A + ',' + N +'))';
            instructionSize = 4;
            break;
        }
        case 41: // execute_aux: [41, P, A]
        {
            let P = code[codePosition + 1];
            let A = code[codePosition + 2];

            instruction = 'execute_aux(' + P + ',' + A +'))';
            instructionSize = 3;
            break;
        }
        // retry_foreign is for foreign predicates with nondeterministic behaviour
        case 42: // retry_foreign: [42]
        {
            instruction = 'retry_foreign';
            instructionSize = 1;
            break;
        }
            // get_choicepoint is used for setup_call_cleanup
            //encode_opcode(get_choicepoint(N, y(I)), 3, [43, N, I]).
        case 43: // get_choicepoint: [43, N, I]
        {
            let N = code[codePosition + 1];
            let I = code[codePosition + 2];
            instruction = 'get_choicepoint(' + N + ', y(' + I + '))';
            instructionSize = 3;
            break;
        }
        case 254: // nop2: [254, 0]
        {
            instruction = 'nop2(0)';
            instructionSize = 2;
            break;
        }

        default:
            instruction = 'unknown(' + op + ')';
            instructionSize = 1;
            break;
    }

    return {string: (predicate + ':' + '(' + instruction + ',' + codePosition + ')'), size:instructionSize};

}
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
    ['window', ['eventtarget']],
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
    ['navigatorid', []],
    ['navigatorlanguage', []],
    ['navigatoronline', []],
    ['navigatorcontentutils', []],
    ['navigatorcookies', []]
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
    "Navigator" : 'navigator'
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
    canvasrenderingcontext2d: 'getImageData',
    blob: 'slice'
};

// [createImageData, [number, number], object]
// [MethodName, ArgTypes, ReturnType] or [MethodName, ArgTypes] if no return.
//
// ['createImageData',{name:'createImageData',arguments:[{type:'number'},{type:'number'}],returns:{type:'object'}}]

function convert_method_spec(specTerm, resultContainer) {
    if (TAG(specTerm) !== TAG_LST) {
        return type_error("list", specTerm);
    }

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
        if(functor !== 'array') {
            return domain_error('type array', functorPL);
        }
        let arity = ftable[VAL(memory[VAL(typePL)])][1];
        if(arity !== 1) {
            return representation_error('type array arity 1', typePL);
        }
        let subContainer = {};
        if(! convert_type_term(deref(memory[VAL(typePL) + 1]), subContainer, true)) {
            return false;
        }
        let extendedType = {};
        extendedType.array = subContainer.value;
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

function predicate_dom_create_object(type, object, spec) {
    if(TAG(type) !== TAG_ATM && TAG(type) !== TAG_STR) {
        return type_error('atom or structure', type);
    }

    let typeJS;
    let structureArguments = [];
    let arity;
    if(TAG(type) === TAG_ATM) {
        typeJS = atable[VAL(type)];
    } else if(TAG(type) === TAG_STR) {
        let functorPL = ftable[VAL(memory[VAL(type)])][0];
        arity = ftable[VAL(memory[VAL(type)])][1];
        typeJS = atable[functorPL];
        for(let ofst = 0; ofst < arity;ofst++) {
            let argument = deref(memory[VAL(type) + ofst + 1]);
            structureArguments.push(argument);
        }
    } else {
        return type_error('atom or structure', type);
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
            if (convert_method_argument(structureArguments[i], specArguments[i], applyArgumentContainer)) {
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

The Proscript implementation provides explicit access to selected properties and methods
of various WebAPI objects and Javascript runtime objects.
Currently the supported WebAPI objects are Node, Element, and HTMLElement.
The Javascript runtime object is Promise.

The properties are handled in the Javascript supporting Proscript
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
    ['setProperty',{name:'setProperty',arguments:[{type:'string'},{type:'string'},{type:'atom'}]}]
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
    ['setProperty',{name:'setProperty',arguments:[{type:'string'},{type:'string'},{type:'atom'}]}]
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
    ['getPropertyPriority',{name:'getPropertyPriority',arguments:[{type:'string'}],returns:{type:'atom'}}],
    ['getPropertyValue',{name:'getPropertyValue',arguments:[{type:'string'}],returns:{type:'atom'}}],
    ['item',{name:'item',arguments:[{type:'integer'}],returns:{type:'atom'}}],
    ['removeProperty',{name:'removeProperty',arguments:[{type:'string'}],returns:{type:'atom'}}],
    ['setProperty',{name:'setProperty',arguments:[{type:'string'},{type:'string'},{type:'atom'}]}]
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
    ['pageY', SimpleProperty('number', 'pageY')]
]);

var mouseEventMethodSpecs = new Map([
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
// File object_property.js

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

    let result;
    if ((VAL(value) & (1 << (WORD_BITS-1))) === (1 << (WORD_BITS-1)))
        result = VAL(value) - (1 << WORD_BITS);
    else
        result = VAL(value);
    container.value = result;
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
/*
The general approach to providing access to Javascript objects
through Prolog predicates is described in object_property.js.

This file implements access to Javascript object methods.


 */

// object_method(Element, add_event_listener(click, bar(thing))).
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
        if (TAG(methodStructure) === TAG_ATM) {
            methodName = atable[VAL(methodStructure)];
            arity = 0;
        } else {
            methodName = atable[ftable[VAL(memory[VAL(methodStructure)])][0]];
            arity = ftable[VAL(memory[VAL(methodStructure)])][1];
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
            if (convert_method_argument(deref(memory[VAL(methodStructure) + i + 1]), specArgument, applyArgumentContainer)) {
                applyArguments.push(applyArgumentContainer.value);
            } else {
                return false;
            }
        }

        if (spec.returns) {
            let resultJS = object_method_return(objectJS, spec.name, applyArguments);
            if(spec.returns.multiple) {
                let values = [];
                if(typeof resultJS !== 'undefined' && resultJS !== null) {
                    if(typeof resultJS === 'object' && resultJS.constructor.name === 'NodeList') {
                        values = Array.from(resultJS);
                    } else if(typeof resultJS === 'object' && resultJS.constructor.name === 'FileList') {
                        values = Array.from(resultJS);
                    } else if(typeof resultJS === 'object' && resultJS.constructor.name === 'HTMLOptionsCollection') {
                        values = Array.from(resultJS);
                    } else if(typeof resultJS === 'object' && resultJS.constructor.name === 'HTMLCollection') {
                        values = Array.from(resultJS);
                    } else {
                        values.push(resultJS);
                    }
                }

                cursor = {values: values, spec: spec, arity: arity};
                cursorIDJS = 'crs' + domCursorCounter++;
                domCursors.set(cursorIDJS, cursor);
                cursorIDPL = lookup_atom(cursorIDJS);

                create_choicepoint();

            } else {
                let resultContainer = {};
                if (convert_result(resultJS, spec.returns, resultContainer)) {
                    let resultPL = resultContainer.value;
                    if (spec.returns.type === 'boolean') {
                        return resultPL;
                    } else {
                        return unify(resultPL, deref(memory[VAL(methodStructure) + arity + 1]));
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
        if (convert_result(resultJS, cursor.spec.returns, resultContainer)) {
            let resultPL = resultContainer.value;
            if (cursor.spec.returns.type === 'boolean') {
                return resultPL;
            } else {
                return unify(resultPL, deref(memory[VAL(methodStructure) + cursor.arity + 1]));
            }
        } else {
            return false;
        }
    }

    destroy_choicepoint();
    return false;
}

function convert_method_argument(term, spec, resultContainer, reportError) {
    if(TAG(term) === TAG_REF) {
        return instantiation_error(term);
    }

    let arg;
    if(typeof spec.type === 'object') {
        if(spec.type.arrayType) {
            // [X1, X2, ...] for array of arrayType items
            if(TAG(term) === TAG_LST) {
                let arrayContainer = {};
                if(terms_to_array(term, spec.type.arrayType, arrayContainer, reportError)) {
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
                if (convert_method_argument(term, {type: subtype}, resultContainer, false)) {
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
            goal = format_term(term, {quoted: true});
        } else {
            return reportError && type_error('atom or structure', term);
        }

        arg = goalFunctions.get(goal);
        if (!arg) {
            arg = function () {
                proscript_apply(arguments, goal);
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

function terms_to_array(listRoot, itemType, arrayContainer, reportError) {
    var array = [];

    var list = listRoot;

    while(list !== NIL) {
        if(TAG(list) !== TAG_LST) {
            return instantiation_error(list);
        }

        var itemPL = deref(memory[VAL(list)]);
        let itemContainer = {};
        if(convert_method_argument(itemPL, {type: itemType}, itemContainer, reportError)) {
            array.push(itemContainer.value);
        } else {
            return false;
        }

        list = deref(memory[VAL(list) + 1]);
    }

    arrayContainer.value = array;
    return true;
}

function result_array_to_terms(arrayJS, itemType, arrayContainer, reportError) {
    if(arrayJS.length === 0) {
        arrayContainer.value = NIL;
    } else {

        arrayContainer.value = state.H ^ (TAG_LST << WORD_BITS);
        for (var i = 0; i < arrayJS.length; i++) {
            let itemContainer = {};
            if(convert_result(arrayJS[i], {type: itemType}, itemContainer, reportError)) {
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

function convert_result(resultJS, spec, resultContainer, reportError) {
    let resultPL;
    if(typeof spec.type === 'object') {
        if(spec.type.arrayType) {
            // [X1, X2, ...] for array of arrayType items
            if(Array.isArray(resultJS)) {
                let arrayContainer = {};
                if(result_array_to_terms(resultJS, spec.type.arrayType, arrayContainer, reportError)) {
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
                if (convert_method_argument(resultJS, {type: subtype}, resultContainer, false)) {
                    return true;
                }
            }
            return reportError && type_error('union: ' + spec.type, lookup_atom(JSON.stringify(resultJS)));
        }
    } else if(spec.type === 'atom') {
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
        memory[state.H++] = getIntegerPLPropertyValue(resultJS.left);
        memory[state.H++] = getIntegerPLPropertyValue(resultJS.top);
        memory[state.H++] = getIntegerPLPropertyValue(resultJS.right);
        memory[state.H++] = getIntegerPLPropertyValue(resultJS.bottom);
        memory[state.H++] = getIntegerPLPropertyValue(resultJS.x);
        memory[state.H++] = getIntegerPLPropertyValue(resultJS.y);
        memory[state.H++] = getIntegerPLPropertyValue(resultJS.width);
        memory[state.H++] = getIntegerPLPropertyValue(resultJS.height);
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
function load_state() {
bootstrap_code = [0,255,3,164,0,4,165,42];
retry_foreign_offset = 7;
atable = ["[]", "system:acyclic_term", "system", "acyclic_term", "system:subsumes_term", "subsumes_term", "system:compare", "compare", "system:var", "var", "system:atom", "atom", "system:integer", "integer", "system:float", "float", "system:compound", "compound", "system:ground", "ground", "system:=", "=", "system:==", "==", "system:functor", "functor", "system:arg", "arg", "system:=..", "=..", "system:copy_term", "copy_term", "system:halt", "halt", "system:current_prolog_flag", "current_prolog_flag", "system:set_prolog_flag", "set_prolog_flag", "system:repeat", "repeat", "system:atom_length", "atom_length", "system:atom_concat", "atom_concat", "system:sub_atom", "sub_atom", "system:char_code", "char_code", "system:atom_chars", "atom_chars", "system:atom_codes", "atom_codes", "system:number_chars", "number_chars", "system:number_codes", "number_codes", "system:char_conversion", "char_conversion", "system:current_char_conversion", "current_char_conversion", "system:current_predicate", "current_predicate", "system:@>", "@>", "system:@>=", "@>=", "system:@<", "@<", "system:@=<", "@=<", "system:is", "is", "system:>", ">", "system:<", "<", "system:=<", "=<", "system:>=", ">=", "system:=:=", "=:=", "system:=\\=", "=\\=", "system:set_input", "set_input", "system:set_output", "set_output", "system:current_output", "current_output", "system:current_input", "current_input", "system:get_char", "get_char", "system:get_code", "get_code", "system:peek_char", "peek_char", "system:peek_code", "peek_code", "system:put_char", "put_char", "system:put_code", "put_code", "system:get_byte", "get_byte", "system:peek_byte", "peek_byte", "system:put_byte", "put_byte", "system:flush_output", "flush_output", "system:at_end_of_stream", "at_end_of_stream", "system:set_stream_position", "set_stream_position", "system:stream_property_1", "stream_property_1", "system:current_stream", "current_stream", "system:write_term", "write_term", "system:current_op", "current_op", "system:fail", "fail", "system:true", "true", "system:term_variables", "term_variables", "system:writeln", "writeln", "system:gensym", "gensym", "system:atom_to_term", "atom_to_term", "system:clause", "clause", "system:abolish", "abolish", "system:retract_clause", "retract_clause", "system:read_term", "read_term", "system:open", "open", "system:close", "close", "system:op", "op", "system:atom_to_memory_file", "atom_to_memory_file", "system:memory_file_to_atom", "memory_file_to_atom", "system:new_memory_file", "new_memory_file", "system:open_memory_file", "open_memory_file", "system:free_memory_file", "free_memory_file", "system:format", "format", "system:flag", "flag", "system:memory_file_description", "memory_file_description", "system:reset_compile_buffer", "reset_compile_buffer", "system:emit_code", "emit_code", "system:lookup_atom", "lookup_atom", "system:lookup_float", "lookup_float", "system:lookup_functor", "lookup_functor", "system:add_clause_to_predicate", "add_clause_to_predicate", "system:add_clause_to_aux", "add_clause_to_aux", "system:prepend_clause_to_predicate", "prepend_clause_to_predicate", "system:flush_stdout", "flush_stdout", "system:debug", "debug", "system:nodebug", "nodebug", "system:$jmp", "$jmp", "system:generate_initialization_goal", "generate_initialization_goal", "system:generate_system_goal", "generate_system_goal", "system:define_dynamic_predicate", "define_dynamic_predicate", "system:compiled_state_boot_code", "compiled_state_boot_code", "system:dump_tables", "dump_tables", "system:add_module_export", "add_module_export", "system:method_export", "method_export", "system:request_result", "request_result", "system:handle_result", "handle_result", "system:fetch_promise", "fetch_promise", "system:trace_unify", "trace_unify", "system:$trace_set", "$trace_set", "system:$trace_value", "$trace_value", "system:$trace_set_info", "$trace_set_info", "system:$suspend_set", "$suspend_set", "system:get_terminal_char", "get_terminal_char", "system:$trace_set_retry", "$trace_set_retry", "system:$trace_retry_value", "$trace_retry_value", "system:$trace_set_prompt", "$trace_set_prompt", "system:$get_backtrack_frame", "$get_backtrack_frame", "system:$set_backtrack_frame", "$set_backtrack_frame", "system:$trace_instruction_set", "$trace_instruction_set", "system:member", "member", "system:mark_top_choicepoint", "mark_top_choicepoint", "system:unmark_choicepoint", "unmark_choicepoint", "system:unmark_top_choicepoint", "unmark_top_choicepoint", "system:get_current_block", "get_current_block", "system:install_new_block", "install_new_block", "system:reset_block", "reset_block", "system:unwind_stack", "unwind_stack", "system:clean_up_block", "clean_up_block", "system:throw", "throw", "system:get_exception", "get_exception", "system:clear_exception", "clear_exception", "system:recorda", "recorda", "system:recordz", "recordz", "system:recorded", "recorded", "system:erase", "erase", "system:record_term", "record_term", "system:gc", "gc", "system:statistics", "statistics", "system:wam_duration", "wam_duration", "system:eval_javascript", "eval_javascript", "system:remove_dom_element_class", "remove_dom_element_class", "system:replace_dom_element_class", "replace_dom_element_class", "system:toggle_dom_element_class", "toggle_dom_element_class", "system:set_dom_element_attribute_value", "set_dom_element_attribute_value", "system:dom_element_attribute_value", "dom_element_attribute_value", "system:create_dom_element", "create_dom_element", "system:create_dom_text_node", "create_dom_text_node", "system:append_dom_node_child", "append_dom_node_child", "system:insert_before_dom_node", "insert_before_dom_node", "system:dom_select_element", "dom_select_element", "system:dom_select_all_elements", "dom_select_all_elements", "system:dom_object_property", "dom_object_property", "system:dom_object_method", "dom_object_method", "system:dom_object_type", "dom_object_type", "system:dom_create_object", "dom_create_object", "system:dom_type_reference", "dom_type_reference", "system:dom_release_object", "dom_release_object", "system:set_dom_object_property", "set_dom_object_property", "system:alert", "alert", "system:dom_window", "dom_window", "system:dom_type_property", "dom_type_property", "system:dom_type_method", "dom_type_method", "system:dom_type_parent", "dom_type_parent", "toplevel", "user:writeln", ":-", "bootstrap", "run_unit_tests", "user:compile_clause", "user:bootstrap", "user:toplevel", "bootstrap_js", "append", "assert", "save_clausea", "handle_term_expansion", "include", "dynamic", "consult_atom", "ensure_loaded", "compile_message", "??", "?", "otherwise", "end_block", "findall", "setof", "bagof", "asserta", "assertz", "retract", "unify_with_occurs_check", "\\=", "\\==", "atomic", "number", "stream_property", "read", "write", "writeq", "write_canonical", "callable", "retractall", "sort", "keysort", "length", "delete", "call", "define_current_module", "bootstrap_js:define_current_module", "module", "bootstrap_js:module", "define_use_module", "bootstrap_js:define_use_module", "use_module", "bootstrap_js:use_module", "bootstrap_js:assertz", "bootstrap_js:assert", "transform_predicate_name", "bootstrap_js:transform_predicate_name", "/", "bootstrap_js:save_clausea", "bootstrap_js:handle_term_expansion", "bootstrap_js:include", "consult", "bootstrap_js:call", "query", "wam_compiler:compile_clause_2", "wam_compiler:define_dynamic_predicate", "bootstrap_js:dynamic", ",", "compile_atom", "bootstrap_js:compile_atom", "bootstrap_js:consult_atom", "wam_compiler:canonical_source", "wam_compiler:$loaded", "wam_compiler:consult", "bootstrap_js:ensure_loaded", "bootstrap_js:format", "bootstrap_js:compile_message", "CALL ~q~n", "bootstrap_js:end_block", "exception", "exit", "!", "bootstrap_js:halt", "FAIL ~q~n", "EXIT ~q~n", "CUT  ~q~n", "error", "ERROR ~q ~p~n", "PEND ~q~n", "bootstrap_js:otherwise", "bootstrap_js:??", "bootstrap_js:?", "save_instances", "list_instances", "-", "bootstrap_js:save_instances", "bootstrap_js:list_instances", "bootstrap_js:findall", "bag_of", "bootstrap_js:bag_of", "bootstrap_js:sort", "set_of", "bootstrap_js:set_of", "free_variables", "concordant_subset", "bootstrap_js:free_variables", "bootstrap_js:\\==", ".", "bootstrap_js:keysort", "bootstrap_js:concordant_subset", "existential_variables", "bootstrap_js:existential_variables", "^", "replace_key_variables", "bootstrap_js:replace_key_variables", "nonvar", "bootstrap_js:nonvar", "bootstrap_js:unify_with_occurs_check", "bootstrap_js:\\=", "bootstrap_js:number", "bootstrap_js:atomic", "bootstrap_js:asserta", "save_clause", "bootstrap_js:save_clause", "bootstrap_js:retract", "bootstrap_js:setof", "bootstrap_js:bagof", "bootstrap_js:open", "bootstrap_js:close", "bootstrap_js:flush_output", "bootstrap_js:stream_property", "bootstrap_js:at_end_of_stream", "bootstrap_js:get_char", "bootstrap_js:get_code", "bootstrap_js:peek_char", "bootstrap_js:peek_code", "bootstrap_js:put_char", "bootstrap_js:put_code", "bootstrap_js:get_byte", "bootstrap_js:peek_byte", "bootstrap_js:put_byte", "bootstrap_js:read_term", "bootstrap_js:read", "bootstrap_js:write_term", "quoted", "false", "ignore_ops", "numbervars", "bootstrap_js:write", "bootstrap_js:writeq", "bootstrap_js:write_canonical", "bootstrap_js:callable", "bootstrap_js:retractall", "partition", "bootstrap_js:partition", "bootstrap_js:append", "key_partition", "bootstrap_js:key_partition", "bootstrap_js:length", "+", "bootstrap_js:delete", "debugger", "trace", "notrace", "notrace_backtrackable", "$traceR", "debugger:trace", "no_trace", "debugger:notrace", "no_trace_backtrackable", "debugger:notrace_backtrackable", "$trace", "debugger:$trace", "$trace_call_msg", "$trace_exit_msg", "debugger:$trace_call_msg", "trace_next_jmp", "debugger:$trace_exit_msg", "Call ", "Fail ", "Exit ", "Redo ", "$trace_msg", "$trace_push_info", "Call", "Fail", "debugger:$trace_msg", "debugger:$trace_push_info", "Exit", "Redo", "$trace_msg1", "debugger:$trace_msg1", " ", "~w\n", "debugger:$traceR", "$trace_retry", "$trace_interact", "$trace_suspend_if_active", "$trace_is_suspended", "debugger:$trace_retry", "debugger:$trace_interact", "debugger:$trace_suspend_if_active", "debugger:$trace_is_suspended", "redo", "skip_trace", "suspend_leap_trace", "leap_trace", "none", "$trace_interaction_enabled", "$trace_prompt", "$trace_read_and_cmd", "$trace_cmd", "debugger:$trace_interaction_enabled", "debugger:$trace_prompt", "debugger:$trace_read_and_cmd", "l", "debugger:$trace_cmd", "$trace_spy_mode", "debugger:$trace_spy_mode", "all", "$trace_spy_specification", "specified", "debugger:$trace_spy_specification", "Found spec for ", "$trace_spy_mode1", "debugger:$trace_spy_mode1", "$trace_create_prompt", "debugger:$trace_create_prompt", "pad_number", "concat_list", "debugger:pad_number", "debugger:concat_list", "capitalize", "debugger:capitalize", ": ", "$trace_check_command", "debugger:$trace_check_command", "read_char", "debugger:read_char", "z", "y", "x", "m", "n", "a", "g", "r", "f", "s", "c", "Commands are: \"c\" (creep), \"s\" (skip), \"l\" (leap), \"+\" (spy this), \"-\" (nospy this), \"f\" (fail), \"r\" (retry), \"g\" (ancestors), \"a\" (abort), \"n\" (nodebug), \"m\" (creep wam), \"x\" (creep wam long), \"y\" (trace wam), \"z\" (trace wam long).", "$trace_cmd_creep", "debugger:$trace_cmd_creep", "leap_trace_next_jmp", "$trace_write_ancestors", "debugger:$trace_write_ancestors", "Spypoint placed on ", "Spypoint removed from ", "$trace_cmd_creep_wam", "debugger:$trace_cmd_creep_wam", "step", "reverse", "$trace_write_ancestors1", "debugger:reverse", "Ancestors:", "debugger:$trace_write_ancestors1", "$suspend", "debugger:$suspend", "pad_codes", "debugger:pad_codes", "pad_codes1", "debugger:pad_codes1", "capitalize_code", "debugger:capitalize_code", "", "~w", "url", "current_compile_url", "push_current_compile_url", "pop_current_compile_url", "convert_URL_to_base", "url_directory", "resolve_url", "$current_compile_url", "url:$current_compile_url", "url:current_compile_url", "url:push_current_compile_url", "url:pop_current_compile_url", "absolute_url", "absolute_file_name", "url:absolute_url", "url:url_directory", "url:resolve_url", "url:absolute_file_name", "url:convert_URL_to_base", "./", "sub", "url:sub", "trim_directory", "url:trim_directory", "url:reverse", "not", "telling", "tell", "nl", "break", "not:free_variables", "not:telling", "user", "not:tell", "not:nl", "** ", "-- free variables ", "not:break", "not:not", "term_is_free_of", "list_is_free_of", "not:term_is_free_of", "not:list_is_free_of", "explicit_binding", "not:explicit_binding", "\\+", "promise", "promise_result", "promise:promise_result", "wam_compiler", "compile_clause", "compile_files", "save_compiled_state", "wam_assemble", "assemble", "wam_bootstrap_util", "lookup_dynamic_functor", "ftable", "fltable", "atable", "clause_table", "fptable", "ctable", "itable", "stable", "dtable", "$module_export", "add_clause_to_existing", "wam_bootstrap_util:ftable", "wam_bootstrap_util:fltable", "wam_bootstrap_util:atable", "wam_bootstrap_util:clause_table", "wam_bootstrap_util:fptable", "wam_bootstrap_util:ctable", "wam_bootstrap_util:itable", "wam_bootstrap_util:stable", "wam_bootstrap_util:dtable", "wam_bootstrap_util:$module_export", "wam_bootstrap_util:lookup_dynamic_functor", "/\\", "wam_bootstrap_util:nonvar", "defined", "xor", "wam_bootstrap_util:add_clause_to_existing", "label", "encode_opcodes_1", "link", "wam_assemble:encode_opcodes_1", "linking", "wam_assemble:link", "linked", "wam_assemble:assemble", "link_1", "wam_assemble:link_1", "address_of", "to", "emitting_address_1_of", "aux_address_of", "emitting_aux_address_2_of", "emit_codes", "wam_assemble:emit_codes", "encode_register", "wam_assemble:encode_register", "aux_label", "auxiliary", "call_aux", "    ", "at", "need_address_of", "execute_aux", "try_me_else", "encode_opcode", "wam_assemble:encode_opcode", "allocate", "deallocate", "execute", "proceed", "put_variable", "put_value", "put_unsafe_value", "illegal_unsafe_value_register", "put_constant", "put_nil", "put_structure", "put_list", "put_integer", "put_float", "get_variable", "get_value", "get_constant", "get_nil", "get_structure", "get_list", "get_integer", "get_float", "unify_void", "unify_variable", "unify_value", "unify_local_value", "unify_constant", "unify_integer", "unify_float", "retry_me_else", "trust_me", "neck_cut", "cut", "get_level", "retry_foreign", "get_choicepoint", "nop2", "wam_util", "transform_predicate_name1", "path_to_module_name", "list_length", "library_name_to_path", "library_path", "wam_util:transform_predicate_name1", "../../library/", "wam_util:library_path", "wam_util:library_name_to_path", "path_to_file_name", "strip_suffix", "wam_util:path_to_file_name", "wam_util:strip_suffix", "wam_util:path_to_module_name", "list_length_1", "wam_util:list_length_1", "wam_util:list_length", "delayed_initialization", "wam_compiler:delayed_initialization", "$loaded", "$current_compilation_module", "wam_compiler:$current_compilation_module", "$current_compilation_stream", "wam_compiler:$current_compilation_stream", "$import", "wam_compiler:$import", "$meta_predicate", "wam_compiler:$meta_predicate", "term_expansion", "wam_compiler:term_expansion", "expand_term", "wam_compiler:expand_term", "compile", "mode", "wam_compiler:compile_clause", "compile_clause_1", "Compiling ", "done_gc", "wam_compiler:compile_clause_1", "Failed compilation of ", "compile_clause_save", "wam_compiler:compile_clause_save", "compile_clause_directive", "wam_compiler:compile_clause_directive", "mode_skip", "compile_clause_2", "wam_compiler:mode_skip", "wam_compiler:save_clause", "skip", "mode_compile", "wam_compiler:mode_compile", "compile_clause_directive_macro", "compile_clause_directive_nonmacro", "wam_compiler:compile_clause_directive_macro", "wam_compiler:compile_clause_directive_nonmacro", "compile_clause_system_directive", "wam_compiler:compile_clause_system_directive", "compile_clause_compilation_directive", "wam_compiler:compile_clause_compilation_directive", "compile_clause_initialization_directive", "initialization", "wam_compiler:compile_clause_initialization_directive", "reexport", "meta_predicate", "if", "else", "elseif", "endif", "define_dynamic_predicates", "wam_compiler:define_dynamic_predicates", "wam_compiler:define_current_module", "wam_compiler:define_use_module", "define_reexport", "wam_compiler:define_reexport", "define_meta_predicate", "wam_compiler:define_meta_predicate", "macro_if", "wam_compiler:macro_if", "macro_else", "wam_compiler:macro_else", "macro_elseif", "wam_compiler:macro_elseif", "macro_endif", "wam_compiler:macro_endif", "if skip level >= iflevelnext", "else ifLevel less than skiplevel", "else ifLevel greater than compileLevel", "else ifLevel less than compileLevel", "elseif skip level > iflevel", "if skip level not = iflevel", "if compile level not = iflevel", "endif unknown action", "define_dynamic_predicate1", "wam_compiler:define_dynamic_predicate1", "current_compilation_module", "wam_compiler:current_compilation_module", "wam_compiler:transform_predicate_name", "current_compilation_stream", "push_current_compilation_module", "define_module_export", "wam_compiler:current_compilation_stream", "wam_compiler:push_current_compilation_module", "wam_compiler:define_module_export", "library", "pop_current_compilation_module", "wam_compiler:pop_current_compilation_module", "setup_use_module", "wam_compiler:setup_use_module", "cyclic_module", "module_export", "load_file_for_use_module", "wam_compiler:module_export", "wam_compiler:load_file_for_use_module", "canonical_source", "compile_file", "wam_compiler:compile_file", "use_module_imports", "wam_compiler:use_module_imports", "no_imports", "default_meta_arg_types", "wam_compiler:default_meta_arg_types", "defined_meta_predicate", "wam_compiler:defined_meta_predicate", "$current_import", "clear_imports", "wam_compiler:clear_imports", "current_import", "wam_compiler:current_import", "current_import1", "wam_compiler:current_import1", "transform_body", "commit_to_cut", "entail", "permanent_variable_list", "environment_size_required", "allocate_environment", "first_goal_arity", "compile_body", "compile_auxiliary_goals", "?-", "first", "wam_compiler:transform_body", "wam_compiler:commit_to_cut", "wam_compiler:entail", "wam_compiler:permanent_variable_list", "wam_compiler:environment_size_required", "next", "wam_compiler:allocate_environment", "wam_compiler:first_goal_arity", "wam_compiler:compile_body", "wam_compiler:compile_auxiliary_goals", "compile_head", "wam_compiler:compile_head", "no_cut", "next_free_variable", "wam_compiler:next_free_variable", "has_cut", "goal_expansion", "wam_compiler:goal_expansion", "not_first", "include_cut_point_as_argument_if_needed", "once", "aux_head", "aux_definition", "local_cut", "wam_compiler:include_cut_point_as_argument_if_needed", "instantiate_local_cut", "setup_call_catcher_cleanup", "goal", "get_top_choicepoint", "no_local_cut", "need", "wam_compiler:instantiate_local_cut", "catch", "forall", ";", "->", "variable_is_in_list", "wam_compiler:variable_is_in_list", "include_local_cut_in_arity", "compile_head_args", "wam_compiler:include_local_cut_in_arity", "wam_compiler:compile_head_args", "last_goal", "wam_compiler:last_goal", "grab_variables_from_goals", "classify_variables", "wam_compiler:grab_variables_from_goals", "wam_compiler:classify_variables", "variable_is_permanent", "permanent_var", "last_occurrence", "wam_compiler:variable_is_permanent", "ensure_vars_allocated", "wam_compiler:ensure_vars_allocated", "variable_is_known_permanent", "get", "wam_compiler:variable_is_known_permanent", "put", "deallocate_environment", "wam_compiler:deallocate_environment", "atom_or_empty_list", "wam_compiler:atom_or_empty_list", "compile_head_arg", "wam_compiler:compile_head_arg", "wam_compiler:get_variable", "compile_head_unification", "complete_head_unification", "wam_compiler:compile_head_unification", "wam_compiler:complete_head_unification", "wam_compiler:unify_variable", "fresh_variable", "unify", "wam_compiler:fresh_variable", "already_used", "wam_compiler:already_used", "unify_possibly_local_variable", "wam_compiler:unify_possibly_local_variable", "put_possibly_unsafe_value", "wam_compiler:put_possibly_unsafe_value", "wam_compiler:put_variable", "set", "mark_variable_as_safe", "wam_compiler:mark_variable_as_safe", "variable_has_not_been_trimmed", "wam_compiler:variable_has_not_been_trimmed", "resize_state", "wam_compiler:resize_state", "trim_environment", "wam_compiler:trim_environment", "compile_body_goals", "depart", "wam_compiler:compile_body_goals", "compile_goal", "wam_compiler:compile_goal", "compile_body_args", "compile_aux_call", "wam_compiler:compile_body_args", "wam_compiler:compile_aux_call", "variable_must_be_known_permanent", "wam_compiler:variable_must_be_known_permanent", "compile_predicate_call", ":", "wam_compiler:compile_predicate_call", "wrong_type_of_functor", "missing_permanent_var", "compile_body_arg", "compile_body_arg_adjust", "wam_compiler:compile_body_arg", "guv", "wam_compiler:compile_body_arg_adjust", "adjust_unify_variable", "wam_compiler:adjust_unify_variable", "compile_body_unification", "wam_compiler:compile_body_unification", "generated_unify_variable", "adjust_unify_variable1", "adjust", "wam_compiler:adjust_unify_variable1", "adjust_unify_variable11", "wam_compiler:adjust_unify_variable11", "adjust_unify_variable10", "wam_compiler:adjust_unify_variable10", "adjust_unify_variable2", "wam_compiler:adjust_unify_variable2", "wam_compiler:compile_files", "compile_stream", "wam_compiler:compile_stream", "push_current_compilation_stream", "wam_compiler:push_current_compilation_stream", "pop_current_compilation_stream", "wam_compiler:pop_current_compilation_stream", "compile_stream_term", "wam_compiler:compile_stream_term", "call_init", "end_of_file", "macro error: missing endif directive.", "wam_compiler:call_init", "wam_compiler:save_compiled_state", "function load_state() {~n", "bootstrap_code = ~w;~n", "retry_foreign_offset = 7;~n", "}~n", "call_list", "wam_compiler:call_list", "compile_atoms", "wam_compiler:compile_atoms", "wam_compiler:compile_atom", "compile_and_free_memory_file", "wam_compiler:compile_and_free_memory_file", "Compiled atom", "compile_memory_file", "wam_compiler:compile_memory_file", "canonical_sources", "fetch_promises", "compile_results", "wam_compiler:canonical_sources", "wam_compiler:fetch_promises", "wam_compiler:compile_results", "true.", "print_bindings", "wam_compiler:print_bindings", "print_bindings_1", "wam_compiler:print_bindings_1", "repl_1", "wam_compiler:repl_1", "repl", "wam_compiler:repl", "write_list", "*", "//", "ms)", "(", "wam_compiler:write_list", "write_list1", "wam_compiler:write_list1", "call_atom", "wam_compiler:call_atom", "user:test", "user:macro1", "user:macro2", "user:macro3", "user:macro4", "test", "~w: ", "check_test_results", "user:term_expansion", "user:gc", "running_tests", "user:debug", "user:statistics", "user:clause", "user:fail", "user:true", "user:run_test", "user:run_unit_tests", "OK~n", "user:format", "user:check_test_results", "FAIL: Expected ~q, got ~q~n", "failed_test", "user:throw", "user:once_test_1", "macro1", "user:findall", "macro2", "macro3", "macro4", "user:findall_test_1", "b", "d", "findall_test_1", "user:==", "foo", "user:flag", "user:repeat", "user:var", "user:=", "user:arg", "user:subsumes_term", "splungesp", "sp", "spl", "splu", "splun", "splung", "splunge", "splunges", "p", "pl", "plu", "plun", "plung", "plunge", "plunges", "plungesp", "lu", "lun", "lung", "lunge", "lunges", "lungesp", "u", "un", "ung", "unge", "unges", "ungesp", "ng", "nge", "nges", "ngesp", "ge", "ges", "gesp", "e", "es", "esp", "arithmetic_test", "user:is", "type_error", "evaluable", "t", "max", "ok", "user:,", "sin", "pi", "user:<", "user:>", "univ", "bar", "user:=..", "baz", "user:deterministic_goal", "user:nondeterministic_goal", "user:goal_that_fails", "egg", "user:goal_raising_exception", "user:check_value", "mismatch", "deterministic_setup_call_cleanup", "user:get_current_block", "user:mark_top_choicepoint", "user:install_new_block", "user:end_block", "user:reset_block", "user:get_exception", "user:unwind_stack", "user:unmark_top_choicepoint", "user:unmark_choicepoint", "user:halt", "nondeterministic_setup_call_cleanup", "failing_setup_call_cleanup", "unexpected_success", "user:error_setup_call_cleanup_test_1", "error_setup_call_cleanup", "user:clear_exception", "user:nsv1", "user:call", "user:nsv", "nsv1", "user:nsvtop", "nested_structure_vars", "escapes", "\n", "user:atom_codes", "	", "	\nfoo"];
floats = [0.0001, -0.0001];
ftable = [[1,1], [4,2], [6,3], [8,1], [10,1], [12,1], [14,1], [16,1], [18,1], [20,2], [22,2], [24,3], [26,3], [28,2], [30,2], [32,1], [34,2], [36,2], [38,0], [40,2], [42,3], [44,5], [46,2], [48,2], [50,2], [52,2], [54,2], [56,2], [58,2], [60,1], [62,2], [64,2], [66,2], [68,2], [70,2], [72,2], [74,2], [76,2], [78,2], [80,2], [82,2], [84,1], [86,1], [88,1], [90,1], [92,2], [94,2], [96,2], [98,2], [100,2], [102,2], [104,2], [106,2], [108,2], [110,1], [112,1], [114,2], [116,2], [118,1], [120,3], [122,3], [124,0], [126,0], [128,2], [130,1], [132,2], [134,3], [136,2], [138,1], [140,2], [142,3], [144,4], [146,2], [148,3], [150,2], [152,2], [154,1], [156,3], [158,1], [160,3], [162,3], [164,2], [166,0], [168,2], [170,2], [172,2], [174,3], [176,3], [178,4], [180,3], [182,0], [184,0], [186,0], [188,1], [190,1], [192,1], [194,1], [196,1], [198,1], [200,2], [202,2], [204,1], [206,2], [208,2], [210,2], [212,1], [214,1], [216,1], [218,1], [220,1], [222,1], [224,1], [226,1], [228,1], [230,1], [232,1], [234,2], [236,2], [238,1], [240,0], [242,1], [244,1], [246,1], [248,0], [250,1], [252,1], [254,1], [256,0], [258,3], [260,3], [262,3], [264,1], [266,2], [268,0], [270,0], [272,1], [274,1], [274,2], [276,2], [278,3], [280,3], [282,3], [284,3], [286,2], [288,2], [290,2], [292,3], [294,2], [296,2], [298,4], [300,2], [300,3], [302,2], [304,2], [304,3], [306,4], [308,1], [310,3], [310,4], [312,1], [314,1], [316,4], [318,5], [320,2], [322,0], [33,0], [323,1], [324,2], [327,1], [328,0], [329,0], [368,2], [370,2], [372,1], [374,1], [375,1], [376,1], [378,2], [379,2], [380,1], [381,1], [382,1], [383,1], [384,1], [385,1], [386,1], [387,1], [388,1], [389,2], [391,1], [392,1], [393,2], [394,1], [395,1], [396,1], [397,2], [398,1], [400,2], [401,1], [404,0], [408,1], [411,0], [412,1], [413,1], [416,1], [417,2], [418,2], [419,3], [419,4], [421,3], [422,2], [424,3], [427,4], [428,2], [416,2], [418,4], [430,2], [431,3], [433,3], [434,2], [418,3], [418,5], [436,3], [438,1], [431,4], [431,5], [439,2], [440,2], [441,1], [442,1], [443,1], [445,1], [446,1], [447,3], [448,3], [449,3], [450,1], [451,0], [452,2], [453,0], [454,1], [455,1], [456,1], [457,1], [458,1], [459,1], [460,1], [461,1], [462,1], [463,2], [464,3], [464,1], [464,2], [465,2], [466,1], [468,1], [469,1], [470,1], [470,2], [471,1], [471,2], [472,1], [472,2], [473,1], [474,1], [476,4], [477,3], [479,4], [480,3], [480,2], [481,2], [482,3], [384,2], [384,3], [384,4], [384,5], [384,6], [384,7], [384,8], [488,0], [490,0], [492,0], [494,1], [497,1], [499,1], [494,2], [508,4], [509,2], [513,3], [494,3], [508,5], [509,3], [11,1], [513,4], [516,3], [521,2], [522,6], [523,0], [524,0], [524,1], [523,1], [522,5], [534,2], [535,4], [536,5], [538,6], [540,1], [534,3], [544,3], [547,2], [549,5], [552,3], [553,2], [549,4], [555,2], [558,1], [560,1], [574,6], [577,1], [542,3], [581,1], [585,2], [587,2], [589,0], [591,2], [593,2], [595,2], [585,3], [553,3], [606,1], [607,1], [605,1], [608,1], [609,1], [612,1], [613,2], [614,3], [615,3], [616,2], [619,3], [621,2], [622,2], [628,4], [629,1], [631,1], [632,0], [623,1], [635,0], [636,1], [639,2], [640,2], [642,4], [628,5], [643,1], [345,3], [346,3], [423,3], [420,3], [639,3], [646,2], [666,2], [667,2], [668,2], [669,5], [670,2], [671,2], [672,1], [673,1], [674,1], [675,3], [655,2], [663,1], [676,3], [657,2], [656,2], [660,2], [677,2], [71,2], [235,2], [658,5], [678,1], [679,1], [680,2], [681,2], [682,2], [685,6], [686,2], [687,2], [689,2], [691,3], [692,2], [693,1], [694,3], [695,2], [696,3], [698,2], [562,1], [700,3], [563,1], [701,1], [702,2], [703,3], [703,4], [704,1], [705,1], [706,2], [707,2], [707,3], [708,1], [708,2], [682,1], [704,2], [710,3], [137,1], [366,2], [713,1], [715,2], [715,1], [716,2], [717,2], [718,1], [719,2], [720,1], [721,2], [722,1], [723,2], [724,2], [725,2], [726,2], [727,2], [728,1], [729,2], [730,1], [731,2], [732,2], [733,1], [734,1], [735,1], [736,1], [737,1], [738,1], [739,1], [740,1], [743,1], [744,1], [746,2], [754,4], [756,1], [757,2], [760,2], [761,3], [762,2], [764,3], [765,2], [767,1], [770,1], [772,1], [774,2], [776,3], [778,2], [780,2], [781,1], [782,2], [783,3], [783,1], [787,3], [790,3], [324,1], [792,3], [795,1], [796,1], [797,1], [799,1], [802,3], [803,3], [149,3], [805,1], [336,1], [807,3], [809,1], [810,1], [338,1], [335,1], [369,2], [373,1], [811,1], [812,1], [813,1], [815,1], [818,1], [819,2], [820,1], [822,1], [824,1], [826,3], [828,2], [830,3], [832,2], [842,1], [844,1], [845,4], [849,1], [850,2], [851,2], [852,1], [844,2], [769,1], [854,2], [856,2], [857,1], [773,2], [860,2], [861,1], [864,1], [858,2], [866,2], [822,2], [867,1], [775,3], [869,2], [871,3], [872,1], [874,0], [876,4], [876,3], [878,4], [766,1], [888,1], [890,6], [891,1], [892,2], [127,1], [893,3], [894,3], [895,1], [896,7], [897,2], [898,8], [899,4], [901,8], [777,2], [904,2], [905,1], [907,2], [910,1], [911,2], [912,4], [913,1], [403,1], [914,3], [916,4], [917,1], [243,1], [237,2], [245,1], [918,2], [343,2], [247,1], [255,1], [21,2], [239,1], [920,1], [921,2], [922,3], [923,2], [924,2], [925,2], [927,2], [930,3], [931,7], [933,3], [892,3], [936,3], [937,6], [939,3], [940,1], [941,6], [943,5], [9,3], [946,3], [949,3], [951,1], [953,7], [954,7], [957,7], [958,6], [959,6], [961,2], [962,3], [964,4], [966,7], [968,10], [969,8], [972,3], [974,3], [976,4], [978,5], [981,10], [983,10], [986,10], [987,6], [989,3], [991,2], [992,6], [993,2], [994,2], [997,11], [998,1], [999,3], [1001,2], [997,10], [1003,11], [13,1], [1001,4], [1006,2], [1007,2], [1009,1], [1011,2], [1013,1], [1014,1], [1016,1], [768,1], [771,1], [1018,1], [1020,1], [1016,2], [1022,3], [1026,1], [1027,2], [1027,1], [1033,1], [1035,1], [1036,1], [1038,1], [1041,1], [1045,2], [1046,2], [1047,1], [1050,1], [1052,1], [1054,1], [1056,1], [1058,2], [1059,2], [1062,2], [1062,3], [1064,3], [1066,2], [1067,1], [1068,1], [1069,1], [1070,1], [1071,1], [1072,2], [1072,1], [161,2], [1074,2], [1075,2], [1076,0], [1078,0], [1079,0], [1080,2], [1081,0], [1082,0], [1083,1], [1084,0], [1086,2], [1087,2], [1090,1], [1091,0], [1092,1], [1093,3], [1067,2], [1094,1], [1095,1], [1096,1], [1097,1], [1100,1], [1101,2], [1103,3], [1104,0], [81,2], [1105,1], [1106,2], [27,1], [1102,2], [1107,3], [1102,3], [5,1], [1108,2], [569,2], [567,1], [569,1], [45,1], [45,5], [1147,1], [1148,2], [1149,2], [1152,2], [131,1], [1154,2], [1155,1], [1157,2], [1158,2], [1159,1], [1102,1], [1161,2], [1163,0], [1164,0], [1165,0], [1167,0], [1168,2], [1169,2], [1171,1], [1172,2], [1173,1], [1174,2], [1175,1], [1176,1], [1177,0], [1178,0], [1179,1], [1180,0], [1184,0], [1186,0], [1187,2], [1188,1], [1189,1], [1190,2], [344,3], [1191,0], [1195,2]];
dtable = [192, 329, 360, 361, 362, 363, 364, 365, 366, 367, 368, 369, 454, 455, 456, 457, 458, 648];
predicates = {83: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,15,1,4,1,12,375,0,25,1,3,25,1,4,4,176], key:0}}, clause_keys:[0], next_key:1, key:83}, 84: {is_public:false, clauses:{0:{code:[28,1,17,0,0,21,0,1,31,5], key:0}, 1:{code:[30,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,7,6,2,41,24,3,28,2147483707,1,60,0,33,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,5,0,8,1,6,1,3,362,1,32,0,2,4,62,30,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,6,0,8,1,5,1,41,84,2,254,0,1,60,0,60,1,60,2,33,2,16,0,1,0,16,0,0,1,3,201,3,32,2,10,657,0,8,0,1,1,12,270,2,25,0,1,27,1,3,80,2,12,373,0,25,0,0,25,0,1,2,4,176], key:1}}, clause_keys:[0, 1], next_key:2, key:84}, 85: {is_public:false, clauses:{0:{code:[254,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,7,6,2,41,24,3,28,2147483707,1,60,0,33,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,5,0,8,1,6,1,3,361,1,32,0,2,4,62,30,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,6,0,8,1,5,1,41,84,2,254,0,1,60,0,60,1,60,2,33,2,16,0,1,0,16,0,0,1,3,201,3,32,2,10,656,0,8,0,1,1,12,270,2,25,0,1,27,1,3,80,2,12,374,0,25,0,0,25,0,1,2,4,176], key:0}}, clause_keys:[0], next_key:1, key:85}, 86: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,15,1,5,0,16,0,1,1,16,0,2,2,8,1,5,0,8,0,0,1,3,84,3,9,0,0,9,1,1,9,2,2,7,4,3,2,41,48,4,28,2147483740,1,60,0,33,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,12,178,0,25,1,6,25,1,7,8,1,8,1,3,360,1,32,0,2,4,62,30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,8,1,8,0,8,1,6,1,8,1,7,2,41,125,3,254,0,1,60,0,60,1,60,2,60,3,33,3,16,0,2,0,16,0,0,1,16,0,1,2,3,201,4,32,3,10,655,0,8,0,2,1,12,270,2,25,0,2,27,1,3,80,3,12,178,1,25,0,0,25,0,1,12,370,0,24,1,1,25,0,2,2,4,176], key:0}}, clause_keys:[0], next_key:1, key:86}, 87: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,19,178,0,25,0,4,25,0,5,16,0,2,1,16,0,3,2,12,214,0,23,1,6,25,0,6,12,375,9,23,1,7,25,0,6,12,376,11,24,1,7,27,2147483647,12,377,10,24,1,6,24,1,11,12,188,8,24,1,9,24,1,10,12,219,1,24,1,7,24,1,8,8,0,7,2,3,233,8,9,6,0,12,214,3,23,1,4,25,0,6,12,378,1,24,1,3,25,0,7,8,0,1,2,3,207,8,9,4,0,9,5,1,8,0,0,2,3,86,6,9,0,0,7,14,1,7,15,2,7,16,3,7,17,4,7,18,5,9,1,6,9,2,7,9,3,8,7,19,9,7,20,10,7,21,11,7,22,12,7,23,13,2,41,183,14,28,2147484035,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,33,9,16,0,0,0,16,0,5,1,16,0,6,2,16,0,7,3,16,0,8,4,16,0,1,5,16,0,2,6,16,0,3,7,16,0,4,8,15,1,16,9,15,1,17,10,15,1,18,11,15,1,19,12,15,1,20,13,13,22,27,0,25,0,6,13,21,27,254,24,1,22,12,379,0,25,0,0,25,0,5,24,1,21,25,0,7,25,0,8,3,232,10,32,9,8,0,1,0,12,270,1,25,0,5,27,1,3,34,9,13,2,25,0,1,25,0,6,13,1,27,28,24,1,2,12,379,0,25,0,0,25,0,5,24,1,1,25,0,7,25,0,8,3,175,9,13,2,27,0,25,0,2,13,1,27,30,24,1,2,12,379,0,25,0,0,25,0,1,24,1,1,25,0,3,25,0,4,2,4,175,30,0,15,1,16,0,15,1,17,1,15,1,18,2,15,1,19,3,15,1,20,4,15,1,21,5,15,1,22,6,15,1,23,7,15,1,24,8,15,1,25,9,15,1,26,10,15,1,27,11,15,1,28,12,15,1,29,13,8,1,16,0,8,1,17,1,8,1,18,2,8,1,19,3,8,1,20,4,8,1,21,5,8,1,22,6,8,1,23,7,8,1,24,8,8,1,26,9,8,1,27,10,8,1,28,11,8,1,29,12,41,500,13,28,2147484348,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,33,9,16,0,0,0,16,0,5,1,16,0,6,2,16,0,7,3,16,0,8,4,16,0,1,5,16,0,2,6,16,0,3,7,16,0,4,8,15,1,15,9,15,1,16,10,15,1,17,11,15,1,18,12,13,20,27,0,25,0,6,13,19,27,30,24,1,20,12,379,0,25,0,0,25,0,5,24,1,19,25,0,7,25,0,8,3,232,10,32,9,8,0,1,0,12,270,1,25,0,5,27,1,3,34,9,13,2,25,0,1,25,0,6,13,1,27,29,24,1,2,12,379,0,25,0,0,25,0,5,24,1,1,25,0,7,25,0,8,3,175,9,13,2,27,0,25,0,2,13,1,27,30,24,1,2,12,379,0,25,0,0,25,0,1,24,1,1,25,0,3,25,0,4,2,4,175,30,0,15,1,15,0,15,1,16,1,15,1,17,2,15,1,18,3,15,1,19,4,15,1,20,5,15,1,21,6,15,1,22,7,15,1,23,8,15,1,24,9,15,1,25,10,15,1,26,11,15,1,27,12,8,1,15,0,8,1,25,1,8,1,26,2,8,1,21,3,8,1,22,4,8,1,23,5,8,1,27,6,41,785,7,28,2147484531,1,60,0,60,1,60,2,60,3,60,4,33,4,16,0,0,0,15,1,9,1,15,1,10,2,16,0,1,3,16,0,2,4,16,0,3,5,15,1,11,6,12,379,0,25,0,0,27,0,26,0,25,1,9,25,1,10,3,232,5,32,4,13,2,27,0,25,0,1,13,1,27,254,24,1,2,12,379,0,25,0,0,27,0,24,1,1,25,0,2,25,0,3,2,4,175,30,0,15,1,9,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,15,1,14,5,15,1,15,6,8,1,9,0,8,1,12,1,8,1,13,2,8,1,14,3,41,932,4,254,0,1,60,0,60,1,60,2,60,3,60,4,33,4,16,0,0,0,16,0,1,1,16,0,2,2,16,0,3,3,3,201,5,32,4,13,2,27,0,25,0,1,13,1,27,254,24,1,2,12,379,0,25,0,0,27,0,24,1,1,25,0,2,25,0,3,2,4,175], key:0}}, clause_keys:[0], next_key:1, key:87}, 88: {is_public:false, clauses:{0:{code:[254,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,8,1,6,0,7,10,1,7,11,2,8,1,7,3,8,1,8,4,8,1,9,5,7,12,6,41,46,7,28,2147483795,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,5,16,0,6,0,16,0,2,1,16,0,3,2,16,0,4,3,16,0,0,4,16,0,1,5,15,1,9,6,8,0,6,0,3,380,7,9,6,0,12,381,1,25,0,2,3,9,7,32,5,8,0,3,0,12,382,1,25,0,4,27,2147483648,3,34,5,9,2,0,9,3,1,3,383,4,9,0,0,9,1,1,2,4,9,30,0,15,1,9,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,15,1,14,5,15,1,15,6,8,1,11,0,8,1,12,1,8,1,9,2,8,1,13,3,8,1,14,4,41,200,5,254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,33,5,16,0,4,0,16,0,2,1,16,0,1,2,16,0,0,3,16,0,3,4,3,201,6,32,5,8,0,4,0,12,270,1,25,0,2,27,1,3,34,5,12,375,0,25,0,2,27,254,3,176,5,12,375,0,25,0,4,27,0,3,176,5,8,0,1,0,12,381,1,25,0,2,3,9,4,9,0,0,12,384,2,25,0,1,25,0,2,13,1,24,1,2,25,0,3,2,4,9], key:0}}, clause_keys:[0], next_key:1, key:88}, 170: {is_public:false, clauses:{0:{code:[254,0,1,60,0,33,0,10,322,0,3,166,1,12,167,0,26,325,26,326,3,168,1,32,0,2,4,169], key:0}}, clause_keys:[0], next_key:1, key:170}, 172: {is_public:false, clauses:{0:{code:[254,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,4,171], key:0}}, clause_keys:[0], next_key:1, key:172}, 174: {is_public:false, clauses:{0:{code:[254,0,15,1,2,0,8,1,2,0,4,173], key:0}}, clause_keys:[0], next_key:1, key:174}, 175: {is_public:false, clauses:{0:{code:[254,0,1,60,0,16,0,0,0,8,0,0,0,3,185,1,9,0,0,2,4,231], key:0}}, clause_keys:[0], next_key:1, key:175}, 176: {is_public:false, clauses:{0:{code:[254,0,15,1,2,0,8,1,2,0,4,175], key:0}}, clause_keys:[0], next_key:1, key:176}, 179: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,19,167,0,25,0,2,25,0,3,31,8,0,2,0,8,0,4,1,8,0,1,2,3,11,5,9,4,0,8,0,0,1,3,177,5,12,178,0,25,0,0,25,0,1,9,2,1,9,3,2,2,4,89], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,16,0,2,0,31,8,0,2,0,8,0,3,1,8,0,1,2,3,11,4,9,3,0,8,0,0,1,3,177,4,12,178,0,25,0,0,25,0,1,9,2,1,10,127,2,2,4,89], key:1}}, clause_keys:[0, 1], next_key:2, key:179}, 180: {is_public:false, clauses:{0:{code:[254,0,15,1,1,0,5], key:0}}, clause_keys:[0], next_key:1, key:180}, 181: {is_public:false, clauses:{0:{code:[254,0,15,1,1,0,5], key:0}}, clause_keys:[0], next_key:1, key:181}, 183: {is_public:false, clauses:{0:{code:[28,1,20,0,23,1,2,23,1,3,13,4,24,1,2,24,1,3,12,182,0,24,1,4,4,183], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,33,1,16,0,2,0,8,0,2,0,8,0,0,1,3,63,3,12,184,1,25,0,0,12,167,0,24,1,1,25,0,2,3,185,3,32,1,9,0,0,3,93,1,2,4,62], key:1}}, clause_keys:[0, 1], next_key:2, key:183}, 185: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,19,527,0,25,0,9,31,8,0,9,0,10,889,1,8,0,12,2,8,0,1,3,10,0,4,8,0,11,5,3,528,13,8,0,11,0,3,529,13,9,12,0,8,0,4,1,3,530,13,12,531,0,25,0,11,8,0,4,1,8,0,5,2,3,532,12,8,0,5,0,8,0,4,1,8,0,6,2,3,533,12,8,0,6,0,9,11,1,8,0,5,2,8,0,0,3,8,0,8,4,12,534,7,25,0,10,13,5,24,1,7,26,0,8,0,7,6,3,535,12,9,9,0,9,10,1,3,536,11,8,0,2,0,3,499,9,9,4,0,8,0,2,1,9,5,2,9,6,3,9,7,4,7,8,5,9,8,6,8,0,3,7,3,537,9,9,1,0,9,2,1,9,3,2,10,0,3,3,538,4,3,82,1,9,0,0,14,2,1,2,4,388], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,33,0,19,167,0,25,0,1,25,0,2,31,8,0,2,0,10,889,1,8,0,14,2,8,0,4,3,10,0,4,8,0,13,5,3,528,15,8,0,13,0,3,529,15,9,14,0,8,0,7,1,3,530,15,12,214,0,25,0,1,25,0,13,8,0,7,1,8,0,8,2,3,532,14,8,0,8,0,8,0,7,1,8,0,9,2,3,533,14,8,0,2,0,8,0,12,1,3,536,14,8,0,1,0,9,12,1,9,13,2,8,0,8,3,8,0,9,4,8,0,10,5,8,0,3,6,8,0,11,7,3,539,14,8,0,5,0,3,499,12,9,7,0,8,0,5,1,9,8,2,9,9,3,9,10,4,7,8,5,9,11,6,8,0,6,7,3,537,12,9,4,0,9,5,1,9,6,2,10,0,3,3,538,7,3,82,4,32,0,9,3,0,14,2,1,3,388,4,8,0,0,0,9,1,1,7,5,2,7,6,3,9,2,4,2,41,251,5,28,2147483960,1,60,0,60,1,60,2,33,2,15,1,7,0,16,0,0,1,15,1,8,2,15,1,9,3,16,0,1,4,8,0,0,0,12,540,1,25,1,8,25,1,9,3,9,3,32,2,12,167,0,25,0,0,25,0,1,2,4,180,30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,15,1,10,4,4,62], key:1}, 2:{code:[30,0,1,60,0,60,1,16,0,0,0,8,0,0,0,14,0,1,10,902,2,10,0,3,10,529,4,7,9,5,8,0,1,6,13,7,26,714,26,0,3,539,2,3,82,2,9,1,0,14,2,1,3,388,2,9,0,0,7,3,1,7,4,2,2,41,68,3,28,2147483761,1,60,0,60,1,33,1,16,0,0,0,15,1,5,1,15,1,6,2,8,0,0,0,12,540,1,25,1,5,25,1,6,3,9,2,32,1,9,0,0,2,4,180,30,0,15,1,4,0,15,1,5,1,15,1,6,2,4,62], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:185}, 187: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,19,178,0,25,0,0,25,0,1,31,12,178,0,25,0,0,25,0,1,2,4,186], key:0}, 1:{code:[29,2,17,0,0,5], key:1}, 2:{code:[29,3,1,60,0,60,1,20,0,25,0,1,25,0,0,31,9,1,0,3,187,2,9,0,0,2,4,187], key:2}, 3:{code:[30,0,1,60,0,19,188,0,23,1,2,25,0,0,8,1,2,0,3,187,1,9,0,0,2,4,187], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:187}, 190: {is_public:false, clauses:{0:{code:[254,0,15,1,2,0,8,1,2,0,4,189], key:0}}, clause_keys:[0], next_key:1, key:190}, 191: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,16,0,3,0,16,0,1,1,8,0,3,0,8,0,2,1,3,24,4,7,5,0,9,2,1,8,0,0,2,9,3,3,7,6,4,40,59,5,4,9,0,0,9,1,1,2,4,338,28,2147483780,1,60,0,60,1,60,2,33,2,15,1,7,0,15,1,8,1,16,0,0,2,16,0,1,3,15,1,9,4,8,1,7,0,13,11,27,108,26,0,13,10,27,112,24,1,11,13,1,27,46,24,1,10,8,1,8,2,3,266,3,32,2,9,0,0,9,1,1,2,4,9,30,0,1,60,0,60,1,15,1,6,0,15,1,7,1,16,0,0,2,15,1,8,3,16,0,1,4,8,1,7,0,13,10,27,108,26,0,13,9,27,112,24,1,10,13,1,27,46,24,1,9,8,0,1,2,3,266,2,9,0,0,9,1,1,2,4,24], key:0}}, clause_keys:[0], next_key:1, key:191}, 192: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:192}, 193: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,15,1,3,0,8,1,3,0,8,0,1,1,3,635,2,9,1,0,8,0,0,1,3,636,2,9,0,0,2,4,637], key:0}}, clause_keys:[0], next_key:1, key:193}, 194: {is_public:false, clauses:{0:{code:[254,0,1,60,0,15,1,3,0,8,1,3,0,8,0,0,1,3,191,1,9,0,0,2,41,27,1,28,2147483698,1,60,0,33,0,15,1,3,0,8,1,3,0,3,192,1,32,0,2,4,62,30,0,15,1,2,0,13,0,25,1,2,26,0,4,193], key:0}}, clause_keys:[0], next_key:1, key:194}, 195: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,1,0,16,0,2,1,8,0,0,0,3,43,3,9,0,0,9,1,1,9,2,2,2,4,79], key:0}}, clause_keys:[0], next_key:1, key:195}, 196: {is_public:false, clauses:{0:{code:[28,1,15,1,1,0,5], key:0}, 1:{code:[30,0,15,1,1,0,5], key:1}}, clause_keys:[0, 1], next_key:2, key:196}, 197: {is_public:false, clauses:{0:{code:[28,1,1,60,0,16,0,0,0,15,1,3,1,8,1,3,0,3,124,1,9,0,0,2,4,122], key:0}, 1:{code:[30,0,1,15,1,3,0,15,1,4,1,8,1,4,0,3,122,0,2,4,61], key:1}}, clause_keys:[0, 1], next_key:2, key:197}, 199: {is_public:false, clauses:{0:{code:[254,0,14,0,0,4,15], key:0}}, clause_keys:[0], next_key:1, key:199}, 201: {is_public:false, clauses:{0:{code:[254,0,5], key:0}}, clause_keys:[0], next_key:1, key:201}, 202: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,1,0,10,399,0,13,1,25,0,1,26,0,3,195,3,8,0,2,0,3,120,3,9,2,0,8,0,1,1,8,0,0,2,7,4,3,40,64,4,3,9,0,0,9,1,1,7,3,2,2,41,836,3,28,2147483830,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,16,0,6,0,16,0,4,1,16,0,3,2,16,0,5,3,13,6,25,0,5,26,0,13,5,25,0,4,24,1,6,13,0,25,0,3,24,1,5,8,0,2,1,3,117,7,40,436,0,7,8,0,0,0,3,121,7,8,0,4,0,40,336,1,7,43,0,1,9,6,0,8,0,0,1,3,197,7,9,0,0,8,0,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,41,348,6,29,2147483915,1,60,0,60,1,60,2,60,3,60,4,33,4,15,1,6,0,16,0,1,1,16,0,0,2,16,0,2,3,8,1,6,0,3,122,5,8,0,3,0,3,126,5,32,4,8,0,0,0,12,198,1,25,0,3,3,9,4,9,0,0,9,1,1,7,6,2,7,7,3,7,8,4,9,2,5,40,492,6,3,2,4,123,30,0,1,60,0,60,1,60,2,60,3,33,0,15,1,6,0,16,0,2,1,16,0,1,2,16,0,3,3,8,0,1,0,10,125,1,3,9,4,3,119,4,9,1,0,9,2,1,7,6,2,7,7,3,7,8,4,9,3,5,40,492,6,4,32,0,2,4,61,254,0,15,1,2,0,8,1,2,0,4,183,28,2147484080,1,60,0,60,1,60,2,60,3,15,1,7,0,15,1,8,1,16,0,3,2,16,0,0,3,16,0,1,4,16,0,2,5,8,1,7,0,8,1,8,1,3,9,4,8,0,0,0,10,402,1,3,9,4,9,3,0,3,118,4,9,0,0,9,1,1,7,6,2,7,7,3,7,8,4,9,2,5,2,41,492,6,30,0,4,62,254,0,1,60,0,60,1,60,2,16,0,0,0,16,0,1,1,16,0,2,2,8,0,0,0,10,403,1,3,9,3,9,0,0,9,1,1,7,6,2,7,7,3,7,8,4,9,2,5,40,492,6,3,2,4,199,28,2147484198,1,60,0,60,1,33,1,15,1,8,0,16,0,0,1,15,1,9,2,15,1,10,3,15,1,11,4,15,1,12,5,8,1,8,0,10,125,1,3,10,2,32,1,10,405,0,13,1,25,0,0,26,0,2,4,195,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,8,0,8,1,9,1,8,1,11,2,8,1,12,3,8,1,13,4,41,599,5,28,2147484301,1,60,0,60,1,33,1,15,1,7,0,16,0,0,1,15,1,8,2,15,1,9,3,15,1,10,4,8,1,7,0,10,402,1,3,10,2,32,1,10,406,0,13,1,25,0,0,26,0,2,4,195,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,7,0,8,1,8,1,8,1,10,2,8,1,11,3,41,694,4,28,2147484392,1,60,0,60,1,33,1,15,1,6,0,16,0,0,1,15,1,7,2,15,1,8,3,8,1,6,0,10,403,1,3,10,2,32,1,10,407,0,13,1,25,0,0,26,0,2,4,195,30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,8,1,6,0,8,1,9,1,8,1,7,2,41,777,3,254,0,1,60,0,60,1,60,2,33,2,15,1,5,0,16,0,1,1,16,0,0,2,8,1,5,0,12,200,1,25,0,1,3,9,3,32,2,10,409,0,13,2,25,0,1,26,0,13,1,25,0,0,24,1,2,2,4,195,28,2147484527,1,60,0,60,1,33,1,15,1,5,0,16,0,0,1,15,1,6,2,8,1,5,0,3,3,2,32,1,10,410,0,13,1,25,0,0,26,0,2,4,195,30,0,15,1,5,0,15,1,6,1,15,1,7,2,41,896,0,254,0,1,60,0,33,0,3,201,1,32,0,2,4,62], key:0}}, clause_keys:[0], next_key:1, key:202}, 203: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,16,0,4,0,8,0,4,0,8,0,1,1,8,0,2,2,3,11,5,10,399,0,12,178,2,25,0,1,25,0,2,13,1,24,1,2,26,0,3,195,5,8,0,3,0,3,120,5,9,3,0,9,4,1,8,0,0,2,8,0,1,3,8,0,2,4,7,6,5,40,102,6,5,9,0,0,9,1,1,9,2,2,7,4,3,2,41,1061,4,28,2147483902,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,16,0,8,0,16,0,4,1,16,0,3,2,16,0,5,3,16,0,6,4,16,0,7,5,13,10,25,0,7,26,0,13,9,25,0,6,24,1,10,13,8,25,0,5,24,1,9,13,7,25,0,4,24,1,8,13,0,25,0,3,24,1,7,8,0,2,1,3,117,9,40,562,0,9,8,0,0,0,3,121,9,8,0,4,0,40,444,1,9,43,0,1,9,8,0,8,0,0,1,3,197,9,9,0,0,8,0,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,2,41,456,8,29,2147484005,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,15,1,8,0,16,0,1,1,16,0,0,2,16,0,2,3,16,0,3,4,16,0,4,5,8,1,8,0,3,122,7,8,0,5,0,3,126,7,32,6,8,0,0,0,12,198,1,25,0,5,3,9,6,9,0,0,9,1,1,7,8,2,9,2,3,9,3,4,7,9,5,7,10,6,9,4,7,40,636,8,5,2,4,123,30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,33,0,15,1,8,0,16,0,2,1,16,0,1,2,16,0,3,3,16,0,4,4,16,0,5,5,8,0,1,0,10,125,1,3,9,6,3,119,6,9,1,0,9,2,1,7,8,2,9,3,3,9,4,4,7,9,5,7,10,6,9,5,7,40,636,8,6,32,0,2,4,61,254,0,15,1,2,0,8,1,2,0,4,183,28,2147484206,1,60,0,60,1,60,2,60,3,60,4,60,5,15,1,9,0,15,1,10,1,16,0,5,2,16,0,0,3,16,0,1,4,16,0,2,5,16,0,3,6,16,0,4,7,8,1,9,0,8,1,10,1,3,9,6,8,0,0,0,10,402,1,3,9,6,9,5,0,3,118,6,9,0,0,9,1,1,7,8,2,9,2,3,9,3,4,7,9,5,7,10,6,9,4,7,2,41,636,8,30,0,4,62,254,0,1,60,0,60,1,60,2,60,3,60,4,16,0,0,0,16,0,1,1,16,0,2,2,16,0,3,3,16,0,4,4,8,0,0,0,10,403,1,3,9,5,9,0,0,9,1,1,7,8,2,9,2,3,9,3,4,7,9,5,7,10,6,9,4,7,40,636,8,5,2,4,199,28,2147484350,1,60,0,60,1,33,1,15,1,10,0,16,0,0,1,15,1,11,2,15,1,12,3,15,1,13,4,15,1,14,5,15,1,15,6,15,1,16,7,8,1,10,0,10,125,1,3,10,2,32,1,10,405,0,13,1,25,0,0,26,0,2,4,195,30,0,15,1,10,0,15,1,11,1,15,1,12,2,15,1,13,3,15,1,14,4,15,1,15,5,15,1,16,6,15,1,17,7,8,1,10,0,8,1,13,1,8,1,14,2,8,1,15,3,8,1,16,4,8,1,17,5,41,763,6,28,2147484480,1,60,0,60,1,60,2,33,2,15,1,8,0,16,0,0,1,16,0,1,2,15,1,9,3,15,1,10,4,15,1,11,5,8,1,8,0,10,402,1,3,10,3,32,2,10,406,0,12,178,2,25,0,0,25,0,1,13,1,24,1,2,26,0,2,4,195,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,8,0,8,1,9,1,8,1,10,2,8,1,12,3,8,1,13,4,41,881,5,28,2147484594,1,60,0,60,1,60,2,33,2,15,1,7,0,16,0,0,1,16,0,1,2,15,1,8,3,15,1,9,4,8,1,7,0,10,403,1,3,10,3,32,2,10,407,0,12,178,2,25,0,0,25,0,1,13,1,24,1,2,26,0,2,4,195,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,7,0,8,1,11,1,8,1,8,2,8,1,9,3,41,987,4,254,0,1,60,0,60,1,60,2,60,3,33,3,15,1,6,0,16,0,2,1,16,0,0,2,16,0,1,3,8,1,6,0,12,200,1,25,0,2,3,9,4,32,3,10,409,0,12,178,2,25,0,0,25,0,1,13,3,25,0,2,26,0,13,1,24,1,2,24,1,3,2,4,195,28,2147484767,1,60,0,60,1,60,2,33,2,15,1,6,0,16,0,0,1,16,0,1,2,15,1,7,3,8,1,6,0,3,3,3,32,2,10,410,0,12,178,2,25,0,0,25,0,1,13,1,24,1,2,26,0,2,4,195,30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,41,1140,0,254,0,1,60,0,33,0,3,201,1,32,0,2,4,62], key:0}}, clause_keys:[0], next_key:1, key:203}, 205: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,16,0,0,0,15,1,5,1,8,1,5,0,7,6,1,8,0,1,2,3,218,2,10,429,0,10,416,1,7,3,2,3,128,2,9,1,0,3,183,2,10,429,0,9,0,1,7,3,2,3,128,1,2,4,61], key:0}, 1:{code:[30,0,15,1,2,0,15,1,3,1,5], key:1}}, clause_keys:[0, 1], next_key:2, key:205}, 206: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,33,3,16,0,1,0,16,0,2,1,10,429,0,8,0,0,1,8,0,4,2,3,130,5,9,4,0,3,131,5,32,3,9,0,0,9,1,1,9,2,2,2,4,220], key:0}}, clause_keys:[0], next_key:1, key:206}, 207: {is_public:false, clauses:{0:{code:[254,0,1,60,0,15,1,5,0,15,1,6,1,16,0,0,2,12,204,0,25,1,5,8,1,6,1,3,205,1,10,0,0,9,0,1,2,4,206], key:0}}, clause_keys:[0], next_key:1, key:207}, 208: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,15,1,6,0,15,1,7,1,16,0,0,2,16,0,1,3,12,204,0,25,1,6,8,1,7,1,3,205,2,9,0,0,9,1,1,2,4,206], key:0}}, clause_keys:[0], next_key:1, key:208}, 209: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,33,4,16,0,7,0,16,0,8,1,16,0,0,2,8,0,8,0,8,0,7,1,10,0,2,8,0,9,3,3,212,10,8,0,9,0,10,0,1,3,213,10,32,4,8,0,3,0,13,1,26,429,25,0,9,3,13,10,8,0,3,0,10,429,1,8,0,6,2,3,11,9,12,214,0,25,0,3,25,0,7,9,8,1,3,205,9,8,0,3,0,9,6,1,10,0,2,8,0,5,3,3,215,7,9,5,0,8,0,2,1,3,216,6,32,4,9,2,0,9,3,1,8,0,1,2,3,217,4,9,0,0,9,1,1,2,4,9], key:0}, 1:{code:[30,0,1,60,0,15,1,5,0,15,1,6,1,16,0,0,2,12,204,0,25,1,5,8,1,6,1,3,205,1,10,0,0,8,0,0,1,3,206,1,9,0,0,10,0,1,2,4,213], key:1}}, clause_keys:[0, 1], next_key:2, key:209}, 210: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,20,0,25,0,1,23,1,6,16,0,3,1,8,1,6,0,8,0,1,1,8,0,5,2,8,0,4,3,3,265,6,9,5,0,8,0,0,1,3,210,6,9,4,0,8,0,2,1,3,210,5,9,0,0,13,1,25,0,1,25,0,2,9,3,2,2,4,266], key:0}, 1:{code:[30,0,17,0,0,17,0,1,5], key:1}}, clause_keys:[0, 1], next_key:2, key:210}, 211: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,15,1,6,0,15,1,7,1,16,0,1,2,8,1,6,0,8,1,7,1,8,0,0,2,3,209,2,9,0,0,9,1,1,2,4,210], key:0}}, clause_keys:[0], next_key:1, key:211}, 213: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,15,1,4,1,8,1,3,0,8,1,4,1,41,21,2,28,2147483700,1,60,0,33,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,3,10,1,32,0,2,4,61,30,0,15,1,3,0,15,1,4,1,4,62], key:0}}, clause_keys:[0], next_key:1, key:213}, 215: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,5,16,0,1,0,16,0,2,1,16,0,3,2,16,0,4,3,10,429,0,8,0,0,1,8,0,6,2,3,130,7,9,6,0,3,131,7,32,5,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,221], key:0}}, clause_keys:[0], next_key:1, key:215}, 216: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,20,0,23,1,6,23,1,7,19,214,6,25,0,1,25,0,2,16,0,4,1,8,1,7,0,8,0,1,1,8,0,6,2,8,0,5,3,3,267,7,9,6,0,8,0,0,1,3,216,7,9,5,0,8,0,3,1,3,216,6,9,0,0,12,214,3,25,0,1,25,0,2,13,1,24,1,3,25,0,3,9,4,2,2,4,266], key:0}, 1:{code:[30,0,17,0,0,17,0,1,5], key:1}}, clause_keys:[0, 1], next_key:2, key:216}, 217: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,20,0,23,1,7,23,1,8,19,214,7,25,0,1,25,0,2,16,0,4,1,16,0,5,2,8,1,8,0,8,0,1,1,8,0,3,2,8,0,0,3,3,224,6,9,0,0,9,1,1,13,2,25,0,2,25,0,3,9,4,3,9,5,4,2,4,225], key:0}}, clause_keys:[0], next_key:1, key:217}, 218: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,19,219,0,23,1,3,25,0,0,20,1,24,1,3,25,0,1,16,0,2,2,31,9,0,0,9,1,1,9,2,2,2,4,218], key:0}, 1:{code:[30,0,15,1,3,0,17,0,1,16,1,3,2,5], key:1}}, clause_keys:[0, 1], next_key:2, key:218}, 220: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,17,416,0,16,0,1,1,16,0,0,2,31,9,0,0,9,1,1,2,4,9], key:0}, 1:{code:[30,0,19,204,0,23,1,5,15,1,6,1,15,1,7,2,13,0,24,1,5,25,1,6,8,1,7,1,4,206], key:1}}, clause_keys:[0, 1], next_key:2, key:220}, 221: {is_public:false, clauses:{0:{code:[28,1,17,416,0,15,1,5,1,15,1,6,2,15,1,7,3,16,1,7,4,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,19,214,0,25,0,2,25,0,3,16,0,0,1,16,0,1,2,16,0,4,3,16,0,5,4,8,0,1,0,8,0,0,1,8,0,2,2,3,222,7,32,6,9,0,0,9,1,1,12,214,4,25,0,2,25,0,3,13,2,24,1,4,25,0,4,9,5,3,2,4,215], key:1}}, clause_keys:[0, 1], next_key:2, key:221}, 222: {is_public:false, clauses:{0:{code:[28,1,21,0,0,15,1,3,1,15,1,4,2,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,33,4,16,0,3,0,16,0,1,1,16,0,2,2,8,0,3,0,8,0,2,1,8,0,5,2,3,12,6,9,5,0,3,223,6,32,4,8,0,0,0,12,214,1,25,0,3,27,1,3,34,4,9,0,0,9,1,1,9,2,2,2,4,222], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,16,0,3,0,16,0,1,1,16,0,2,2,8,0,3,0,8,0,1,1,8,0,4,2,3,12,5,8,0,3,0,8,0,2,1,9,4,2,3,12,5,8,0,0,0,12,214,1,25,0,3,27,1,3,34,4,9,0,0,9,1,1,9,2,2,2,4,222], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:222}, 223: {is_public:false, clauses:{0:{code:[254,0,15,1,2,0,8,1,2,0,41,13,1,28,2147483684,1,60,0,33,0,15,1,3,0,8,1,3,0,3,3,1,32,0,2,4,61,30,0,15,1,2,0,4,62], key:0}}, clause_keys:[0], next_key:1, key:223}, 224: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,33,4,20,0,23,1,6,25,0,0,19,214,6,23,1,7,23,1,8,16,0,1,1,20,2,24,1,8,25,0,2,16,0,3,3,8,1,7,0,8,0,1,1,3,10,5,32,4,9,0,0,9,1,1,9,2,2,9,3,3,2,4,224], key:0}, 1:{code:[30,0,15,1,4,0,15,1,5,1,17,0,2,16,1,4,3,5], key:1}}, clause_keys:[0, 1], next_key:2, key:224}, 225: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,5,1,15,1,6,2,16,1,5,3,16,1,6,4,31,5], key:0}, 1:{code:[29,2,15,1,5,0,15,1,6,1,15,1,7,2,16,1,6,3,16,1,7,4,5], key:1}, 2:{code:[30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,8,1,8,0,8,1,11,1,8,1,12,2,4,217], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:225}, 226: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,16,1,3,1,8,1,3,0,4,0], key:0}}, clause_keys:[0], next_key:1, key:226}, 227: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,15,1,4,1,8,1,3,0,8,1,4,1,41,21,2,28,2147483700,1,60,0,33,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,3,9,1,32,0,2,4,61,30,0,15,1,3,0,15,1,4,1,4,62], key:0}}, clause_keys:[0], next_key:1, key:227}, 228: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,8,1,3,0,41,13,1,28,2147483684,1,60,0,33,0,15,1,3,0,8,1,3,0,3,5,1,32,0,2,4,62,30,0,15,1,2,0,8,1,2,0,4,6], key:0}}, clause_keys:[0], next_key:1, key:228}, 229: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,8,1,3,0,41,13,1,28,2147483684,1,60,0,33,0,15,1,3,0,8,1,3,0,3,4,1,32,0,2,4,62,30,0,15,1,2,0,8,1,2,0,4,228], key:0}}, clause_keys:[0], next_key:1, key:229}, 230: {is_public:false, clauses:{0:{code:[254,0,1,60,0,16,0,0,0,8,0,0,0,3,185,1,9,0,0,2,4,179], key:0}}, clause_keys:[0], next_key:1, key:230}, 232: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,19,167,0,25,0,0,25,0,1,31,9,0,0,9,1,1,2,4,69], key:0}, 1:{code:[30,0,1,60,0,16,0,0,0,31,9,0,0,10,127,1,2,4,69], key:1}}, clause_keys:[0, 1], next_key:2, key:232}, 233: {is_public:false, clauses:{0:{code:[254,0,15,1,6,0,15,1,7,1,15,1,8,2,8,1,6,0,8,1,7,1,8,1,8,2,4,211], key:0}}, clause_keys:[0], next_key:1, key:233}, 234: {is_public:false, clauses:{0:{code:[254,0,15,1,6,0,15,1,7,1,15,1,8,2,8,1,6,0,8,1,7,1,8,1,8,2,4,209], key:0}}, clause_keys:[0], next_key:1, key:234}, 235: {is_public:false, clauses:{0:{code:[254,0,15,1,7,0,15,1,8,1,15,1,9,2,8,1,7,0,8,1,8,1,8,1,9,2,10,0,3,4,71], key:0}}, clause_keys:[0], next_key:1, key:235}, 236: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,8,1,3,0,10,0,1,4,72], key:0}}, clause_keys:[0], next_key:1, key:236}, 237: {is_public:false, clauses:{0:{code:[254,0,1,60,0,8,0,0,0,3,43,1,9,0,0,2,4,54], key:0}}, clause_keys:[0], next_key:1, key:237}, 238: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,33,2,16,0,0,0,16,0,1,1,8,0,0,0,3,3,3,32,2,8,0,0,0,3,58,2,9,0,0,9,1,1,2,4,57], key:0}, 1:{code:[30,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,4,57], key:1}}, clause_keys:[0, 1], next_key:2, key:238}, 239: {is_public:false, clauses:{0:{code:[254,0,1,60,0,8,0,0,0,3,43,1,9,0,0,2,4,55], key:0}}, clause_keys:[0], next_key:1, key:239}, 240: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,44,2,9,0,0,9,1,1,2,4,45], key:0}}, clause_keys:[0], next_key:1, key:240}, 241: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,44,2,9,0,0,9,1,1,2,4,46], key:0}}, clause_keys:[0], next_key:1, key:241}, 242: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,44,2,9,0,0,9,1,1,2,4,47], key:0}}, clause_keys:[0], next_key:1, key:242}, 243: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,44,2,9,0,0,9,1,1,2,4,48], key:0}}, clause_keys:[0], next_key:1, key:243}, 244: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,43,2,9,0,0,9,1,1,2,4,49], key:0}}, clause_keys:[0], next_key:1, key:244}, 245: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,43,2,9,0,0,9,1,1,2,4,50], key:0}}, clause_keys:[0], next_key:1, key:245}, 246: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,44,2,9,0,0,9,1,1,2,4,51], key:0}}, clause_keys:[0], next_key:1, key:246}, 247: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,44,2,9,0,0,9,1,1,2,4,52], key:0}}, clause_keys:[0], next_key:1, key:247}, 248: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,43,2,9,0,0,9,1,1,2,4,53], key:0}}, clause_keys:[0], next_key:1, key:248}, 249: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,1,0,16,0,2,1,8,0,0,0,3,44,3,9,0,0,9,1,1,9,2,2,2,4,70], key:0}}, clause_keys:[0], next_key:1, key:249}, 251: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,44,2,9,0,0,9,1,1,10,0,2,2,4,250], key:0}}, clause_keys:[0], next_key:1, key:251}, 252: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,8,1,5,0,8,1,6,1,10,0,2,4,70], key:0}}, clause_keys:[0], next_key:1, key:252}, 253: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,1,0,16,0,2,1,8,0,0,0,3,43,3,9,0,0,9,1,1,9,2,2,2,4,59], key:0}}, clause_keys:[0], next_key:1, key:253}, 257: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,43,2,9,0,0,9,1,1,12,254,3,26,467,12,255,5,26,467,12,256,7,26,127,13,6,24,1,7,26,0,13,4,24,1,5,24,1,6,13,2,24,1,3,24,1,4,2,4,59], key:0}}, clause_keys:[0], next_key:1, key:257}, 258: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,8,1,5,0,8,1,6,1,12,254,7,26,467,12,255,9,26,467,12,256,11,26,127,13,10,24,1,11,26,0,13,8,24,1,9,24,1,10,13,2,24,1,7,24,1,8,4,59], key:0}}, clause_keys:[0], next_key:1, key:258}, 259: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,43,2,9,0,0,9,1,1,12,254,3,26,127,12,255,5,26,467,12,256,7,26,127,13,6,24,1,7,26,0,13,4,24,1,5,24,1,6,13,2,24,1,3,24,1,4,2,4,59], key:0}}, clause_keys:[0], next_key:1, key:259}, 260: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,8,1,5,0,8,1,6,1,12,254,7,26,127,12,255,9,26,467,12,256,11,26,127,13,10,24,1,11,26,0,13,8,24,1,9,24,1,10,13,2,24,1,7,24,1,8,4,59], key:0}}, clause_keys:[0], next_key:1, key:260}, 261: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,43,2,9,0,0,9,1,1,12,254,3,26,127,12,255,5,26,127,12,256,7,26,467,13,6,24,1,7,26,0,13,4,24,1,5,24,1,6,13,2,24,1,3,24,1,4,2,4,59], key:0}}, clause_keys:[0], next_key:1, key:261}, 262: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,8,1,5,0,8,1,6,1,12,254,7,26,127,12,255,9,26,127,12,256,11,26,467,13,10,24,1,11,26,0,13,8,24,1,9,24,1,10,13,2,24,1,7,24,1,8,4,59], key:0}}, clause_keys:[0], next_key:1, key:262}, 263: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,8,1,3,0,41,13,1,28,2147483684,1,60,0,33,0,15,1,3,0,8,1,3,0,3,4,1,32,0,2,4,62,30,0,15,1,2,0,8,1,2,0,4,7], key:0}}, clause_keys:[0], next_key:1, key:263}, 264: {is_public:false, clauses:{0:{code:[28,1,1,15,1,2,0,8,1,2,0,3,232,0,2,4,61], key:0}, 1:{code:[30,0,15,1,1,0,5], key:1}}, clause_keys:[0, 1], next_key:2, key:264}, 265: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,33,4,20,0,23,1,6,25,0,0,16,0,1,1,16,0,2,2,16,0,3,3,8,1,6,0,8,0,1,1,3,10,5,32,4,9,0,0,9,1,1,9,2,2,9,3,3,2,4,265], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,20,0,23,1,6,25,0,0,16,0,1,1,20,2,24,1,6,25,0,2,16,0,3,3,8,1,6,0,8,0,1,1,3,33,4,9,0,0,9,1,1,9,2,2,9,3,3,2,4,265], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,20,0,23,1,6,25,0,0,16,0,1,1,16,0,2,2,20,3,24,1,6,25,0,3,8,1,6,0,8,0,1,1,3,30,4,9,0,0,9,1,1,9,2,2,9,3,3,2,4,265], key:2}, 3:{code:[30,0,17,0,0,15,1,4,1,17,0,2,17,0,3,5], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:265}, 266: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,16,1,3,2,5], key:0}, 1:{code:[30,0,20,0,23,1,6,23,1,7,15,1,8,1,20,2,24,1,6,23,1,9,8,1,7,0,8,1,8,1,8,1,9,2,4,266], key:1}}, clause_keys:[0, 1], next_key:2, key:266}, 267: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,33,4,20,0,23,1,6,25,0,0,19,214,6,23,1,7,23,1,8,16,0,1,1,16,0,2,2,16,0,3,3,8,1,7,0,8,0,1,1,3,10,5,32,4,9,0,0,9,1,1,9,2,2,9,3,3,2,4,267], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,20,0,23,1,6,25,0,0,19,214,6,23,1,7,23,1,8,16,0,1,1,20,2,23,1,9,25,0,2,19,214,9,24,1,7,24,1,8,16,0,3,3,8,1,7,0,8,0,1,1,3,33,4,9,0,0,9,1,1,9,2,2,9,3,3,2,4,267], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,20,0,23,1,6,25,0,0,19,214,6,23,1,7,23,1,8,16,0,1,1,16,0,2,2,20,3,23,1,9,25,0,3,19,214,9,24,1,7,24,1,8,8,1,7,0,8,0,1,1,3,30,4,9,0,0,9,1,1,9,2,2,9,3,3,2,4,267], key:2}, 3:{code:[30,0,17,0,0,15,1,4,1,17,0,2,17,0,3,5], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:267}, 268: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,16,1,3,2,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,20,0,23,1,5,25,0,0,15,1,6,1,16,0,2,2,8,0,1,0,12,270,1,25,1,6,27,1,3,34,3,9,0,0,9,1,1,9,2,2,2,4,268], key:1}}, clause_keys:[0, 1], next_key:2, key:268}, 269: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,8,1,5,0,14,0,1,8,1,6,2,4,268], key:0}}, clause_keys:[0], next_key:1, key:269}, 271: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,17,0,2,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,20,0,25,0,1,25,0,0,16,0,1,1,16,0,2,2,31,9,0,0,9,1,1,9,2,2,2,4,271], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,20,0,23,1,3,25,0,0,16,0,1,1,20,2,24,1,3,25,0,2,31,9,0,0,9,1,1,9,2,2,2,4,271], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:271}, 272: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,15,1,4,0,16,0,4,1,8,1,4,0,13,1,25,0,1,25,0,3,3,13,5,9,3,0,13,1,25,0,4,26,0,8,0,2,2,3,266,5,8,0,0,0,13,1,25,0,1,25,0,2,3,13,3,9,0,0,2,4,183], key:0}}, clause_keys:[0], next_key:1, key:272}, 273: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,15,1,5,0,16,0,4,1,16,0,5,2,8,1,5,0,13,1,25,0,1,25,0,3,3,13,6,9,3,0,13,3,25,0,5,26,0,13,1,25,0,4,24,1,3,8,0,2,2,3,266,6,8,0,0,0,13,1,25,0,1,25,0,2,3,13,3,9,0,0,2,4,183], key:0}}, clause_keys:[0], next_key:1, key:273}, 274: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,15,1,6,0,16,0,4,1,16,0,5,2,16,0,6,3,8,1,6,0,13,1,25,0,1,25,0,3,3,13,7,9,3,0,13,4,25,0,6,26,0,13,3,25,0,5,24,1,4,13,1,25,0,4,24,1,3,8,0,2,2,3,266,7,8,0,0,0,13,1,25,0,1,25,0,2,3,13,3,9,0,0,2,4,183], key:0}}, clause_keys:[0], next_key:1, key:274}, 275: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,15,1,7,0,16,0,4,1,16,0,5,2,16,0,6,3,16,0,7,4,8,1,7,0,13,1,25,0,1,25,0,3,3,13,8,9,3,0,13,5,25,0,7,26,0,13,4,25,0,6,24,1,5,13,3,25,0,5,24,1,4,13,1,25,0,4,24,1,3,8,0,2,2,3,266,8,8,0,0,0,13,1,25,0,1,25,0,2,3,13,3,9,0,0,2,4,183], key:0}}, clause_keys:[0], next_key:1, key:275}, 276: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,15,1,8,0,16,0,4,1,16,0,5,2,16,0,6,3,16,0,7,4,16,0,8,5,8,1,8,0,13,1,25,0,1,25,0,3,3,13,9,9,3,0,13,6,25,0,8,26,0,13,5,25,0,7,24,1,6,13,4,25,0,6,24,1,5,13,3,25,0,5,24,1,4,13,1,25,0,4,24,1,3,8,0,2,2,3,266,9,8,0,0,0,13,1,25,0,1,25,0,2,3,13,3,9,0,0,2,4,183], key:0}}, clause_keys:[0], next_key:1, key:276}, 277: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,15,1,9,0,16,0,4,1,16,0,5,2,16,0,6,3,16,0,7,4,16,0,8,5,16,0,9,6,8,1,9,0,13,1,25,0,1,25,0,3,3,13,10,9,3,0,13,7,25,0,9,26,0,13,6,25,0,8,24,1,7,13,5,25,0,7,24,1,6,13,4,25,0,6,24,1,5,13,3,25,0,5,24,1,4,13,1,25,0,4,24,1,3,8,0,2,2,3,266,10,8,0,0,0,13,1,25,0,1,25,0,2,3,13,3,9,0,0,2,4,183], key:0}}, clause_keys:[0], next_key:1, key:277}, 278: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,15,1,10,0,16,0,4,1,16,0,5,2,16,0,6,3,16,0,7,4,16,0,8,5,16,0,9,6,16,0,10,7,8,1,10,0,13,1,25,0,1,25,0,3,3,13,11,9,3,0,13,8,25,0,10,26,0,13,7,25,0,9,24,1,8,13,6,25,0,8,24,1,7,13,5,25,0,7,24,1,6,13,4,25,0,6,24,1,5,13,3,25,0,5,24,1,4,13,1,25,0,4,24,1,3,8,0,2,2,3,266,11,8,0,0,0,13,1,25,0,1,25,0,2,3,13,3,9,0,0,2,4,183], key:0}}, clause_keys:[0], next_key:1, key:278}, 279: {is_public:false, clauses:{0:{code:[254,0,10,484,0,4,105], key:0}}, clause_keys:[0], next_key:1, key:279}, 280: {is_public:false, clauses:{0:{code:[254,0,10,489,0,4,105], key:0}}, clause_keys:[0], next_key:1, key:280}, 281: {is_public:false, clauses:{0:{code:[254,0,10,491,0,4,105], key:0}}, clause_keys:[0], next_key:1, key:281}, 282: {is_public:false, clauses:{0:{code:[28,1,1,17,485,0,31,10,489,0,2,4,105], key:0}, 1:{code:[30,0,1,60,0,16,0,0,0,10,489,0,3,105,1,8,0,0,0,3,283,1,10,498,0,3,105,1,8,0,0,0,3,183,1,10,489,0,3,105,1,9,0,0,3,284,1,10,484,0,2,4,105], key:1}}, clause_keys:[0, 1], next_key:2, key:282}, 283: {is_public:false, clauses:{0:{code:[28,1,1,60,0,16,0,0,0,10,500,0,3,257,1,9,0,0,2,4,64], key:0}, 1:{code:[30,0,1,60,0,60,1,33,0,16,0,1,0,10,501,0,3,257,2,9,1,0,3,64,2,32,0,2,4,61], key:1}}, clause_keys:[0, 1], next_key:2, key:283}, 284: {is_public:false, clauses:{0:{code:[28,1,1,60,0,16,0,0,0,10,502,0,3,257,1,9,0,0,2,4,64], key:0}, 1:{code:[30,0,1,60,0,60,1,33,0,16,0,1,0,10,503,0,3,257,2,9,1,0,3,64,2,32,0,2,4,61], key:1}}, clause_keys:[0, 1], next_key:2, key:284}, 285: {is_public:false, clauses:{0:{code:[28,1,1,17,485,0,15,1,2,1,31,10,489,0,2,4,105], key:0}, 1:{code:[30,0,1,60,0,60,1,16,0,0,0,16,0,1,1,10,489,0,3,105,2,10,506,0,10,507,1,8,0,0,2,8,0,1,3,3,286,2,8,0,0,0,8,0,1,1,3,287,2,10,498,0,3,105,2,8,0,0,0,3,183,2,10,489,0,3,105,2,8,0,1,0,3,107,2,10,510,0,10,511,1,9,0,2,9,1,3,3,286,2,10,484,0,2,4,105], key:1}}, clause_keys:[0, 1], next_key:2, key:285}, 286: {is_public:false, clauses:{0:{code:[28,1,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,8,1,7,0,8,1,9,1,8,1,10,2,4,288], key:0}, 1:{code:[30,0,1,60,0,33,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,8,1,8,0,8,1,9,1,8,1,10,2,3,288,1,32,0,2,4,61], key:1}}, clause_keys:[0, 1], next_key:2, key:286}, 287: {is_public:false, clauses:{0:{code:[28,1,15,1,3,0,15,1,4,1,13,0,25,1,3,25,1,4,4,107], key:0}, 1:{code:[30,0,1,60,0,33,0,15,1,3,0,15,1,4,1,8,1,4,0,3,107,1,32,0,2,4,61], key:1}}, clause_keys:[0, 1], next_key:2, key:287}, 288: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,1,0,16,0,0,1,15,1,5,2,8,1,5,0,8,0,2,1,3,269,3,9,2,0,3,257,3,10,514,0,3,257,2,9,1,0,3,257,2,10,514,0,3,257,1,9,0,0,2,4,64], key:0}}, clause_keys:[0], next_key:1, key:288}, 289: {is_public:false, clauses:{0:{code:[28,1,1,17,485,0,15,1,3,1,15,1,4,2,31,10,489,0,2,4,105], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,16,0,0,0,16,0,1,1,16,0,2,2,10,489,0,3,105,3,10,506,0,10,507,1,8,0,0,2,8,0,1,3,8,0,2,4,3,290,3,8,0,2,0,8,0,0,1,8,0,1,2,3,291,3,10,498,0,3,105,3,8,0,0,0,3,183,3,10,489,0,3,105,3,8,0,1,0,3,107,3,10,510,0,10,511,1,9,0,2,9,1,3,9,2,4,3,290,3,10,484,0,2,4,105], key:1}}, clause_keys:[0, 1], next_key:2, key:289}, 290: {is_public:false, clauses:{0:{code:[28,1,15,1,9,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,8,1,9,0,8,1,11,1,8,1,12,2,8,1,13,3,4,293], key:0}, 1:{code:[30,0,1,60,0,33,0,15,1,9,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,8,1,10,0,8,1,11,1,8,1,12,2,8,1,13,3,3,293,1,32,0,2,4,61], key:1}}, clause_keys:[0, 1], next_key:2, key:290}, 291: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,16,0,0,0,15,1,6,1,16,0,2,2,12,292,0,25,0,1,10,515,1,13,2,25,1,6,26,0,3,79,3,12,214,1,25,0,0,25,0,1,13,0,24,1,1,25,0,2,2,4,107], key:0}, 1:{code:[30,0,1,60,0,33,0,15,1,4,0,15,1,5,1,15,1,6,2,8,1,6,0,3,107,1,32,0,2,4,61], key:1}}, clause_keys:[0, 1], next_key:2, key:291}, 293: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,16,0,1,0,16,0,0,1,16,0,3,2,15,1,5,3,8,1,5,0,3,257,4,10,514,0,3,257,4,9,3,0,8,0,2,1,3,269,4,9,2,0,3,257,3,10,514,0,3,257,2,9,1,0,3,257,2,10,514,0,3,257,1,9,0,0,2,4,64], key:0}}, clause_keys:[0], next_key:1, key:293}, 294: {is_public:false, clauses:{0:{code:[28,1,1,17,485,0,15,1,3,1,15,1,4,2,31,10,489,0,2,4,105], key:0}, 1:{code:[29,2,1,17,486,0,15,1,3,1,15,1,4,2,31,10,486,0,2,4,105], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,60,3,16,0,1,0,16,0,0,1,16,0,2,2,8,0,2,0,8,0,3,1,3,295,4,10,366,0,10,125,1,8,0,1,2,8,0,0,3,8,0,2,4,8,0,3,5,3,296,4,8,0,1,0,3,183,4,3,297,4,9,0,0,9,1,1,9,2,2,9,3,3,2,41,85,4,28,2147483797,1,60,0,60,1,60,2,60,3,60,4,33,4,16,0,1,0,16,0,0,1,16,0,2,2,16,0,3,3,3,298,5,32,4,8,0,1,0,3,107,4,10,402,0,10,525,1,9,0,2,9,1,3,9,2,4,9,3,5,2,4,296,30,0,15,1,5,0,15,1,6,1,15,1,7,2,15,1,8,3,4,62], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:294}, 295: {is_public:false, clauses:{0:{code:[28,1,15,1,3,0,15,1,4,1,8,1,4,0,4,113], key:0}, 1:{code:[30,0,1,60,0,60,1,16,0,0,0,16,0,1,1,8,0,0,0,3,111,2,10,529,0,3,110,2,9,0,0,9,1,1,2,4,295], key:1}}, clause_keys:[0, 1], next_key:2, key:295}, 296: {is_public:false, clauses:{0:{code:[28,1,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,15,1,15,4,15,1,16,5,8,1,11,0,8,1,13,1,8,1,14,2,8,1,15,3,8,1,16,4,4,301], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,33,0,15,1,7,0,16,0,1,1,16,0,2,2,16,0,3,3,16,0,4,4,16,0,5,5,40,72,0,6,40,94,0,6,9,1,0,9,2,1,9,3,2,9,4,3,9,5,4,3,301,6,32,0,2,4,61,28,2147483738,1,60,0,33,0,10,489,0,3,106,1,32,0,2,4,61,30,0,4,62,28,2147483760,1,60,0,33,0,10,491,0,3,106,1,32,0,2,4,61,30,0,4,62], key:1}}, clause_keys:[0, 1], next_key:2, key:296}, 297: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,33,0,8,0,1,0,3,106,2,9,1,0,3,300,2,32,0,2,5], key:0}}, clause_keys:[0], next_key:1, key:297}, 298: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,33,0,8,0,1,0,3,106,2,9,1,0,3,299,2,32,0,2,5], key:0}}, clause_keys:[0], next_key:1, key:298}, 299: {is_public:false, clauses:{0:{code:[28,1,17,526,0,5], key:0}, 1:{code:[30,0,17,527,0,5], key:1}}, clause_keys:[0, 1], next_key:2, key:299}, 300: {is_public:false, clauses:{0:{code:[28,1,1,17,484,0,31,10,526,0,2,4,105], key:0}, 1:{code:[29,2,1,17,528,0,31,10,527,0,2,4,105], key:1}, 2:{code:[30,0,15,1,1,0,5], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:300}, 301: {is_public:false, clauses:{0:{code:[254,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,7,0,8,1,8,1,8,1,9,2,8,1,10,3,8,1,11,4,41,45,5,28,2147483780,1,60,0,60,1,60,2,60,3,60,4,60,5,33,5,16,0,0,0,16,0,1,1,16,0,2,2,16,0,3,3,16,0,4,4,8,0,0,0,8,0,1,1,3,302,6,32,5,8,0,0,0,8,0,1,1,8,0,2,2,8,0,3,3,3,303,5,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,304,30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,15,1,10,4,10,537,0,8,1,6,1,8,1,7,2,8,1,8,3,8,1,9,4,8,1,10,5,4,305], key:0}}, clause_keys:[0], next_key:1, key:301}, 302: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,1,0,16,0,2,1,8,0,0,0,3,306,3,9,0,0,9,1,1,9,2,2,2,4,307], key:0}}, clause_keys:[0], next_key:1, key:302}, 303: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,16,0,2,0,16,0,3,1,15,1,6,2,16,0,4,3,8,1,6,0,8,0,1,1,3,269,5,9,1,0,9,2,1,9,3,2,9,4,3,8,0,0,4,3,310,5,9,0,0,2,4,112], key:0}}, clause_keys:[0], next_key:1, key:303}, 304: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,16,0,1,0,16,0,2,1,16,0,3,2,16,0,4,3,16,0,5,4,8,0,0,0,3,315,7,32,6,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,305], key:0}}, clause_keys:[0], next_key:1, key:304}, 305: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,17,571,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,31,10,571,0,9,0,1,9,1,2,9,2,3,9,3,4,9,4,5,2,4,317], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,17,564,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,31,10,564,0,9,0,1,9,1,2,9,2,3,9,3,4,9,4,5,2,4,317], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,60,4,17,563,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,31,10,563,0,9,0,1,9,1,2,9,2,3,9,3,4,9,4,5,2,4,317], key:2}, 3:{code:[29,4,1,60,0,60,1,60,2,60,3,17,562,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,15,1,6,5,31,10,562,0,9,0,1,9,1,2,9,2,3,9,3,4,7,6,5,2,4,317], key:3}, 4:{code:[29,5,1,60,0,60,1,60,2,60,3,60,4,17,561,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,31,10,561,0,9,0,1,9,1,2,9,2,3,9,3,4,9,4,5,2,4,317], key:4}, 5:{code:[29,6,1,60,0,60,1,60,2,17,537,0,17,366,1,16,0,1,2,16,0,2,3,16,0,0,4,15,1,6,5,31,9,0,0,9,1,1,9,2,2,3,291,3,10,575,0,2,4,105], key:5}, 6:{code:[29,7,1,17,537,0,17,402,1,15,1,6,2,15,1,7,3,15,1,8,4,15,1,9,5,31,10,528,0,2,4,105], key:6}, 7:{code:[29,8,17,537,0,15,1,6,1,15,1,7,2,15,1,8,3,15,1,9,4,15,1,10,5,31,5], key:7}, 8:{code:[29,9,1,17,570,0,17,366,1,15,1,6,2,15,1,7,3,15,1,8,4,15,1,9,5,31,10,526,0,2,4,105], key:8}, 9:{code:[29,10,17,570,0,15,1,6,1,15,1,7,2,15,1,8,3,15,1,9,4,15,1,10,5,31,5], key:9}, 10:{code:[29,11,1,17,565,0,15,1,6,1,15,1,7,2,15,1,8,3,15,1,9,4,15,1,10,5,31,10,489,0,2,4,105], key:10}, 11:{code:[29,12,1,17,569,0,15,1,6,1,15,1,7,2,15,1,8,3,15,1,9,4,15,1,10,5,31,2,4,61], key:11}, 12:{code:[29,13,1,60,0,60,1,17,568,0,15,1,6,1,15,1,7,2,15,1,8,3,16,0,1,4,16,0,0,5,31,9,1,0,3,110,2,9,0,0,3,114,1,2,4,61], key:12}, 13:{code:[29,14,1,17,566,0,15,1,6,1,15,1,7,2,15,1,8,3,15,1,9,4,15,1,10,5,31,2,4,199], key:13}, 14:{code:[29,15,1,60,0,60,1,60,2,60,3,60,4,17,567,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,31,8,0,2,0,3,318,5,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,304], key:14}, 15:{code:[29,16,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,17,481,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,31,8,0,1,0,13,1,25,0,5,25,0,9,3,13,10,9,9,0,8,0,6,1,3,269,10,8,0,8,0,8,0,6,1,3,269,9,8,0,7,0,13,1,25,0,5,25,0,8,3,13,9,12,319,0,23,1,1,25,0,7,26,127,3,175,8,10,578,0,3,257,7,12,178,0,25,0,5,25,0,6,3,64,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,304], key:15}, 16:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,17,416,0,16,0,0,1,16,0,1,2,16,0,2,3,16,0,3,4,16,0,4,5,31,8,0,1,0,13,1,25,0,5,25,0,9,3,13,10,9,9,0,8,0,6,1,3,269,10,8,0,8,0,8,0,6,1,3,269,9,8,0,7,0,13,1,25,0,5,25,0,8,3,13,9,12,319,0,23,1,1,25,0,7,23,1,2,3,264,8,10,579,0,3,257,7,12,178,0,25,0,5,25,0,6,3,64,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,304], key:16}}, clause_keys:[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16], next_key:17, key:305}, 306: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,106,2,9,0,0,9,1,1,2,4,309], key:0}}, clause_keys:[0], next_key:1, key:306}, 307: {is_public:false, clauses:{0:{code:[28,1,17,541,0,15,1,3,1,15,1,4,2,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,17,543,0,16,0,0,1,16,0,1,2,31,9,0,0,9,1,1,7,3,2,2,41,32,3,28,2147483720,1,60,0,33,0,15,1,6,0,15,1,7,1,15,1,8,2,8,1,6,0,8,1,7,1,8,1,8,2,40,88,3,1,32,0,2,4,61,30,0,15,1,4,0,15,1,5,1,15,1,6,2,4,62,28,2147483812,1,60,0,60,1,60,2,60,3,33,0,16,0,3,0,16,0,2,1,16,0,1,2,8,0,3,0,8,0,2,1,8,0,1,2,3,308,4,10,545,0,3,257,4,9,3,0,3,257,4,10,514,0,3,257,3,9,2,0,3,64,3,9,1,0,40,180,1,2,32,0,2,4,61,30,0,15,1,4,0,15,1,5,1,15,1,6,2,4,62,28,2147483854,1,60,0,33,0,15,1,3,0,8,1,3,0,10,127,1,3,9,1,32,0,2,4,62,30,0,15,1,2,0,8,1,2,0,4,183], key:1}}, clause_keys:[0, 1], next_key:2, key:307}, 309: {is_public:false, clauses:{0:{code:[28,1,17,484,0,17,541,1,5], key:0}, 1:{code:[29,2,17,526,0,17,541,1,5], key:1}, 2:{code:[29,3,17,528,0,17,543,1,5], key:2}, 3:{code:[30,0,17,527,0,17,543,1,5], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:309}, 310: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,16,0,6,0,16,0,5,1,16,0,3,2,15,1,8,3,16,0,4,4,8,1,8,0,14,7,1,8,0,0,2,3,311,7,9,6,0,14,5,1,8,0,1,2,3,311,7,9,5,0,8,0,2,1,3,314,6,13,6,25,0,3,26,0,13,5,26,556,24,1,6,13,4,25,0,2,24,1,5,13,3,26,514,24,1,4,13,2,25,0,1,24,1,3,13,0,25,0,0,24,1,2,9,4,1,2,4,312], key:0}}, clause_keys:[0], next_key:1, key:310}, 311: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,15,1,5,0,16,0,3,1,16,0,0,2,8,1,5,0,8,0,4,1,3,26,5,8,0,4,0,8,0,2,1,3,269,5,9,2,0,9,3,1,7,6,2,7,7,3,9,4,4,8,0,1,5,40,79,6,5,9,0,0,9,1,1,2,4,24,28,2147483821,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,16,0,5,0,16,0,4,1,16,0,3,2,16,0,0,3,16,0,1,4,16,0,2,5,8,0,5,0,8,0,4,1,3,36,7,32,6,8,0,3,0,12,214,1,25,0,4,25,0,5,3,34,6,9,3,0,8,0,0,1,3,324,4,9,0,0,9,1,1,9,2,2,2,4,266,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,15,1,12,5,13,0,27,32,26,0,8,1,11,1,8,1,12,2,4,266], key:0}}, clause_keys:[0], next_key:1, key:311}, 312: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,8,1,5,0,10,596,1,8,1,6,2,4,328], key:0}}, clause_keys:[0], next_key:1, key:312}, 313: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,16,0,4,0,16,0,2,1,15,1,7,2,16,0,3,3,8,1,7,0,14,7,1,8,0,0,2,3,311,5,9,4,0,14,5,1,8,0,1,2,3,311,5,13,4,25,0,2,26,0,13,3,26,514,24,1,4,13,2,25,0,1,24,1,3,13,0,25,0,0,24,1,2,9,3,1,2,4,312], key:0}}, clause_keys:[0], next_key:1, key:313}, 314: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,15,1,4,0,16,0,0,1,8,1,4,0,8,0,4,1,3,24,5,9,4,0,13,1,25,0,3,25,0,2,3,9,5,9,3,0,8,0,1,1,3,326,4,9,0,0,13,1,25,0,1,25,0,2,2,4,24], key:0}}, clause_keys:[0], next_key:1, key:314}, 315: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,33,0,16,0,1,0,8,0,1,0,3,316,2,9,1,0,13,14,26,561,26,0,13,13,26,562,24,1,14,13,12,26,563,24,1,13,13,11,26,564,24,1,12,13,10,26,565,24,1,11,13,9,26,566,24,1,10,13,8,26,567,24,1,9,13,7,26,568,24,1,8,13,6,26,569,24,1,7,13,5,26,416,24,1,6,13,4,26,481,24,1,5,13,3,26,537,24,1,4,13,2,26,570,24,1,3,13,1,26,571,24,1,2,3,116,2,32,0,2,5], key:0}, 1:{code:[30,0,1,60,0,16,0,0,0,10,572,0,3,64,1,9,0,0,2,4,315], key:1}}, clause_keys:[0, 1], next_key:2, key:315}, 316: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,2,0,8,1,2,0,3,109,1,32,0,2,5], key:0}, 1:{code:[30,0,1,60,0,16,0,0,0,3,323,1,9,0,0,2,4,109], key:1}}, clause_keys:[0, 1], next_key:2, key:316}, 317: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,16,0,0,0,17,366,1,16,0,2,2,16,0,3,3,16,0,1,4,15,1,6,5,31,9,1,0,9,2,1,9,3,2,3,291,4,9,0,0,3,320,1,10,498,0,2,4,105], key:0}, 1:{code:[29,2,1,15,1,6,0,17,402,1,15,1,7,2,15,1,8,3,15,1,9,4,15,1,10,5,31,10,484,0,2,4,105], key:1}, 2:{code:[30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,15,1,10,4,15,1,11,5,31,5], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:317}, 318: {is_public:false, clauses:{0:{code:[28,1,17,0,0,31,5], key:0}, 1:{code:[30,0,1,60,0,15,1,3,0,8,1,3,0,8,0,0,1,3,321,1,10,586,0,3,64,1,9,0,0,14,1,1,2,4,322], key:1}}, clause_keys:[0, 1], next_key:2, key:318}, 320: {is_public:false, clauses:{0:{code:[28,1,1,17,571,0,10,489,0,3,115,0,2,4,92], key:0}, 1:{code:[29,2,1,17,564,0,10,582,0,3,115,0,2,4,91], key:1}, 2:{code:[29,3,1,17,563,0,10,582,0,3,115,0,2,4,92], key:2}, 3:{code:[29,4,1,17,562,0,10,484,0,3,115,0,2,4,91], key:3}, 4:{code:[30,0,1,17,561,0,10,484,0,3,115,0,2,4,92], key:4}}, clause_keys:[0, 1, 2, 3, 4], next_key:5, key:320}, 321: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,8,1,5,0,10,0,1,8,1,6,2,4,327], key:0}}, clause_keys:[0], next_key:1, key:321}, 322: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,2,1,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,20,0,23,1,6,25,0,0,19,214,6,23,1,7,23,1,8,16,0,2,1,8,0,2,0,8,1,8,1,8,1,7,2,8,0,3,3,3,313,4,9,3,0,3,64,4,8,0,1,0,12,270,1,25,0,2,27,1,3,34,3,9,0,0,9,1,1,2,4,322], key:1}}, clause_keys:[0, 1], next_key:2, key:322}, 323: {is_public:false, clauses:{0:{code:[28,1,1,10,127,0,3,108,0,2,4,199], key:0}, 1:{code:[30,0,10,467,0,4,108], key:1}}, clause_keys:[0, 1], next_key:2, key:323}, 324: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,15,1,4,0,16,0,0,1,8,0,0,0,8,1,4,1,3,269,2,13,0,27,32,26,0,13,1,25,0,1,26,0,3,9,2,9,0,0,9,1,1,2,4,325], key:0}}, clause_keys:[0], next_key:1, key:324}, 325: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,2,1,5], key:0}, 1:{code:[30,0,20,0,23,1,4,23,1,5,16,1,4,1,8,1,5,0,8,1,4,1,4,325], key:1}}, clause_keys:[0, 1], next_key:2, key:325}, 326: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,16,0,1,0,16,0,3,1,13,0,27,97,26,0,13,1,25,0,0,26,0,3,9,4,13,0,27,122,26,0,13,1,25,0,2,26,0,3,9,4,9,0,0,9,1,1,9,2,2,7,6,3,7,7,4,9,3,5,2,41,73,6,28,2147483834,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,5,16,0,4,0,16,0,3,1,16,0,6,2,16,0,2,3,16,0,1,4,16,0,0,5,8,0,4,0,8,0,3,1,3,37,7,8,0,3,0,9,6,1,3,37,7,32,5,13,0,27,65,26,0,13,1,25,0,2,26,0,3,9,5,8,0,1,0,12,214,1,25,0,3,25,0,4,3,34,5,9,0,0,12,270,1,25,0,1,25,0,2,2,4,34,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,15,1,12,5,8,1,12,0,8,1,8,1,4,9], key:0}}, clause_keys:[0], next_key:1, key:326}, 327: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,16,1,3,2,5], key:0}, 1:{code:[30,0,20,0,23,1,6,23,1,7,15,1,8,1,15,1,9,2,8,1,7,0,13,1,24,1,6,25,1,8,8,1,9,2,4,327], key:1}}, clause_keys:[0, 1], next_key:2, key:327}, 328: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,16,1,3,2,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,20,0,23,1,5,25,0,0,15,1,6,1,16,0,2,2,8,1,5,0,8,1,6,1,8,0,1,2,7,7,3,7,8,4,7,9,5,40,62,6,3,9,0,0,9,1,1,9,2,2,2,4,328,28,2147483768,1,60,0,60,1,60,2,60,3,33,3,16,0,1,0,16,0,0,1,16,0,2,2,15,1,8,3,15,1,9,4,15,1,10,5,8,0,1,0,3,4,4,32,3,9,0,0,9,1,1,9,2,2,2,4,20,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,8,0,8,1,12,1,8,1,13,2,8,1,9,3,8,1,10,4,41,169,5,28,2147483895,1,60,0,60,1,60,2,60,3,60,4,60,5,33,5,16,0,4,0,16,0,3,1,16,0,1,2,16,0,0,3,16,0,2,4,8,0,4,0,3,228,6,32,5,9,4,0,8,0,3,1,3,26,5,8,0,1,0,9,3,1,3,24,4,9,0,0,9,1,1,9,2,2,2,4,20,30,0,1,60,0,60,1,60,2,15,1,6,0,15,1,7,1,16,0,1,2,16,0,0,3,16,0,2,4,12,292,0,25,0,1,10,597,1,13,2,25,1,6,26,0,3,79,3,9,0,0,9,1,1,9,2,2,2,4,20], key:1}}, clause_keys:[0, 1], next_key:2, key:328}, 329: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:329}, 330: {is_public:false, clauses:{0:{code:[254,0,15,1,2,0,13,0,25,1,2,23,1,3,4,329], key:0}}, clause_keys:[0], next_key:1, key:330}, 332: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,0,0,8,0,1,0,40,36,1,2,13,1,25,0,0,25,0,1,12,331,0,24,1,1,2,4,230,28,2147483709,1,60,0,33,0,15,1,3,0,12,331,0,25,1,3,3,232,1,32,0,2,4,62,30,0,15,1,2,0,8,1,2,0,10,0,1,4,9], key:0}}, clause_keys:[0], next_key:1, key:332}, 333: {is_public:false, clauses:{0:{code:[254,0,1,60,0,15,1,2,0,13,3,25,1,2,25,0,0,12,331,0,24,1,3,3,232,1,12,331,0,25,0,0,2,4,230], key:0}}, clause_keys:[0], next_key:1, key:333}, 334: {is_public:false, clauses:{0:{code:[254,0,1,60,0,15,1,3,0,8,1,3,0,8,0,0,1,3,24,1,7,5,0,7,6,1,7,7,2,9,0,3,7,8,4,2,41,39,5,28,2147483754,1,60,0,60,1,60,2,60,3,33,0,15,1,7,0,16,0,2,1,16,0,1,2,16,0,3,3,15,1,8,4,13,0,27,58,26,0,8,1,7,1,8,0,2,2,3,266,4,9,1,0,9,2,1,9,3,2,3,266,4,32,0,2,4,62,30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,15,1,10,4,13,0,27,47,26,0,8,1,10,1,8,1,9,2,4,266], key:0}}, clause_keys:[0], next_key:1, key:334}, 335: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,15,1,4,0,16,0,4,1,8,1,4,0,8,0,1,1,3,24,5,13,0,27,47,26,0,7,3,1,8,0,2,2,3,266,5,13,0,27,47,26,0,8,0,3,1,8,0,0,2,3,266,5,7,9,0,9,0,1,9,1,2,7,10,3,7,11,4,9,2,5,9,3,6,7,12,7,9,4,8,2,41,96,9,28,2147483856,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,33,3,16,0,2,0,15,1,11,1,15,1,12,2,16,0,4,3,16,0,5,4,16,0,6,5,16,0,7,6,16,0,1,7,16,0,0,8,8,0,2,0,8,1,11,1,8,1,12,2,3,266,8,9,5,0,9,6,1,9,7,2,40,255,3,8,32,3,9,2,0,13,1,27,47,26,0,8,0,1,2,3,266,3,9,0,0,9,1,1,2,4,24,30,0,15,1,10,0,15,1,11,1,15,1,12,2,15,1,13,3,15,1,14,4,15,1,15,5,15,1,16,6,15,1,17,7,15,1,18,8,8,1,18,0,10,617,1,4,9,28,2147483942,1,60,0,33,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,5,0,8,1,6,1,8,1,7,2,3,266,1,32,0,2,4,61,30,0,15,1,4,0,15,1,5,1,15,1,6,2,4,62], key:0}}, clause_keys:[0], next_key:1, key:335}, 336: {is_public:false, clauses:{0:{code:[28,1,17,617,0,15,1,3,1,16,1,3,2,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,16,0,0,0,15,1,5,1,16,0,2,2,8,1,5,0,8,0,1,1,3,24,3,9,0,0,9,1,1,9,2,2,2,4,339], key:1}}, clause_keys:[0, 1], next_key:2, key:336}, 338: {is_public:false, clauses:{0:{code:[254,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,7,6,2,7,7,3,7,8,4,7,9,5,7,10,6,41,36,7,28,2147483741,1,60,0,60,1,60,2,33,2,16,0,0,0,16,0,1,1,15,1,9,2,15,1,10,3,15,1,11,4,15,1,12,5,15,1,13,6,8,0,0,0,3,334,3,32,2,9,0,0,9,1,1,2,4,9,30,0,15,1,9,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,15,1,14,5,15,1,15,6,8,1,12,0,8,1,13,1,8,1,9,2,8,1,10,3,8,1,14,4,8,1,15,5,41,150,6,28,2147483868,1,60,0,60,1,60,2,60,3,60,4,33,4,16,0,3,0,16,0,0,1,16,0,1,2,16,0,2,3,15,1,8,4,15,1,9,5,8,0,3,0,3,330,5,32,4,9,3,0,8,0,0,1,3,335,4,9,0,0,9,1,1,9,2,2,2,4,336,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,10,0,8,1,13,1,8,1,11,2,41,261,3,254,0,1,60,0,60,1,60,2,33,2,15,1,5,0,16,0,1,1,16,0,0,2,8,1,5,0,8,0,1,1,10,0,2,3,337,3,32,2,9,0,0,9,1,1,2,4,9], key:0}}, clause_keys:[0], next_key:1, key:338}, 339: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,15,1,7,2,7,8,0,8,1,6,1,8,1,5,2,7,9,3,7,10,4,8,1,7,5,7,11,6,7,12,7,41,44,8,28,2147483806,1,60,0,60,1,60,2,60,3,60,4,60,5,33,5,16,0,3,0,15,1,10,1,16,0,4,2,16,0,0,3,16,0,1,4,16,0,2,5,15,1,11,6,15,1,12,7,13,14,27,47,26,0,13,13,27,46,24,1,14,13,0,27,46,24,1,13,8,0,3,1,8,1,10,2,3,266,6,32,5,9,4,0,8,0,0,1,3,340,5,8,0,1,0,9,3,1,3,24,4,9,0,0,9,1,1,9,2,2,2,4,336,30,0,1,60,0,60,1,60,2,60,3,15,1,9,0,16,0,3,1,15,1,10,2,15,1,11,3,15,1,12,4,16,0,0,5,16,0,2,6,16,0,1,7,8,1,10,0,8,0,2,1,3,24,4,9,2,0,9,3,1,8,0,1,2,3,266,4,9,0,0,9,1,1,2,4,24], key:0}}, clause_keys:[0], next_key:1, key:339}, 340: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,15,1,4,0,16,0,0,1,8,1,4,0,8,0,7,1,3,24,8,9,7,0,8,0,6,1,3,341,8,13,0,27,47,26,0,8,0,5,1,9,6,2,3,266,7,7,3,0,13,1,27,47,26,0,8,0,4,2,3,266,6,9,4,0,8,0,3,1,9,5,2,3,266,6,13,0,27,47,26,0,9,3,1,8,0,2,2,3,266,4,9,2,0,8,0,1,1,3,341,3,9,0,0,9,1,1,2,4,24], key:0}}, clause_keys:[0], next_key:1, key:340}, 342: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,33,0,16,0,2,0,16,0,3,1,16,0,1,2,20,3,25,0,2,25,0,1,8,0,2,0,3,3,4,9,3,0,8,0,2,1,3,349,4,9,1,0,9,2,1,3,350,3,32,0,2,5], key:0}, 1:{code:[29,2,1,60,0,33,0,15,1,5,0,15,1,6,1,15,1,7,2,16,1,7,3,8,1,5,0,3,3,1,32,0,2,5], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,60,4,33,4,15,1,8,0,15,1,9,1,16,0,2,2,16,0,3,3,8,1,8,0,8,1,9,1,8,0,0,2,8,0,1,3,3,351,5,32,4,9,0,0,9,1,1,9,2,2,9,3,3,2,4,342], key:2}, 3:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,16,0,1,0,16,0,2,1,16,0,3,2,16,0,4,3,8,0,1,0,7,7,1,8,0,0,2,3,11,5,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,352], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:342}, 348: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,33,0,16,0,1,0,8,0,1,0,10,0,1,10,0,2,8,0,3,3,3,342,4,8,0,3,0,10,0,1,3,213,4,32,0,8,0,2,0,3,343,4,10,630,0,3,344,4,3,345,4,10,633,0,3,257,4,12,346,0,25,0,1,3,257,4,3,345,4,10,634,0,3,257,4,9,3,0,3,257,4,3,345,3,3,347,3,9,2,0,3,344,3,32,0,9,1,0,3,183,2,32,0,2,4,61], key:0}, 1:{code:[29,2,1,60,0,33,0,15,1,2,0,8,1,2,0,3,183,1,32,0,2,4,61], key:1}, 2:{code:[30,0,15,1,1,0,5], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:348}, 349: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,33,2,16,0,0,0,16,0,1,1,8,0,0,0,3,3,3,32,2,9,0,0,9,1,1,2,4,213], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,16,0,1,0,16,0,2,1,8,0,1,0,7,5,1,8,0,0,2,3,11,3,9,0,0,9,1,1,9,2,2,2,4,358], key:1}}, clause_keys:[0, 1], next_key:2, key:349}, 350: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,33,2,20,0,23,1,4,25,0,0,16,0,1,1,8,1,4,0,8,0,1,1,3,213,3,32,2,9,0,0,9,1,1,2,4,350], key:0}, 1:{code:[30,0,17,0,0,15,1,2,1,5], key:1}}, clause_keys:[0, 1], next_key:2, key:350}, 351: {is_public:false, clauses:{0:{code:[28,1,19,353,0,23,1,4,15,1,5,1,17,125,2,16,1,5,3,31,5], key:0}, 1:{code:[29,2,19,346,0,23,1,4,15,1,5,1,17,125,2,16,1,5,3,31,5], key:1}, 2:{code:[29,3,19,219,0,23,1,4,23,1,5,15,1,6,1,16,1,5,2,19,270,3,25,1,6,24,1,4,31,5], key:2}, 3:{code:[29,4,19,354,0,23,1,4,23,1,5,23,1,6,15,1,7,1,19,214,2,24,1,5,24,1,6,19,270,3,25,1,7,24,1,4,31,5], key:3}, 4:{code:[29,5,19,355,0,23,1,4,23,1,5,23,1,6,15,1,7,1,19,214,2,24,1,5,24,1,6,19,270,3,25,1,7,24,1,4,31,5], key:4}, 5:{code:[29,6,19,356,0,23,1,4,23,1,5,23,1,6,15,1,7,1,19,214,2,24,1,5,24,1,6,19,270,3,25,1,7,24,1,4,31,5], key:5}, 6:{code:[30,0,19,357,0,23,1,4,23,1,5,23,1,6,15,1,7,1,19,214,2,24,1,5,24,1,6,19,270,3,25,1,7,24,1,4,31,5], key:6}}, clause_keys:[0, 1, 2, 3, 4, 5, 6], next_key:7, key:351}, 352: {is_public:false, clauses:{0:{code:[28,1,21,0,0,15,1,5,1,15,1,6,2,15,1,7,3,16,1,7,4,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,33,5,16,0,6,0,16,0,1,1,16,0,2,2,16,0,8,3,16,0,4,4,8,0,6,0,8,0,1,1,8,0,7,2,3,12,9,9,7,0,8,0,2,1,9,8,2,8,0,3,3,3,342,9,8,0,0,0,12,214,1,25,0,6,27,1,3,34,7,32,5,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,352], key:1}}, clause_keys:[0, 1], next_key:2, key:352}, 358: {is_public:false, clauses:{0:{code:[28,1,21,0,0,15,1,3,1,15,1,4,2,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,33,3,16,0,4,0,16,0,1,1,16,0,2,2,8,0,4,0,8,0,1,1,8,0,5,2,3,12,6,9,5,0,8,0,2,1,3,349,6,8,0,0,0,12,214,1,25,0,4,27,1,3,34,5,32,3,9,0,0,9,1,1,9,2,2,2,4,358], key:1}}, clause_keys:[0, 1], next_key:2, key:358}, 359: {is_public:false, clauses:{0:{code:[28,1,1,15,1,3,0,15,1,4,1,8,1,3,0,3,101,0,2,4,199], key:0}, 1:{code:[30,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,4,102], key:1}}, clause_keys:[0, 1], next_key:2, key:359}, 360: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:360}, 361: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:361}, 362: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:362}, 363: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:363}, 364: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:364}, 365: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:365}, 366: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:366}, 367: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:367}, 368: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:368}, 369: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:369}, 372: {is_public:false, clauses:{0:{code:[254,0,1,60,0,15,1,6,0,15,1,7,1,16,0,0,2,8,1,6,0,8,1,7,1,8,0,0,2,3,86,1,9,0,0,7,2,1,2,41,42,2,28,2147483717,1,60,0,33,0,15,1,4,0,15,1,5,1,8,1,4,0,3,368,1,32,0,2,4,62,30,0,15,1,4,0,15,1,5,1,8,1,4,0,41,86,1,254,0,1,60,0,60,1,33,1,16,0,0,0,3,201,2,32,1,12,371,0,25,0,0,2,4,176], key:0}}, clause_keys:[0], next_key:1, key:372}, 383: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,16,0,0,0,16,0,2,1,8,0,1,0,12,270,1,25,0,0,27,1,3,34,4,8,0,3,0,12,270,1,25,0,2,27,1,3,34,4,9,0,0,7,12,1,9,1,2,7,13,3,9,2,4,9,3,5,7,14,6,7,15,7,7,16,8,7,17,9,7,18,10,7,19,11,2,41,89,12,28,2147483885,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,16,0,3,0,16,0,5,1,16,0,2,2,16,0,4,3,16,0,1,4,16,0,0,5,15,1,14,6,15,1,15,7,15,1,16,8,15,1,17,9,15,1,18,10,15,1,19,11,8,0,3,0,14,254,1,3,365,7,32,6,12,375,0,25,0,3,25,0,5,3,232,6,12,375,0,25,0,2,25,0,4,3,232,5,12,375,0,25,0,3,27,28,3,176,4,12,375,0,25,0,2,25,0,1,3,176,3,12,375,0,25,0,1,27,30,3,176,2,12,375,0,25,0,0,27,0,2,4,176,30,0,15,1,14,0,15,1,15,1,15,1,16,2,15,1,17,3,15,1,18,4,15,1,19,5,15,1,20,6,15,1,21,7,15,1,22,8,15,1,23,9,15,1,24,10,15,1,25,11,8,1,14,0,8,1,16,1,8,1,21,2,8,1,18,3,8,1,22,4,8,1,23,5,8,1,24,6,8,1,25,7,8,1,19,8,41,326,9,28,2147484054,1,60,0,60,1,60,2,60,3,33,3,15,1,11,0,16,0,2,1,16,0,0,2,16,0,1,3,15,1,12,4,15,1,13,5,15,1,14,6,15,1,15,7,15,1,16,8,8,1,11,0,14,28,1,3,365,4,32,3,9,2,0,8,0,0,1,3,365,3,9,0,0,9,1,1,2,4,383,30,0,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,15,1,15,4,15,1,16,5,15,1,17,6,15,1,18,7,15,1,19,8,8,1,11,0,8,1,12,1,8,1,13,2,8,1,14,3,8,1,16,4,8,1,17,5,8,1,18,6,8,1,19,7,41,479,8,28,2147484203,1,60,0,60,1,60,2,60,3,33,3,15,1,10,0,16,0,2,1,16,0,0,2,16,0,1,3,15,1,11,4,15,1,12,5,15,1,13,6,15,1,14,7,8,1,10,0,14,29,1,3,365,4,32,3,9,2,0,8,0,0,1,3,365,3,9,0,0,9,1,1,2,4,383,30,0,15,1,10,0,15,1,11,1,15,1,12,2,15,1,13,3,15,1,14,4,15,1,15,5,15,1,16,6,15,1,17,7,8,1,10,0,8,1,15,1,8,1,11,2,8,1,16,3,8,1,13,4,8,1,17,5,41,616,6,254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,16,0,3,0,16,0,5,1,16,0,2,2,16,0,4,3,16,0,1,4,16,0,0,5,8,0,3,0,14,30,1,3,365,7,32,6,12,375,0,25,0,3,25,0,5,3,232,6,12,375,0,25,0,2,25,0,4,3,232,5,12,375,0,25,0,3,27,29,3,176,4,12,375,0,25,0,2,25,0,1,3,176,3,12,375,0,25,0,1,27,30,3,176,2,12,375,0,25,0,0,27,0,2,4,176], key:0}}, clause_keys:[0], next_key:1, key:383}, 385: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,6,1,15,1,7,2,16,1,7,3,15,1,8,4,16,1,8,5,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,20,0,23,1,6,25,0,0,19,399,6,25,0,7,16,0,6,1,16,0,8,2,16,0,3,3,16,0,4,4,16,0,5,5,31,8,0,7,0,8,0,6,1,9,8,2,8,0,2,3,3,88,9,12,400,0,25,0,7,25,0,6,3,196,8,8,0,1,0,12,270,1,25,0,6,27,2,3,34,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,385], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,20,0,23,1,6,25,0,0,19,401,6,25,0,7,25,0,10,25,0,11,16,0,6,1,16,0,2,2,16,0,3,3,20,4,23,1,7,25,0,4,19,390,7,25,0,7,25,0,8,16,0,5,5,31,12,402,1,25,0,7,25,0,10,25,0,11,25,0,6,12,403,0,24,1,1,3,196,12,8,0,8,0,12,270,1,25,0,6,27,1,3,34,12,8,0,6,0,13,1,27,40,26,0,3,395,12,8,0,9,0,12,270,1,25,0,6,27,2,3,34,12,9,9,0,13,2,25,0,11,26,0,13,1,25,0,10,24,1,2,3,395,12,12,404,1,25,0,8,12,405,0,25,0,7,24,1,1,3,196,9,8,0,1,0,12,270,1,25,0,6,27,4,3,34,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,385], key:2}, 3:{code:[29,4,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,20,0,23,1,6,25,0,0,19,406,6,25,0,10,25,0,8,16,0,6,1,16,0,2,2,16,0,3,3,20,4,23,1,7,25,0,4,19,390,7,25,0,10,25,0,9,16,0,5,5,31,12,407,1,25,0,10,25,0,8,25,0,6,12,403,0,24,1,1,3,196,11,9,9,0,12,270,1,25,0,6,27,1,3,34,10,8,0,6,0,13,1,27,41,26,0,3,395,9,8,0,7,0,12,270,1,25,0,6,27,2,3,34,9,9,7,0,13,1,25,0,8,26,0,3,395,9,8,0,1,0,12,270,1,25,0,6,27,3,3,34,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,385], key:3}, 4:{code:[29,5,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,20,0,23,1,6,25,0,0,19,408,6,25,0,8,16,0,6,1,16,0,2,2,16,0,3,3,20,4,23,1,7,25,0,4,19,390,7,25,0,8,25,0,7,16,0,5,5,31,12,409,0,25,0,8,25,0,6,3,196,9,9,7,0,12,270,1,25,0,6,27,1,3,34,8,8,0,6,0,13,1,27,28,26,0,3,395,7,8,0,1,0,12,270,1,25,0,6,27,2,3,34,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,385], key:4}, 5:{code:[29,6,1,60,0,60,1,60,2,60,3,60,4,60,5,20,0,23,1,6,25,0,0,19,410,6,23,1,7,16,0,1,1,20,2,23,1,8,25,0,2,19,384,8,24,1,7,25,0,1,16,0,3,3,16,0,4,4,16,0,5,5,31,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,385], key:5}, 6:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,20,0,25,0,9,25,0,0,16,0,6,1,16,0,2,2,16,0,3,3,16,0,4,4,16,0,5,5,31,12,411,0,25,0,9,25,0,6,3,196,10,9,9,0,8,0,7,1,8,0,8,2,3,412,10,8,0,6,0,9,8,1,3,395,9,8,0,1,0,12,270,1,25,0,6,25,0,7,3,34,8,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,385], key:6}}, clause_keys:[0, 1, 2, 3, 4, 5, 6], next_key:7, key:385}, 387: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,2,1,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,20,0,23,1,5,25,0,0,19,384,5,23,1,6,23,1,7,16,0,1,1,8,1,6,0,8,1,7,1,8,0,1,2,3,389,2,9,0,0,9,1,1,2,4,387], key:1}}, clause_keys:[0, 1], next_key:2, key:387}, 388: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,16,0,3,0,16,0,4,1,8,0,3,0,8,0,2,1,3,63,5,9,3,0,9,4,1,8,0,0,2,10,0,3,8,0,1,4,10,0,5,3,385,5,12,386,0,25,0,2,25,0,0,3,196,3,9,0,0,9,1,1,3,387,2,10,688,0,2,4,196], key:0}}, clause_keys:[0], next_key:1, key:388}, 389: {is_public:false, clauses:{0:{code:[28,1,15,1,3,0,15,1,4,1,17,0,2,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,33,5,16,0,0,0,16,0,1,1,20,2,23,1,5,25,0,2,19,390,5,25,0,4,25,0,3,8,0,0,0,8,0,4,1,3,10,6,32,5,12,391,1,25,0,3,12,392,0,25,0,4,25,0,1,24,1,1,3,196,5,9,3,0,8,0,1,1,3,83,4,9,0,0,9,1,1,9,2,2,2,4,389], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,16,0,0,0,16,0,1,1,20,2,23,1,5,25,0,2,19,393,5,25,0,5,25,0,3,8,0,0,0,8,0,5,1,3,10,7,32,6,12,391,1,25,0,3,12,394,0,25,0,5,25,0,1,24,1,1,3,196,6,8,0,4,0,12,382,1,25,0,1,27,2147483648,3,34,5,9,3,0,9,4,1,3,83,5,9,0,0,9,1,1,9,2,2,2,4,389], key:2}, 3:{code:[30,0,15,1,6,0,15,1,7,1,20,2,23,1,8,23,1,9,8,1,6,0,8,1,7,1,8,1,9,2,4,389], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:389}, 395: {is_public:false, clauses:{0:{code:[28,1,15,1,2,0,17,0,1,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,16,0,2,0,20,1,23,1,4,25,0,1,8,0,2,0,8,1,4,1,3,83,3,8,0,0,0,12,270,1,25,0,2,27,1,3,34,3,9,0,0,9,1,1,2,4,395], key:1}}, clause_keys:[0, 1], next_key:2, key:395}, 397: {is_public:false, clauses:{0:{code:[28,1,19,396,0,23,1,3,20,1,27,0,23,1,4,20,4,24,1,3,23,1,5,16,1,5,2,5], key:0}, 1:{code:[30,0,19,398,0,23,1,3,20,1,27,1,23,1,4,20,4,24,1,3,23,1,5,16,1,5,2,5], key:1}}, clause_keys:[0, 1], next_key:2, key:397}, 412: {is_public:false, clauses:{0:{code:[28,1,19,413,0,23,1,3,21,0,1,17,0,2,5], key:0}, 1:{code:[29,2,17,711,0,21,1,1,20,2,27,1,26,0,5], key:1}, 2:{code:[29,3,17,712,0,21,1,1,20,2,27,2,26,0,5], key:2}, 3:{code:[29,4,19,414,0,23,1,6,23,1,7,19,178,6,23,1,8,23,1,9,21,3,1,20,2,27,3,23,1,10,20,10,23,1,11,23,1,12,20,12,24,1,7,26,0,8,1,8,0,8,1,9,1,8,1,11,2,4,86], key:3}, 4:{code:[29,5,19,415,0,23,1,6,19,178,6,23,1,7,23,1,8,21,2,1,20,2,27,4,23,1,9,20,9,23,1,10,26,0,8,1,7,0,8,1,8,1,8,1,10,2,4,86], key:4}, 5:{code:[29,6,17,714,0,21,1,1,20,2,27,5,26,0,5], key:5}, 6:{code:[29,7,19,416,0,23,1,3,23,1,4,19,396,3,23,1,5,19,398,4,23,1,6,21,3,1,20,2,27,6,23,1,7,20,7,24,1,5,23,1,8,20,8,24,1,6,26,0,5], key:6}, 7:{code:[29,8,19,417,0,23,1,3,19,396,3,23,1,4,21,2,1,20,2,27,60,23,1,5,20,5,24,1,4,26,0,5], key:7}, 8:{code:[29,9,19,416,0,23,1,3,23,1,4,19,398,3,23,1,5,19,398,4,23,1,6,21,3,1,20,2,27,7,23,1,7,20,7,24,1,5,23,1,8,20,8,24,1,6,26,0,5], key:8}, 9:{code:[29,10,19,418,0,23,1,6,23,1,7,19,398,7,23,1,8,21,4,1,20,2,27,8,23,1,9,8,1,6,0,8,1,9,1,13,2,24,1,8,26,0,4,397], key:9}, 10:{code:[29,11,19,419,0,23,1,3,23,1,4,19,396,3,23,1,5,19,398,4,23,1,6,21,3,1,20,2,27,9,23,1,7,20,7,24,1,5,23,1,8,20,8,24,1,6,26,0,5], key:10}, 11:{code:[29,12,19,419,0,23,1,4,23,1,5,19,398,4,23,1,6,19,398,5,23,1,7,21,3,1,20,2,27,9,23,1,8,20,8,24,1,6,23,1,9,20,9,24,1,7,26,0,12,398,10,24,1,6,12,420,0,24,1,10,4,125], key:11}, 12:{code:[29,13,19,421,0,23,1,5,23,1,6,19,398,6,23,1,7,21,3,1,20,2,27,10,23,1,8,20,8,23,1,9,23,1,10,20,10,24,1,7,26,0,8,1,5,0,8,1,9,1,4,84], key:12}, 13:{code:[29,14,19,422,0,23,1,3,19,398,3,23,1,4,21,2,1,20,2,27,11,23,1,5,20,5,24,1,4,26,0,5], key:13}, 14:{code:[29,15,19,423,0,23,1,6,23,1,7,19,178,6,23,1,8,23,1,9,19,398,7,23,1,10,21,3,1,20,2,27,12,23,1,11,20,11,23,1,12,23,1,13,20,13,24,1,10,26,0,8,1,8,0,8,1,9,1,8,1,12,2,4,86], key:14}, 15:{code:[29,16,19,424,0,23,1,3,19,398,3,23,1,4,21,2,1,20,2,27,13,23,1,5,20,5,24,1,4,26,0,5], key:15}, 16:{code:[29,17,19,425,0,23,1,3,23,1,4,19,398,4,23,1,5,21,3,1,20,2,27,14,23,1,6,20,6,24,1,3,23,1,7,20,7,24,1,5,26,0,5], key:16}, 17:{code:[29,18,19,426,0,23,1,5,23,1,6,19,398,6,23,1,7,21,3,1,20,2,27,51,23,1,8,20,8,23,1,9,23,1,10,20,10,24,1,7,26,0,8,1,5,0,8,1,9,1,4,85], key:17}, 18:{code:[29,19,19,427,0,23,1,6,23,1,7,19,398,7,23,1,8,21,4,1,20,2,27,15,23,1,9,8,1,6,0,8,1,9,1,13,2,24,1,8,26,0,4,397], key:18}, 19:{code:[29,20,19,428,0,23,1,6,23,1,7,19,398,7,23,1,8,21,4,1,20,2,27,16,23,1,9,8,1,6,0,8,1,9,1,13,2,24,1,8,26,0,4,397], key:19}, 20:{code:[29,21,19,429,0,23,1,5,23,1,6,19,398,6,23,1,7,21,3,1,20,2,27,17,23,1,8,20,8,23,1,9,23,1,10,20,10,24,1,7,26,0,8,1,5,0,8,1,9,1,4,84], key:20}, 21:{code:[29,22,19,430,0,23,1,3,19,398,3,23,1,4,21,2,1,20,2,27,18,23,1,5,20,5,24,1,4,26,0,5], key:21}, 22:{code:[29,23,19,431,0,23,1,6,23,1,7,19,178,6,23,1,8,23,1,9,19,398,7,23,1,10,21,3,1,20,2,27,19,23,1,11,20,11,23,1,12,23,1,13,20,13,24,1,10,26,0,8,1,8,0,8,1,9,1,8,1,12,2,4,86], key:22}, 23:{code:[29,24,19,432,0,23,1,3,19,398,3,23,1,4,21,2,1,20,2,27,20,23,1,5,20,5,24,1,4,26,0,5], key:23}, 24:{code:[29,25,19,433,0,23,1,3,23,1,4,19,398,4,23,1,5,21,3,1,20,2,27,21,23,1,6,20,6,24,1,3,23,1,7,20,7,24,1,5,26,0,5], key:24}, 25:{code:[29,26,19,434,0,23,1,5,23,1,6,19,398,6,23,1,7,21,3,1,20,2,27,50,23,1,8,20,8,23,1,9,23,1,10,20,10,24,1,7,26,0,8,1,5,0,8,1,9,1,4,85], key:25}, 26:{code:[29,27,19,435,0,23,1,3,21,2,1,20,2,27,22,23,1,4,20,4,24,1,3,26,0,5], key:26}, 27:{code:[29,28,19,436,0,23,1,6,21,3,1,20,2,27,23,23,1,7,8,1,6,0,8,1,7,1,10,0,2,4,397], key:27}, 28:{code:[29,29,19,437,0,23,1,6,21,3,1,20,2,27,24,23,1,7,8,1,6,0,8,1,7,1,10,0,2,4,397], key:28}, 29:{code:[29,30,19,438,0,23,1,6,21,3,1,20,2,27,25,23,1,7,8,1,6,0,8,1,7,1,10,0,2,4,397], key:29}, 30:{code:[29,31,19,439,0,23,1,5,21,2,1,20,2,27,26,23,1,6,20,6,23,1,7,26,0,8,1,5,0,8,1,7,1,4,84], key:30}, 31:{code:[29,32,19,440,0,23,1,3,21,2,1,20,2,27,27,23,1,4,20,4,24,1,3,26,0,5], key:31}, 32:{code:[29,33,19,441,0,23,1,5,21,2,1,20,2,27,52,23,1,6,20,6,23,1,7,26,0,8,1,5,0,8,1,7,1,4,85], key:32}, 33:{code:[29,34,19,408,0,23,1,3,21,2,1,20,2,27,28,23,1,4,20,4,24,1,3,26,0,5], key:33}, 34:{code:[29,35,19,442,0,23,1,3,21,2,1,20,2,27,29,23,1,4,20,4,24,1,3,26,0,5], key:34}, 35:{code:[29,36,17,741,0,21,2,1,20,2,27,30,23,1,3,20,3,27,0,26,0,5], key:35}, 36:{code:[29,37,17,742,0,21,1,1,20,2,27,31,26,0,5], key:36}, 37:{code:[29,38,19,443,0,23,1,3,19,396,3,23,1,4,21,2,1,20,2,27,32,23,1,5,20,5,24,1,4,26,0,5], key:37}, 38:{code:[29,39,19,444,0,23,1,3,19,396,3,23,1,4,21,2,1,20,2,27,33,23,1,5,20,5,24,1,4,26,0,5], key:38}, 39:{code:[29,40,19,401,0,23,1,3,23,1,4,23,1,5,21,4,1,20,2,27,40,23,1,6,20,6,24,1,3,23,1,7,20,7,24,1,4,23,1,8,20,8,24,1,5,26,0,5], key:39}, 40:{code:[29,41,19,406,0,23,1,3,23,1,4,21,3,1,20,2,27,41,23,1,5,20,5,24,1,3,23,1,6,20,6,24,1,4,26,0,5], key:40}, 41:{code:[29,42,17,745,0,21,1,1,20,2,27,42,26,0,5], key:41}, 42:{code:[29,43,19,445,0,23,1,3,23,1,4,19,396,4,23,1,5,21,3,1,20,2,27,43,23,1,6,20,6,24,1,3,23,1,7,20,7,24,1,5,26,0,5], key:42}, 43:{code:[30,0,17,747,0,21,2,1,20,2,27,254,23,1,3,20,3,27,0,26,0,5], key:43}}, clause_keys:[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43], next_key:44, key:412}, 446: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,15,1,6,0,16,0,3,1,16,0,4,2,16,0,0,3,8,1,6,0,8,0,2,1,3,24,5,9,2,0,13,1,25,0,3,25,0,4,8,0,1,2,3,266,5,9,0,0,9,1,1,2,4,24], key:0}}, clause_keys:[0], next_key:1, key:446}, 447: {is_public:false, clauses:{0:{code:[254,0,17,755,0,5], key:0}}, clause_keys:[0], next_key:1, key:447}, 448: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,1,0,16,0,2,1,8,0,0,0,3,447,3,9,0,0,9,1,1,9,2,2,2,4,20], key:0}}, clause_keys:[0], next_key:1, key:448}, 449: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,2,0,16,0,1,1,13,0,27,47,26,0,13,1,25,0,0,26,0,3,9,3,7,5,0,9,0,1,9,1,2,9,2,3,7,6,4,2,41,52,5,28,2147483767,1,60,0,60,1,60,2,60,3,33,0,15,1,7,0,16,0,2,1,16,0,3,2,15,1,8,3,16,0,1,4,8,1,7,0,13,1,25,0,2,25,0,3,8,1,8,2,3,266,4,9,2,0,9,3,1,40,151,2,4,32,0,2,4,62,30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,15,1,10,4,8,1,9,0,8,1,8,1,4,9,28,2147483830,1,60,0,33,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,3,116,1,32,0,2,4,61,30,0,15,1,3,0,15,1,4,1,4,62], key:0}}, clause_keys:[0], next_key:1, key:449}, 450: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,7,0,8,1,6,1,8,1,5,2,41,29,3,28,2147483716,1,60,0,33,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,5,0,8,1,6,1,8,1,7,2,3,266,1,32,0,2,4,62,30,0,15,1,4,0,15,1,5,1,15,1,6,2,8,1,4,0,8,1,6,1,4,9], key:0}}, clause_keys:[0], next_key:1, key:450}, 451: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,15,1,4,0,16,0,0,1,8,1,4,0,8,0,3,1,3,24,4,9,3,0,8,0,2,1,3,449,4,9,2,0,13,4,27,108,26,0,13,3,27,112,24,1,4,13,1,27,46,24,1,3,8,0,1,2,3,450,3,9,0,0,9,1,1,2,4,24], key:0}}, clause_keys:[0], next_key:1, key:451}, 452: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,16,1,3,2,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,20,0,23,1,5,25,0,0,15,1,6,1,16,0,2,2,8,0,1,0,12,270,1,25,1,6,27,1,3,34,3,9,0,0,9,1,1,9,2,2,2,4,452], key:1}}, clause_keys:[0, 1], next_key:2, key:452}, 453: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,8,1,5,0,14,0,1,8,1,6,2,4,452], key:0}}, clause_keys:[0], next_key:1, key:453}, 454: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:454}, 455: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:455}, 456: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:456}, 457: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:457}, 458: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:458}, 460: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,33,0,16,0,1,0,16,0,2,1,12,178,0,26,777,27,2,3,29,3,9,1,0,9,2,1,3,459,3,32,0,2,5], key:0}, 1:{code:[30,0,15,1,2,0,16,1,2,1,5], key:1}}, clause_keys:[0, 1], next_key:2, key:460}, 463: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,16,0,3,0,16,0,1,1,16,0,2,2,13,4,25,0,3,26,0,13,0,26,785,24,1,4,3,196,4,8,0,3,0,8,0,0,1,3,460,4,3,133,4,10,786,0,3,196,4,9,0,0,9,1,1,9,2,2,7,5,3,9,3,4,2,41,79,5,28,2147483774,1,60,0,33,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,7,0,8,1,8,1,8,1,9,2,3,465,1,32,0,2,4,62,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,11,0,41,155,1,254,0,1,60,0,60,1,33,0,16,0,1,0,10,788,0,3,257,2,9,1,0,3,64,2,32,0,2,4,61], key:0}}, clause_keys:[0], next_key:1, key:463}, 464: {is_public:false, clauses:{0:{code:[254,0,15,1,4,0,8,1,4,0,12,461,5,27,0,12,462,1,27,0,24,1,5,7,6,2,4,463], key:0}}, clause_keys:[0], next_key:1, key:464}, 465: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,16,1,3,2,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,20,0,25,0,3,25,0,0,16,0,4,1,16,0,2,2,31,9,3,0,9,4,1,8,0,1,2,3,466,5,9,0,0,9,1,1,9,2,2,2,4,465], key:1}, 2:{code:[30,0,15,1,6,0,15,1,7,1,15,1,8,2,8,1,6,0,8,1,7,1,8,1,8,2,4,466], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:465}, 466: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,19,467,0,25,0,0,16,0,1,1,16,0,2,2,31,9,0,0,9,1,1,9,2,2,2,4,468], key:0}, 1:{code:[30,0,15,1,5,0,15,1,6,1,16,1,6,2,8,1,6,0,8,1,5,1,41,25,2,28,2147483700,1,60,0,33,0,15,1,4,0,15,1,5,1,8,1,4,0,3,469,1,32,0,2,4,62,30,0,1,60,0,15,1,3,0,16,0,0,1,8,0,0,0,3,185,1,9,0,0,2,4,470], key:1}}, clause_keys:[0, 1], next_key:2, key:466}, 468: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,5,0,8,1,6,1,8,1,7,2,7,8,3,41,32,4,28,2147483723,1,60,0,33,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,8,1,6,0,8,1,7,1,8,1,8,2,3,473,1,32,0,2,4,62,30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,8,1,7,0,8,1,8,1,8,1,6,2,41,108,3,28,2147483797,1,60,0,60,1,60,2,33,2,16,0,0,0,16,0,1,1,15,1,5,2,8,0,0,0,3,469,3,32,2,9,0,0,9,1,1,2,4,9,30,0,15,1,4,0,15,1,5,1,15,1,6,2,8,1,6,0,8,1,4,1,8,1,5,2,4,474], key:0}}, clause_keys:[0], next_key:1, key:468}, 469: {is_public:false, clauses:{0:{code:[254,0,19,462,0,23,1,1,23,1,2,19,471,2,23,1,3,5], key:0}}, clause_keys:[0], next_key:1, key:469}, 470: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,19,167,0,25,0,2,25,0,3,31,8,0,2,0,8,0,4,1,8,0,1,2,3,11,6,8,0,5,0,3,499,6,9,4,0,8,0,1,1,9,5,2,8,0,0,3,3,500,6,12,178,0,25,0,0,25,0,1,9,2,1,9,3,2,2,4,87], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,16,0,2,0,31,8,0,2,0,8,0,3,1,8,0,1,2,3,11,5,8,0,4,0,3,499,5,9,3,0,8,0,1,1,9,4,2,8,0,0,3,3,500,5,12,178,0,25,0,0,25,0,1,9,2,1,10,127,2,2,4,87], key:1}}, clause_keys:[0, 1], next_key:2, key:470}, 472: {is_public:false, clauses:{0:{code:[254,0,19,462,0,23,1,1,23,1,2,19,461,2,23,1,3,5], key:0}}, clause_keys:[0], next_key:1, key:472}, 473: {is_public:false, clauses:{0:{code:[28,1,19,487,0,23,1,6,15,1,7,1,15,1,8,2,12,487,0,24,1,6,8,1,7,1,8,1,8,2,4,478], key:0}, 1:{code:[29,2,17,814,0,15,1,6,1,15,1,7,2,10,814,0,8,1,6,1,8,1,7,2,4,478], key:1}, 2:{code:[29,3,19,488,0,23,1,6,15,1,7,1,15,1,8,2,12,488,0,24,1,6,8,1,7,1,8,1,8,2,4,478], key:2}, 3:{code:[30,0,17,816,0,15,1,6,1,15,1,7,2,10,816,0,8,1,6,1,8,1,7,2,4,478], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:473}, 474: {is_public:false, clauses:{0:{code:[28,1,19,475,0,23,1,4,23,1,5,23,1,6,15,1,7,1,16,1,7,2,12,475,0,24,1,4,24,1,5,24,1,6,4,476], key:0}, 1:{code:[29,2,19,477,0,23,1,6,15,1,7,1,16,1,7,2,12,477,0,24,1,6,8,1,7,1,8,1,7,2,4,478], key:1}, 2:{code:[29,3,19,479,0,23,1,4,15,1,5,1,16,1,5,2,8,1,4,0,4,480], key:2}, 3:{code:[29,4,19,481,0,23,1,6,15,1,7,1,16,1,7,2,12,481,0,24,1,6,8,1,7,1,8,1,7,2,4,478], key:3}, 4:{code:[29,5,19,482,0,23,1,3,15,1,4,1,16,1,4,2,5], key:4}, 5:{code:[29,6,19,483,0,23,1,6,23,1,7,15,1,8,1,16,1,8,2,12,483,0,24,1,6,24,1,7,8,1,8,1,8,1,8,2,4,478], key:5}, 6:{code:[29,7,19,484,0,23,1,6,15,1,7,1,16,1,7,2,12,484,0,24,1,6,8,1,7,1,8,1,7,2,4,478], key:6}, 7:{code:[29,8,19,485,0,23,1,6,15,1,7,1,16,1,7,2,12,485,0,24,1,6,8,1,7,1,8,1,7,2,4,478], key:7}, 8:{code:[30,0,19,486,0,23,1,6,15,1,7,1,16,1,7,2,12,486,0,24,1,6,8,1,7,1,8,1,7,2,4,478], key:8}}, clause_keys:[0, 1, 2, 3, 4, 5, 6, 7, 8], next_key:9, key:474}, 476: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,2,0,8,0,2,0,3,183,3,8,0,1,0,3,95,3,8,0,0,0,12,167,1,25,0,1,25,0,2,3,9,3,8,0,0,0,3,185,1,9,0,0,2,4,470], key:0}}, clause_keys:[0], next_key:1, key:476}, 478: {is_public:false, clauses:{0:{code:[28,1,19,477,0,23,1,4,15,1,5,1,16,1,5,2,8,1,4,0,4,489], key:0}, 1:{code:[29,2,19,481,0,23,1,4,15,1,5,1,16,1,5,2,8,1,4,0,4,194], key:1}, 2:{code:[29,3,19,483,0,23,1,5,23,1,6,15,1,7,1,16,1,7,2,8,1,5,0,8,1,6,1,4,490], key:2}, 3:{code:[29,4,19,484,0,23,1,4,15,1,5,1,16,1,5,2,8,1,4,0,4,491], key:3}, 4:{code:[29,5,19,485,0,23,1,4,15,1,5,1,16,1,5,2,8,1,4,0,4,492], key:4}, 5:{code:[29,6,19,486,0,23,1,4,15,1,5,1,16,1,5,2,8,1,4,0,4,493], key:5}, 6:{code:[29,7,19,487,0,23,1,6,15,1,7,1,15,1,8,2,8,1,6,0,8,1,7,1,8,1,8,2,4,494], key:6}, 7:{code:[29,8,17,814,0,15,1,5,1,15,1,6,2,8,1,5,0,8,1,6,1,4,495], key:7}, 8:{code:[29,9,19,488,0,23,1,6,15,1,7,1,15,1,8,2,8,1,6,0,8,1,7,1,8,1,8,2,4,496], key:8}, 9:{code:[30,0,17,816,0,15,1,5,1,15,1,6,2,8,1,5,0,8,1,6,1,4,497], key:9}}, clause_keys:[0, 1, 2, 3, 4, 5, 6, 7, 8, 9], next_key:10, key:478}, 480: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,2,0,12,526,0,25,0,2,3,175,3,8,0,1,0,3,94,3,8,0,0,0,12,167,1,25,0,1,25,0,2,3,9,3,8,0,0,0,3,185,1,9,0,0,2,4,470], key:0}}, clause_keys:[0], next_key:1, key:480}, 489: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,19,178,0,25,0,0,25,0,1,31,12,178,0,25,0,0,25,0,1,2,4,498], key:0}, 1:{code:[29,2,17,0,0,5], key:1}, 2:{code:[29,3,1,60,0,60,1,20,0,25,0,1,25,0,0,31,9,1,0,3,489,2,9,0,0,2,4,489], key:2}, 3:{code:[30,0,1,60,0,19,188,0,23,1,2,25,0,0,8,1,2,0,3,489,1,9,0,0,2,4,489], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:489}, 490: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,0,0,16,0,1,1,8,0,2,0,3,501,3,8,0,0,0,9,2,1,3,502,3,9,1,0,8,0,0,1,3,503,2,9,0,0,2,41,51,1,28,2147483730,1,60,0,33,0,15,1,3,0,8,1,3,0,10,2,1,3,227,1,32,0,12,504,0,26,2,2,4,491,30,0,15,1,2,0,4,62], key:0}}, clause_keys:[0], next_key:1, key:490}, 491: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,15,1,3,0,8,1,3,0,8,0,1,1,3,508,2,8,0,0,0,7,2,1,3,505,2,9,0,0,9,1,1,2,41,42,2,28,2147483729,1,60,0,60,1,33,1,16,0,0,0,15,1,4,1,8,0,0,0,8,1,4,1,3,9,2,32,1,12,509,0,25,0,0,2,4,125,30,0,15,1,3,0,15,1,4,1,12,510,0,25,1,3,25,1,4,4,175], key:0}}, clause_keys:[0], next_key:1, key:491}, 492: {is_public:false, clauses:{0:{code:[28,1,17,0,0,5], key:0}, 1:{code:[30,0,1,60,0,20,0,23,1,3,25,0,0,8,1,3,0,10,541,1,3,516,1,9,0,0,2,4,492], key:1}}, clause_keys:[0, 1], next_key:2, key:492}, 493: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,19,188,0,25,0,1,25,0,0,31,9,1,0,3,493,2,9,0,0,2,4,493], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,15,1,3,0,8,1,3,0,13,1,25,0,3,25,0,2,3,13,5,8,0,2,0,8,0,1,1,3,453,5,8,0,4,0,3,499,5,9,3,0,8,0,1,1,9,4,2,8,0,0,3,3,500,5,12,518,0,25,0,0,25,0,1,25,0,2,2,4,175], key:1}}, clause_keys:[0, 1], next_key:2, key:493}, 494: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,16,0,4,0,15,1,5,1,16,0,2,2,8,1,5,0,12,462,1,25,0,3,25,0,0,3,9,5,8,0,1,0,12,270,1,25,0,3,27,1,3,34,5,9,0,0,7,8,1,9,1,2,9,2,3,9,3,4,7,9,5,7,10,6,9,4,7,2,41,84,8,28,2147483825,1,60,0,60,1,60,2,60,3,60,4,33,3,15,1,10,0,16,0,2,1,16,0,4,2,16,0,0,3,16,0,1,4,15,1,11,5,15,1,12,6,15,1,13,7,8,1,10,0,12,471,1,25,0,2,3,9,5,8,0,2,0,9,4,1,3,36,5,32,3,9,0,0,12,471,2,25,0,2,12,462,1,25,0,1,24,1,2,2,4,9,30,0,15,1,10,0,15,1,11,1,15,1,12,2,15,1,13,3,15,1,14,4,15,1,15,5,15,1,16,6,15,1,17,7,8,1,10,0,8,1,11,1,8,1,12,2,8,1,16,3,8,1,17,4,8,1,13,5,41,238,6,28,2147483951,1,60,0,60,1,60,2,33,0,15,1,8,0,16,0,1,1,16,0,2,2,15,1,9,3,15,1,10,4,15,1,11,5,8,1,8,0,12,471,1,25,0,1,3,9,3,9,1,0,9,2,1,3,38,3,32,0,10,833,0,2,4,125,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,12,0,8,1,13,1,8,1,10,2,41,344,3,28,2147484045,1,60,0,60,1,60,2,33,2,15,1,5,0,16,0,0,1,16,0,1,2,8,1,5,0,3,183,3,32,2,9,0,0,12,461,2,25,0,1,12,462,1,25,0,1,24,1,2,2,4,9,30,0,15,1,4,0,15,1,5,1,15,1,6,2,8,1,5,0,12,471,7,24,1,6,12,462,1,25,1,6,24,1,7,4,9], key:0}}, clause_keys:[0], next_key:1, key:494}, 495: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,16,0,3,0,16,0,2,1,8,0,3,0,12,462,1,25,0,1,25,0,0,3,9,4,9,0,0,7,11,1,9,1,2,9,2,3,9,3,4,7,12,5,7,13,6,7,14,7,7,15,8,7,16,9,7,17,10,2,41,72,11,28,2147483812,1,60,0,60,1,60,2,60,3,60,4,33,2,15,1,13,0,16,0,4,1,16,0,3,2,16,0,0,3,16,0,1,4,15,1,14,5,15,1,15,6,15,1,16,7,15,1,17,8,15,1,18,9,15,1,19,10,8,1,13,0,12,471,1,25,0,4,3,9,5,9,3,0,9,4,1,3,35,5,32,2,9,0,0,9,1,1,2,4,9,30,0,15,1,13,0,15,1,14,1,15,1,15,2,15,1,16,3,15,1,17,4,15,1,18,5,15,1,19,6,15,1,20,7,15,1,21,8,15,1,22,9,15,1,23,10,8,1,13,0,8,1,14,1,8,1,15,2,8,1,16,3,8,1,19,4,8,1,20,5,8,1,21,6,8,1,22,7,8,1,23,8,41,249,9,28,2147483992,1,60,0,60,1,60,2,60,3,33,2,15,1,11,0,16,0,3,1,16,0,1,2,16,0,0,3,15,1,12,4,15,1,13,5,15,1,14,6,15,1,15,7,15,1,16,8,8,1,11,0,12,471,1,25,0,3,3,9,4,8,0,1,0,9,3,1,3,9,4,32,2,9,0,0,12,461,2,25,0,1,12,462,1,25,0,1,24,1,2,2,4,9,30,0,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,15,1,15,4,15,1,16,5,15,1,17,6,15,1,18,7,15,1,19,8,8,1,11,0,8,1,12,1,8,1,13,2,8,1,16,3,8,1,17,4,8,1,18,5,8,1,14,6,8,1,19,7,41,417,8,28,2147484138,1,60,0,60,1,60,2,33,0,15,1,10,0,16,0,2,1,16,0,1,2,15,1,11,3,15,1,12,4,15,1,13,5,15,1,14,6,15,1,15,7,8,1,10,0,12,471,1,25,0,2,3,9,3,9,1,0,9,2,1,3,36,3,32,0,10,834,0,2,4,125,30,0,15,1,10,0,15,1,11,1,15,1,12,2,15,1,13,3,15,1,14,4,15,1,15,5,15,1,16,6,15,1,17,7,8,1,10,0,8,1,14,1,8,1,12,2,8,1,15,3,8,1,16,4,8,1,17,5,41,551,6,28,2147484264,1,60,0,60,1,60,2,33,0,15,1,8,0,16,0,2,1,16,0,1,2,15,1,9,3,15,1,10,4,15,1,11,5,8,1,8,0,12,461,1,25,0,2,3,9,3,9,1,0,9,2,1,3,35,3,32,0,10,835,0,2,4,125,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,8,0,8,1,9,1,8,1,10,2,8,1,12,3,8,1,13,4,41,665,5,28,2147484392,1,60,0,60,1,60,2,60,3,33,2,15,1,7,0,16,0,3,1,16,0,1,2,16,0,0,3,15,1,8,4,8,1,7,0,12,461,1,25,0,3,3,9,4,8,0,1,0,9,3,1,3,9,4,32,2,9,0,0,12,471,2,25,0,1,12,462,1,25,0,1,24,1,2,2,4,9,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,8,1,7,0,8,1,8,1,8,1,9,2,41,781,3,254,0,1,60,0,60,1,60,2,33,0,15,1,5,0,16,0,2,1,16,0,1,2,8,1,5,0,12,461,1,25,0,2,3,9,3,9,1,0,9,2,1,3,36,3,32,0,10,836,0,2,4,125], key:0}}, clause_keys:[0], next_key:1, key:495}, 496: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,16,0,3,0,15,1,5,1,16,0,2,2,8,1,5,0,12,462,1,25,0,1,25,0,0,3,9,4,9,0,0,7,11,1,9,1,2,7,12,3,9,2,4,7,13,5,7,14,6,7,15,7,7,16,8,7,17,9,9,3,10,2,41,76,11,28,2147483809,1,60,0,60,1,60,2,33,0,15,1,13,0,16,0,2,1,16,0,1,2,15,1,14,3,15,1,15,4,15,1,16,5,15,1,17,6,15,1,18,7,15,1,19,8,15,1,20,9,15,1,21,10,8,1,13,0,12,461,1,25,0,2,3,9,3,9,1,0,9,2,1,3,35,3,32,0,10,835,0,2,4,125,30,0,15,1,13,0,15,1,14,1,15,1,15,2,15,1,16,3,15,1,17,4,15,1,18,5,15,1,19,6,15,1,20,7,15,1,21,8,15,1,22,9,15,1,23,10,8,1,13,0,8,1,14,1,8,1,15,2,8,1,17,3,8,1,18,4,8,1,19,5,8,1,20,6,8,1,21,7,8,1,22,8,8,1,23,9,41,250,10,28,2147483997,1,60,0,60,1,60,2,60,3,33,2,15,1,12,0,16,0,3,1,16,0,1,2,16,0,0,3,15,1,13,4,15,1,14,5,15,1,15,6,15,1,16,7,15,1,17,8,15,1,18,9,8,1,12,0,12,461,1,25,0,3,3,9,4,8,0,1,0,9,3,1,3,9,4,32,2,9,0,0,12,471,2,25,0,1,12,462,1,25,0,1,24,1,2,2,4,9,30,0,15,1,12,0,15,1,13,1,15,1,14,2,15,1,15,3,15,1,16,4,15,1,17,5,15,1,18,6,15,1,19,7,15,1,20,8,15,1,21,9,8,1,12,0,8,1,13,1,8,1,14,2,8,1,17,3,8,1,18,4,8,1,15,5,8,1,19,6,8,1,20,7,8,1,21,8,41,430,9,28,2147484155,1,60,0,60,1,60,2,33,0,15,1,11,0,16,0,2,1,16,0,1,2,15,1,12,3,15,1,13,4,15,1,14,5,15,1,15,6,15,1,16,7,15,1,17,8,8,1,11,0,12,461,1,25,0,2,3,9,3,9,1,0,9,2,1,3,36,3,32,0,10,836,0,2,4,125,30,0,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,15,1,15,4,15,1,16,5,15,1,17,6,15,1,18,7,15,1,19,8,8,1,11,0,8,1,15,1,8,1,13,2,8,1,16,3,8,1,17,4,8,1,18,5,8,1,19,6,41,576,7,28,2147484312,1,60,0,60,1,60,2,60,3,33,3,15,1,9,0,16,0,2,1,16,0,1,2,16,0,0,3,15,1,10,4,15,1,11,5,15,1,12,6,8,1,9,0,12,471,1,25,0,2,3,9,4,8,0,2,0,8,0,1,1,3,36,4,32,3,9,0,0,12,471,2,25,0,2,12,462,1,25,0,1,24,1,2,2,4,9,30,0,15,1,9,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,15,1,14,5,15,1,15,6,8,1,9,0,8,1,10,1,8,1,11,2,8,1,14,3,8,1,15,4,8,1,12,5,41,721,6,28,2147484434,1,60,0,60,1,60,2,33,0,15,1,8,0,16,0,1,1,16,0,2,2,15,1,9,3,15,1,10,4,15,1,11,5,8,1,8,0,12,471,1,25,0,1,3,9,3,9,1,0,9,2,1,3,35,3,32,0,10,837,0,2,4,125,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,12,0,8,1,13,1,8,1,10,2,41,827,3,28,2147484528,1,60,0,60,1,60,2,33,2,15,1,5,0,16,0,0,1,16,0,1,2,8,1,5,0,3,183,3,32,2,9,0,0,12,461,2,25,0,1,12,462,1,25,0,1,24,1,2,2,4,9,30,0,15,1,4,0,15,1,5,1,15,1,6,2,8,1,5,0,12,471,7,24,1,6,12,462,1,25,1,6,24,1,7,4,9], key:0}}, clause_keys:[0], next_key:1, key:496}, 497: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,15,1,4,0,16,0,2,1,8,1,4,0,12,462,1,25,0,1,25,0,0,3,9,4,8,0,3,0,12,214,1,25,0,1,27,1,3,34,4,9,0,0,7,10,1,9,1,2,9,2,3,9,3,4,7,11,5,7,12,6,7,13,7,7,14,8,7,15,9,2,41,84,10,28,2147483832,1,60,0,60,1,60,2,60,3,60,4,33,2,15,1,12,0,16,0,3,1,16,0,4,2,16,0,0,3,16,0,1,4,15,1,13,5,15,1,14,6,15,1,15,7,15,1,16,8,15,1,17,9,8,1,12,0,12,471,1,25,0,3,3,9,5,9,3,0,9,4,1,3,9,5,32,2,9,0,0,12,461,2,25,0,1,12,462,1,25,0,1,24,1,2,2,4,9,30,0,15,1,12,0,15,1,13,1,15,1,14,2,15,1,15,3,15,1,16,4,15,1,17,5,15,1,18,6,15,1,19,7,15,1,20,8,15,1,21,9,8,1,12,0,8,1,13,1,8,1,14,2,8,1,18,3,8,1,19,4,8,1,15,5,8,1,16,6,8,1,20,7,8,1,21,8,41,265,9,28,2147483990,1,60,0,60,1,60,2,33,0,15,1,11,0,16,0,1,1,16,0,2,2,15,1,12,3,15,1,13,4,15,1,14,5,15,1,15,6,15,1,16,7,15,1,17,8,8,1,11,0,12,471,1,25,0,1,3,9,3,9,1,0,9,2,1,3,227,3,32,0,10,838,0,2,4,125,30,0,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,15,1,15,4,15,1,16,5,15,1,17,6,15,1,18,7,15,1,19,8,8,1,11,0,8,1,15,1,8,1,13,2,8,1,16,3,8,1,17,4,8,1,18,5,8,1,19,6,41,411,7,28,2147484147,1,60,0,60,1,60,2,60,3,60,4,33,2,15,1,9,0,16,0,3,1,16,0,4,2,16,0,0,3,16,0,1,4,15,1,10,5,15,1,11,6,8,1,9,0,12,461,1,25,0,3,3,9,5,9,3,0,9,4,1,3,9,5,32,2,9,0,0,12,461,2,25,0,1,12,462,1,25,0,1,24,1,2,2,4,9,30,0,15,1,9,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,15,1,14,5,15,1,15,6,8,1,9,0,8,1,10,1,8,1,11,2,8,1,15,3,41,548,4,28,2147484253,1,60,0,60,1,60,2,33,0,15,1,6,0,16,0,1,1,16,0,2,2,15,1,7,3,8,1,6,0,12,461,1,25,0,1,3,9,3,9,1,0,9,2,1,3,227,3,32,0,10,839,0,2,4,125,30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,41,626,0,254,0,1,60,0,33,0,3,62,1,32,0,10,840,0,2,4,125], key:0}}, clause_keys:[0], next_key:1, key:497}, 498: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,19,178,0,25,0,2,25,0,1,8,0,3,0,3,499,4,9,2,0,8,0,1,1,9,3,2,8,0,0,3,3,500,4,12,178,0,25,0,0,25,0,1,2,4,96], key:0}}, clause_keys:[0], next_key:1, key:498}, 499: {is_public:false, clauses:{0:{code:[254,0,15,1,3,0,8,1,3,0,7,4,1,4,505], key:0}}, clause_keys:[0], next_key:1, key:499}, 500: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,16,0,2,0,16,0,4,1,16,0,5,2,16,0,3,3,8,0,2,0,8,0,1,1,3,24,6,13,0,27,58,26,0,13,1,25,0,0,26,0,3,9,6,7,10,0,9,0,1,7,11,2,9,1,3,9,2,4,9,3,5,7,12,6,9,4,7,9,5,8,7,13,9,2,41,92,10,28,2147483821,1,60,0,60,1,60,2,33,2,15,1,12,0,15,1,13,1,15,1,14,2,15,1,15,3,16,0,0,4,16,0,1,5,15,1,16,6,15,1,17,7,15,1,18,8,15,1,19,9,8,1,12,0,13,1,25,1,13,25,1,14,8,1,15,2,3,266,3,32,2,9,0,0,9,1,1,2,4,9,30,0,15,1,12,0,15,1,13,1,15,1,14,2,15,1,15,3,15,1,16,4,15,1,17,5,15,1,18,6,15,1,19,7,15,1,20,8,15,1,21,9,8,1,16,0,8,1,19,1,8,1,20,2,8,1,21,3,8,1,13,4,8,1,15,5,8,1,17,6,41,246,7,28,2147483973,1,60,0,60,1,60,2,60,3,60,4,33,4,15,1,9,0,15,1,10,1,15,1,11,2,16,0,0,3,16,0,1,4,16,0,2,5,16,0,3,6,8,1,9,0,8,1,10,1,8,1,11,2,8,0,0,3,3,523,5,32,4,9,0,0,9,1,1,9,2,2,9,3,3,2,4,446,30,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,15,1,14,6,8,1,10,0,8,1,12,1,8,1,13,2,8,1,14,3,4,446], key:0}}, clause_keys:[0], next_key:1, key:500}, 501: {is_public:false, clauses:{0:{code:[254,0,15,1,2,0,13,0,25,1,2,23,1,3,4,456], key:0}}, clause_keys:[0], next_key:1, key:501}, 502: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,0,0,16,0,1,1,8,0,2,0,40,51,1,3,12,214,2,25,0,0,25,0,1,13,1,24,1,2,25,0,2,12,506,0,24,1,1,2,4,230,28,2147483724,1,60,0,33,0,15,1,3,0,12,506,0,25,1,3,3,232,1,32,0,2,4,62,30,0,15,1,2,0,8,1,2,0,10,0,1,4,9], key:0}}, clause_keys:[0], next_key:1, key:502}, 503: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,2,1,5], key:0}, 1:{code:[30,0,1,60,0,60,1,20,0,23,1,4,25,0,0,16,0,1,1,8,0,1,0,8,1,4,1,3,99,2,9,0,0,9,1,1,2,4,503], key:1}}, clause_keys:[0, 1], next_key:2, key:503}, 505: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,3,0,15,1,4,1,12,214,5,25,1,3,25,1,4,13,0,24,1,5,23,1,6,3,455,1,32,0,2,5], key:0}, 1:{code:[30,0,17,630,0,17,529,1,5], key:1}}, clause_keys:[0, 1], next_key:2, key:505}, 507: {is_public:false, clauses:{0:{code:[254,0,1,60,0,15,1,3,0,15,1,4,1,12,214,6,25,1,3,25,1,4,13,5,24,1,6,25,0,0,12,506,0,24,1,5,3,232,1,12,506,0,25,0,0,2,4,230], key:0}}, clause_keys:[0], next_key:1, key:507}, 508: {is_public:false, clauses:{0:{code:[28,1,1,60,0,19,504,0,25,0,0,16,0,0,1,31,9,0,0,7,4,1,7,5,2,7,6,3,2,41,32,4,28,2147483719,1,60,0,33,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,8,1,6,0,8,1,7,1,3,511,1,32,0,2,4,62,30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,8,1,6,0,8,1,9,1,41,100,2,28,2147483778,1,60,0,33,0,15,1,4,0,15,1,5,1,8,1,4,0,10,2,1,3,9,1,32,0,2,4,62,30,0,1,60,0,15,1,3,0,16,0,0,1,8,1,3,0,8,0,0,1,3,448,1,9,0,0,2,4,512], key:0}, 1:{code:[30,0,1,60,0,60,1,16,0,1,0,15,1,4,1,8,0,1,0,8,1,4,1,3,451,2,8,0,1,0,8,0,0,1,3,191,2,9,0,0,9,1,1,2,41,47,2,28,2147483722,1,60,0,33,0,15,1,4,0,15,1,5,1,8,1,4,0,3,192,1,32,0,2,4,62,30,0,15,1,3,0,15,1,4,1,8,1,4,0,4,513], key:1}}, clause_keys:[0, 1], next_key:2, key:508}, 512: {is_public:false, clauses:{0:{code:[254,0,15,1,2,0,13,0,25,1,2,26,0,4,193], key:0}}, clause_keys:[0], next_key:1, key:512}, 513: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,15,1,3,0,8,1,3,0,8,0,0,1,3,191,2,8,0,0,0,3,332,2,8,0,0,0,10,356,1,8,0,1,2,3,235,2,8,0,1,0,3,620,2,9,1,0,3,236,2,8,0,0,0,3,333,1,12,621,0,25,0,0,3,264,1,12,621,0,25,0,0,2,4,175], key:0}}, clause_keys:[0], next_key:1, key:513}, 515: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,19,504,0,25,0,0,16,0,1,1,31,7,3,0,12,514,1,25,0,0,24,1,3,9,1,2,2,4,233], key:0}, 1:{code:[30,0,1,60,0,60,1,15,1,4,0,16,0,1,1,8,1,4,0,8,0,0,1,3,451,2,7,3,0,12,514,1,25,0,0,24,1,3,9,1,2,2,4,233], key:1}}, clause_keys:[0, 1], next_key:2, key:515}, 516: {is_public:false, clauses:{0:{code:[254,0,1,60,0,16,0,0,0,17,541,1,8,0,0,0,3,491,1,9,0,0,7,3,1,7,4,2,2,41,32,3,28,2147483732,1,60,0,60,1,60,2,33,2,15,1,5,0,16,0,0,1,16,0,1,2,8,1,5,0,8,0,0,1,3,515,3,32,2,8,0,1,0,3,499,2,9,0,0,9,1,1,2,4,503,30,0,15,1,4,0,15,1,5,1,15,1,6,2,12,517,0,25,1,4,4,64], key:0}}, clause_keys:[0], next_key:1, key:516}, 519: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,2,1,5], key:0}, 1:{code:[30,0,20,0,23,1,4,23,1,5,16,1,4,1,8,1,5,0,8,1,4,1,4,519], key:1}}, clause_keys:[0, 1], next_key:2, key:519}, 520: {is_public:false, clauses:{0:{code:[254,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,5,0,8,1,6,1,8,1,7,2,41,29,3,28,2147483705,15,1,4,0,15,1,5,1,15,1,6,2,8,1,4,0,8,1,5,1,8,1,6,2,4,458,30,0,1,60,0,60,1,60,2,16,0,2,0,16,0,1,1,16,0,0,2,8,0,2,0,40,120,1,3,9,2,0,8,0,1,1,8,0,0,2,40,151,3,3,8,0,0,0,9,1,1,3,453,2,9,0,0,10,341,1,2,4,519,28,2147483791,1,60,0,33,0,15,1,3,0,8,1,3,0,3,3,1,32,0,2,4,61,30,0,15,1,2,0,4,62,28,2147483838,1,60,0,33,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,5,0,8,1,6,1,8,1,7,2,3,458,1,32,0,2,4,61,30,0,15,1,4,0,15,1,5,1,15,1,6,2,4,62], key:0}}, clause_keys:[0], next_key:1, key:520}, 522: {is_public:false, clauses:{0:{code:[254,0,12,521,0,23,1,1,4,264], key:0}}, clause_keys:[0], next_key:1, key:522}, 523: {is_public:false, clauses:{0:{code:[254,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,8,1,8,0,7,10,1,8,1,6,2,8,1,7,3,7,11,4,8,1,9,5,41,43,6,28,2147483782,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,16,0,1,0,16,0,2,1,16,0,4,2,16,0,5,3,16,0,0,4,16,0,3,5,8,0,1,0,8,0,2,1,3,457,7,8,0,2,0,12,178,1,25,0,4,25,0,5,3,511,7,32,6,9,1,0,9,2,1,9,3,2,9,4,3,9,5,4,2,41,170,5,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,15,1,12,5,8,1,7,0,8,1,12,1,4,9,28,2147483870,1,60,0,60,1,60,2,33,2,15,1,7,0,16,0,0,1,16,0,1,2,15,1,8,3,15,1,9,4,8,1,7,0,10,2,1,3,9,3,32,2,9,0,0,9,1,1,2,4,9,30,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,15,1,10,4,8,1,9,0,8,1,10,1,8,1,7,2,8,1,8,3,4,525], key:0}}, clause_keys:[0], next_key:1, key:523}, 524: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,33,2,16,0,3,0,16,0,4,1,16,0,1,2,8,0,0,0,3,499,5,9,3,0,9,4,1,8,0,0,2,8,0,1,3,3,523,5,32,2,9,0,0,9,1,1,2,4,227], key:0}}, clause_keys:[0], next_key:1, key:524}, 525: {is_public:false, clauses:{0:{code:[254,0,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,8,1,8,0,8,1,9,1,8,1,10,2,8,1,11,3,4,523], key:0}}, clause_keys:[0], next_key:1, key:525}, 528: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,33,6,16,0,7,0,16,0,1,1,16,0,2,2,16,0,3,3,16,0,4,4,16,0,5,5,12,178,0,26,906,27,2,3,29,8,8,0,7,0,8,0,0,1,3,543,8,8,0,0,0,9,7,1,3,227,8,32,6,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,528], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,19,188,0,25,0,5,25,0,0,16,0,6,1,19,188,2,25,0,7,25,0,1,16,0,8,3,16,0,3,4,16,0,4,5,31,9,5,0,9,6,1,9,7,2,9,8,3,8,0,2,4,8,0,4,5,3,528,9,9,0,0,10,908,1,9,1,2,9,2,3,9,3,4,9,4,5,2,4,528], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,19,544,0,25,0,3,15,1,6,1,19,545,2,23,1,7,25,0,2,20,3,23,1,8,25,0,5,19,546,8,24,1,7,25,0,2,23,1,9,23,1,10,19,547,9,23,1,11,19,188,10,25,0,4,23,1,12,19,548,12,24,1,11,16,0,6,4,16,0,0,5,31,8,0,3,0,10,908,1,9,4,2,9,5,3,9,6,4,8,0,0,5,3,528,7,9,3,0,8,0,1,1,3,63,4,9,0,0,9,1,1,9,2,2,2,4,549], key:2}, 3:{code:[29,4,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,60,15,60,16,60,17,60,18,60,19,60,20,60,21,19,550,0,25,0,16,25,0,7,25,0,3,25,0,4,15,1,6,1,19,188,2,25,0,17,23,1,7,19,188,7,23,1,8,23,1,9,19,551,8,23,1,10,19,552,10,23,1,11,19,545,9,25,0,20,23,1,12,20,12,24,1,11,25,0,6,16,0,21,3,16,0,10,4,16,0,0,5,31,9,21,0,13,3,23,1,4,25,0,6,12,553,7,25,0,19,23,1,8,12,551,6,24,1,7,12,545,10,23,1,11,26,0,12,554,14,23,1,15,12,551,13,24,1,14,12,545,17,23,1,18,25,0,11,12,555,20,27,0,23,1,21,12,556,24,24,1,4,24,1,15,12,551,23,24,1,24,13,29,24,1,8,25,0,2,13,28,24,1,21,24,1,29,13,27,24,1,15,24,1,28,12,545,25,23,1,26,24,1,27,12,188,22,24,1,23,24,1,25,12,188,19,24,1,20,24,1,22,12,188,16,24,1,17,24,1,19,12,188,12,24,1,13,24,1,16,12,188,9,24,1,10,24,1,12,12,188,5,24,1,6,24,1,9,12,546,2,25,0,20,24,1,3,26,919,24,1,5,13,32,24,1,4,25,0,6,12,547,33,23,1,34,12,557,37,24,1,4,12,551,36,24,1,37,12,558,40,23,1,41,12,551,39,24,1,40,12,548,43,24,1,34,12,198,47,24,1,41,12,559,46,25,0,3,24,1,47,12,551,45,24,1,46,12,551,49,26,249,12,188,48,25,0,8,24,1,49,12,188,44,24,1,45,24,1,48,12,188,42,24,1,43,24,1,44,12,188,38,24,1,39,24,1,42,12,188,35,24,1,36,24,1,38,12,546,31,25,0,20,24,1,32,24,1,33,24,1,35,13,52,23,1,53,25,0,6,12,547,54,24,1,34,12,559,57,25,0,3,26,125,12,551,56,24,1,57,12,551,59,26,241,12,548,62,24,1,34,12,551,63,26,125,12,188,61,24,1,62,24,1,63,12,188,60,25,0,8,24,1,61,12,188,58,24,1,59,24,1,60,12,188,55,24,1,56,24,1,58,12,546,51,25,0,20,24,1,52,24,1,54,24,1,55,12,546,65,24,1,18,25,0,11,25,0,12,25,0,14,13,71,24,1,8,25,0,2,13,70,24,1,4,24,1,71,13,68,23,1,69,24,1,70,12,559,74,24,1,69,24,1,4,12,551,73,24,1,74,12,559,77,25,0,3,26,402,12,551,76,24,1,77,12,560,80,24,1,8,12,551,79,24,1,80,12,188,78,24,1,79,25,0,8,12,188,75,24,1,76,24,1,78,12,188,72,24,1,73,24,1,75,12,546,67,24,1,26,24,1,68,26,919,24,1,72,12,551,84,26,127,12,546,82,24,1,26,23,1,83,26,919,24,1,84,12,559,89,25,0,3,26,403,12,551,88,24,1,89,12,551,91,26,33,12,188,90,25,0,8,24,1,91,12,188,87,24,1,88,24,1,90,12,546,86,24,1,11,25,0,19,26,919,24,1,87,13,85,24,1,86,25,0,18,13,81,24,1,82,24,1,85,13,66,24,1,67,24,1,81,13,64,24,1,65,24,1,66,13,50,24,1,51,24,1,64,13,30,24,1,31,24,1,50,13,1,24,1,2,24,1,30,3,9,22,12,561,0,25,0,20,3,196,21,12,214,0,25,0,3,25,0,4,9,19,1,3,63,20,9,16,0,10,908,1,9,17,2,9,18,3,8,0,15,4,8,0,0,5,3,528,19,8,0,7,0,10,908,1,9,14,2,9,15,3,8,0,9,4,8,0,13,5,3,528,16,9,12,0,9,13,1,3,562,14,8,0,7,0,9,11,1,3,63,12,8,0,4,0,10,908,1,9,8,2,9,9,3,9,10,4,8,0,0,5,3,528,11,12,214,2,25,0,7,25,0,3,12,214,0,24,1,2,25,0,4,8,0,5,1,3,63,8,8,0,0,0,9,5,1,9,6,2,3,549,7,12,214,0,25,0,3,25,0,4,8,0,1,1,3,63,5,9,0,0,9,1,1,9,2,2,2,4,549], key:3}, 4:{code:[29,5,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,19,563,0,25,0,3,23,1,12,25,0,4,15,1,13,1,19,188,2,23,1,14,23,1,15,19,551,14,23,1,16,19,552,16,23,1,17,19,545,15,23,1,18,23,1,19,20,19,24,1,17,23,1,20,20,20,24,1,12,25,0,2,20,3,23,1,21,23,1,22,19,546,21,24,1,18,23,1,23,26,919,23,1,24,20,23,23,1,25,23,1,26,20,26,23,1,27,25,0,2,19,188,24,23,1,28,23,1,29,19,551,28,23,1,30,19,554,30,23,1,31,19,188,29,23,1,32,23,1,33,19,551,33,23,1,34,19,556,34,24,1,25,24,1,31,20,22,23,1,35,23,1,36,19,546,35,24,1,18,23,1,37,26,919,23,1,38,20,37,23,1,39,23,1,40,20,40,24,1,12,25,0,2,19,188,38,23,1,41,23,1,42,19,551,41,23,1,43,19,557,43,24,1,39,19,188,42,23,1,44,23,1,45,19,551,44,23,1,46,19,558,46,23,1,47,19,545,45,23,1,48,23,1,49,20,49,24,1,47,23,1,50,20,50,24,1,12,25,0,2,20,36,23,1,51,23,1,52,19,546,51,24,1,48,23,1,53,23,1,54,23,1,55,20,53,24,1,47,23,1,56,20,56,24,1,12,25,0,2,19,547,54,23,1,57,19,188,55,23,1,58,23,1,59,19,551,58,23,1,60,19,559,60,24,1,47,24,1,12,19,188,59,23,1,61,23,1,62,19,548,61,24,1,57,19,188,62,23,1,63,25,0,5,19,551,63,26,257,20,52,23,1,64,23,1,65,19,546,64,24,1,48,23,1,66,26,919,23,1,67,20,66,23,1,68,23,1,69,20,69,23,1,70,25,0,2,19,551,67,26,249,16,0,7,4,16,0,0,5,8,0,3,0,10,908,1,8,1,32,2,8,1,65,3,8,0,6,4,8,0,0,5,3,528,8,8,0,4,0,10,908,1,9,5,2,9,6,3,9,7,4,8,0,0,5,3,528,8,12,214,0,25,0,3,25,0,4,8,0,1,1,3,63,5,9,0,0,9,1,1,9,2,2,2,4,549], key:4}, 5:{code:[29,6,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,19,564,0,25,0,3,25,0,4,16,0,9,1,16,0,10,2,20,3,23,1,6,25,0,11,19,546,6,25,0,8,25,0,2,26,919,25,0,5,16,0,7,4,16,0,0,5,31,12,545,6,25,0,8,25,0,2,12,353,0,24,1,6,9,9,1,9,10,2,9,11,3,8,0,6,4,8,0,0,5,3,528,12,12,353,6,25,0,4,12,188,0,25,0,3,24,1,6,10,908,1,9,5,2,9,6,3,9,7,4,8,0,0,5,3,528,8,12,214,0,25,0,3,25,0,4,8,0,1,1,3,63,5,9,0,0,9,1,1,9,2,2,2,4,549], key:5}, 6:{code:[29,7,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,19,565,0,23,1,6,25,0,6,19,566,6,25,0,11,25,0,9,15,1,7,1,19,545,2,23,1,8,25,0,2,20,3,23,1,9,23,1,10,19,546,9,24,1,8,25,0,2,23,1,11,23,1,12,19,547,11,23,1,13,19,188,12,25,0,3,23,1,14,19,188,14,23,1,15,25,0,4,19,548,15,24,1,13,20,10,23,1,16,25,0,12,19,546,16,24,1,8,25,0,2,26,919,25,0,5,16,0,8,4,16,0,0,5,31,9,11,0,10,908,1,8,0,3,2,9,12,3,8,0,10,4,8,0,0,5,3,528,13,9,9,0,10,908,1,8,0,4,2,9,10,3,8,0,7,4,8,0,0,5,3,528,11,9,6,0,10,908,1,8,0,5,2,9,7,3,9,8,4,8,0,0,5,3,528,9,12,214,2,25,0,3,25,0,4,12,214,0,24,1,2,25,0,5,8,0,1,1,3,63,6,9,0,0,9,1,1,9,2,2,2,4,549], key:6}, 7:{code:[29,8,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,19,566,0,25,0,8,25,0,5,15,1,6,1,19,545,2,23,1,7,25,0,2,20,3,23,1,8,25,0,9,19,546,8,24,1,7,25,0,2,23,1,9,23,1,10,19,547,9,23,1,11,19,188,10,25,0,3,23,1,12,19,188,12,23,1,13,25,0,4,19,548,13,24,1,11,16,0,7,4,16,0,0,5,31,9,8,0,10,908,1,8,0,3,2,9,9,3,8,0,6,4,8,0,0,5,3,528,10,9,5,0,10,908,1,8,0,4,2,9,6,3,9,7,4,8,0,0,5,3,528,8,12,214,0,25,0,3,25,0,4,8,0,1,1,3,63,5,9,0,0,9,1,1,9,2,2,2,4,549], key:7}, 8:{code:[29,9,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,19,565,0,25,0,3,25,0,4,15,1,6,1,19,545,2,23,1,7,25,0,2,20,3,23,1,8,23,1,9,19,546,8,24,1,7,25,0,2,26,919,25,0,8,20,9,23,1,10,25,0,9,19,546,10,24,1,7,25,0,2,26,919,25,0,5,16,0,7,4,16,0,0,5,31,8,0,3,0,10,908,1,9,8,2,9,9,3,8,0,6,4,8,0,0,5,3,528,10,8,0,4,0,10,908,1,9,5,2,9,6,3,9,7,4,8,0,0,5,3,528,8,12,214,0,25,0,3,25,0,4,8,0,1,1,3,63,5,9,0,0,9,1,1,9,2,2,2,4,549], key:8}, 9:{code:[29,10,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,19,353,0,25,0,3,15,1,6,1,19,545,2,23,1,7,25,0,2,20,3,23,1,8,23,1,9,19,546,8,24,1,7,25,0,2,23,1,10,23,1,11,19,547,10,23,1,12,19,188,11,25,0,4,23,1,13,19,188,13,23,1,14,23,1,15,19,548,14,24,1,12,19,551,15,26,125,20,9,23,1,16,25,0,5,19,546,16,24,1,7,25,0,2,26,919,23,1,17,19,551,17,26,127,16,0,6,4,15,1,18,5,31,8,0,3,0,10,908,1,9,4,2,9,5,3,9,6,4,8,0,0,5,3,528,7,9,3,0,8,0,1,1,3,63,4,9,0,0,9,1,1,9,2,2,2,4,549], key:9}, 10:{code:[29,11,17,403,0,17,908,1,19,548,2,23,1,6,15,1,7,3,16,1,7,4,19,542,5,24,1,6,31,5], key:10}, 11:{code:[29,12,19,545,0,23,1,6,23,1,7,15,1,8,1,19,545,2,24,1,6,24,1,7,15,1,9,3,16,1,9,4,15,1,10,5,31,5], key:11}, 12:{code:[29,13,19,551,0,23,1,6,15,1,7,1,19,551,2,24,1,6,15,1,8,3,16,1,8,4,15,1,9,5,31,5], key:12}, 13:{code:[30,0,15,1,6,0,15,1,7,1,19,551,2,25,1,6,15,1,8,3,16,1,8,4,15,1,9,5,5], key:13}}, clause_keys:[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13], next_key:14, key:528}, 529: {is_public:false, clauses:{0:{code:[28,1,17,902,0,31,5], key:0}, 1:{code:[30,0,19,542,0,23,1,1,5], key:1}}, clause_keys:[0, 1], next_key:2, key:529}, 530: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,33,3,15,1,5,0,16,0,2,1,8,1,5,0,8,0,0,1,8,0,1,2,3,570,4,32,3,9,0,0,9,1,1,9,2,2,2,4,571], key:0}, 1:{code:[30,0,15,1,2,0,16,1,2,1,5], key:1}}, clause_keys:[0, 1], next_key:2, key:530}, 532: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,16,0,4,0,19,188,1,25,0,5,25,0,6,16,0,3,2,31,12,214,3,25,0,4,25,0,5,12,188,0,24,1,3,25,0,6,10,0,1,8,0,1,2,3,572,7,8,0,1,0,8,0,2,1,3,453,4,8,0,1,0,8,0,0,1,3,63,4,9,0,0,14,0,1,7,6,2,9,1,3,9,2,4,9,3,5,2,4,573], key:0}, 1:{code:[30,0,15,1,3,0,15,1,4,1,17,0,2,31,5], key:1}}, clause_keys:[0, 1], next_key:2, key:532}, 533: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,16,0,0,0,19,188,1,23,1,3,23,1,4,16,0,1,2,31,9,0,0,9,1,1,2,4,453], key:0}, 1:{code:[29,2,1,60,0,60,1,33,0,15,1,6,0,19,565,1,23,1,7,23,1,8,16,0,1,2,8,1,6,0,8,1,7,1,8,0,1,2,3,533,2,9,1,0,10,529,1,3,213,2,32,0,2,5], key:1}, 2:{code:[29,3,1,60,0,60,1,33,0,15,1,6,0,19,565,1,23,1,7,23,1,8,16,0,1,2,8,1,6,0,8,1,8,1,8,0,1,2,3,533,2,9,1,0,10,529,1,3,213,2,32,0,2,5], key:2}, 3:{code:[29,4,1,60,0,60,1,33,0,15,1,5,0,15,1,6,1,16,0,1,2,8,1,5,0,8,0,1,1,3,453,2,9,1,0,14,0,1,3,35,2,32,0,2,5], key:3}, 4:{code:[30,0,15,1,3,0,15,1,4,1,17,529,2,5], key:4}}, clause_keys:[0, 1, 2, 3, 4], next_key:5, key:533}, 535: {is_public:false, clauses:{0:{code:[28,1,17,529,0,15,1,7,1,15,1,8,2,15,1,9,3,16,1,9,4,15,1,10,5,16,1,10,6,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,15,1,7,0,17,902,1,16,0,0,2,20,3,26,711,25,0,1,16,0,2,4,16,0,3,5,16,0,4,6,31,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,577], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,60,3,33,0,15,1,12,0,19,542,1,25,0,1,16,0,2,2,20,3,26,711,23,1,13,15,1,14,4,15,1,15,5,20,6,23,1,16,23,1,17,19,578,16,25,0,1,26,945,25,0,3,8,0,2,0,8,1,13,1,12,444,18,25,0,3,13,2,24,1,18,25,1,14,8,1,15,3,8,1,17,4,3,577,4,9,1,0,9,2,1,9,3,2,3,579,4,32,0,2,5], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:535}, 536: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,19,188,0,25,0,0,23,1,2,16,0,1,1,31,9,0,0,9,1,1,2,4,536], key:0}, 1:{code:[30,0,15,1,5,0,15,1,6,1,8,1,5,0,7,7,1,8,1,6,2,4,11], key:1}}, clause_keys:[0, 1], next_key:2, key:536}, 537: {is_public:false, clauses:{0:{code:[254,0,15,1,18,0,15,1,19,1,15,1,20,2,15,1,21,3,15,1,22,4,15,1,23,5,15,1,24,6,15,1,25,7,8,1,18,0,8,1,19,1,14,0,2,10,980,3,8,1,20,4,8,1,21,5,8,1,22,6,8,1,23,7,8,1,24,8,8,1,25,9,4,597], key:0}}, clause_keys:[0], next_key:1, key:537}, 538: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,4,1,15,1,5,2,16,1,5,3,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,60,15,60,16,60,17,60,18,20,0,23,1,6,25,0,0,19,546,6,23,1,7,25,0,9,25,0,18,23,1,8,16,0,1,1,20,2,23,1,9,25,0,13,19,399,9,24,1,7,16,0,3,3,8,1,8,0,8,0,4,1,3,530,19,9,18,0,8,0,12,1,3,562,19,12,214,0,25,0,9,25,0,12,8,0,4,1,8,0,5,2,3,532,18,8,0,5,0,8,0,4,1,8,0,6,2,3,533,18,8,0,9,0,8,0,17,1,3,453,18,8,0,4,0,8,0,16,1,3,536,18,9,17,0,8,0,12,1,8,0,15,2,3,568,18,8,0,14,0,12,270,1,25,0,15,25,0,16,3,34,17,8,0,6,0,9,12,1,8,0,5,2,9,13,3,8,0,11,4,12,534,7,25,0,14,13,5,24,1,7,26,0,8,0,10,6,3,535,15,9,9,0,9,10,1,8,0,7,2,14,0,3,8,0,5,4,9,11,5,8,0,8,6,3,569,12,9,4,0,8,0,1,1,9,5,2,9,6,3,9,7,4,7,8,5,9,8,6,8,0,2,7,3,537,9,9,0,0,9,1,1,9,2,2,9,3,3,2,4,538], key:1}}, clause_keys:[0, 1], next_key:2, key:538}, 539: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,16,0,9,0,16,0,8,1,15,1,15,2,16,0,3,3,15,1,16,4,16,0,2,5,15,1,17,6,16,0,5,7,8,1,16,0,8,1,15,1,8,0,3,2,8,1,17,3,8,0,4,4,12,534,18,25,0,6,13,5,24,1,18,26,0,8,0,1,6,3,535,10,9,9,0,13,1,23,1,2,25,0,0,3,13,10,8,0,0,0,8,0,7,1,3,453,9,9,6,0,12,270,1,25,0,7,25,0,8,3,34,9,9,0,0,9,1,1,9,2,2,14,0,3,9,3,4,9,4,5,9,5,6,2,4,569], key:0}}, clause_keys:[0], next_key:1, key:539}, 541: {is_public:false, clauses:{0:{code:[28,1,20,0,23,1,2,23,1,3,19,534,2,23,1,4,16,1,4,1,31,5], key:0}, 1:{code:[30,0,20,0,23,1,4,23,1,5,15,1,6,1,8,1,5,0,8,1,6,1,4,541], key:1}}, clause_keys:[0, 1], next_key:2, key:541}, 549: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,33,2,15,1,4,0,16,0,1,1,16,0,0,2,8,1,4,0,3,3,3,32,2,9,0,0,9,1,1,2,4,9], key:0}, 1:{code:[29,2,1,60,0,33,0,19,542,0,23,1,5,15,1,6,1,16,1,6,2,8,1,5,0,8,1,6,1,3,567,1,32,0,2,5], key:1}, 2:{code:[30,0,19,542,0,23,1,3,15,1,4,1,20,2,24,1,3,25,1,4,5], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:549}, 562: {is_public:false, clauses:{0:{code:[28,1,17,919,0,17,902,1,31,5], key:0}, 1:{code:[30,0,19,547,0,23,1,2,19,542,1,24,1,2,5], key:1}}, clause_keys:[0, 1], next_key:2, key:562}, 567: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,4,0,20,1,23,1,5,23,1,6,8,1,5,0,8,1,4,1,3,10,1,32,0,2,5], key:0}, 1:{code:[30,0,15,1,4,0,20,1,23,1,5,23,1,6,8,1,4,0,8,1,6,1,4,567], key:1}}, clause_keys:[0, 1], next_key:2, key:567}, 568: {is_public:false, clauses:{0:{code:[28,1,15,1,3,0,17,902,1,16,1,3,2,31,5], key:0}, 1:{code:[30,0,15,1,5,0,19,542,1,23,1,6,15,1,7,2,8,1,7,0,12,270,1,25,1,5,27,1,4,34], key:1}}, clause_keys:[0, 1], next_key:2, key:568}, 569: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,7,1,16,1,7,2,15,1,8,3,15,1,9,4,15,1,10,5,16,1,10,6,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,20,0,23,1,14,25,0,0,15,1,15,1,16,0,2,2,16,0,7,3,16,0,4,4,15,1,16,5,16,0,6,6,8,1,14,0,8,1,15,1,8,0,1,2,12,398,3,25,0,7,8,0,4,4,8,1,16,5,8,0,5,6,3,582,8,8,0,3,0,12,270,1,25,0,7,27,1,3,34,8,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,2,4,569], key:1}}, clause_keys:[0, 1], next_key:2, key:569}, 570: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,19,188,0,23,1,6,23,1,7,19,188,1,24,1,6,23,1,8,15,1,9,2,8,1,7,0,8,1,8,1,8,1,9,2,3,570,1,32,0,2,5], key:0}, 1:{code:[30,0,19,188,0,23,1,3,23,1,4,16,1,3,1,16,1,4,2,31,5], key:1}}, clause_keys:[0, 1], next_key:2, key:570}, 571: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,19,188,0,25,0,3,25,0,0,16,0,1,1,16,0,4,2,31,9,3,0,8,0,2,1,9,4,2,3,571,5,9,0,0,9,1,1,9,2,2,2,4,571], key:0}, 1:{code:[30,0,15,1,3,0,15,1,4,1,19,188,2,25,1,3,25,1,4,5], key:1}}, clause_keys:[0, 1], next_key:2, key:571}, 572: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,33,4,19,188,0,23,1,5,25,0,0,16,0,2,1,16,0,3,2,8,1,5,0,8,0,1,1,3,63,5,32,4,9,0,0,13,1,25,0,1,25,0,2,9,3,2,2,4,572], key:0}, 1:{code:[30,0,15,1,5,0,15,1,6,1,20,2,23,1,7,25,1,6,8,1,5,0,8,1,7,1,4,63], key:1}}, clause_keys:[0, 1], next_key:2, key:572}, 573: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,6,1,16,1,6,2,15,1,7,3,15,1,8,4,17,0,5,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,33,7,20,0,23,1,12,25,0,0,16,0,6,1,16,0,2,2,16,0,3,3,16,0,4,4,20,5,23,1,13,25,0,5,19,574,13,24,1,12,23,1,14,23,1,15,19,396,14,25,0,6,19,575,15,25,0,8,8,1,12,0,14,0,1,8,0,3,2,14,0,3,10,0,4,8,0,9,5,3,576,10,9,8,0,12,214,2,25,0,4,25,0,9,12,214,1,24,1,2,27,1,3,34,10,32,7,8,0,1,0,12,270,1,25,0,6,27,1,3,34,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,573], key:1}, 2:{code:[30,0,20,0,23,1,12,23,1,13,15,1,14,1,15,1,15,2,15,1,16,3,15,1,17,4,15,1,18,5,8,1,13,0,8,1,14,1,8,1,15,2,8,1,16,3,8,1,17,4,8,1,18,5,4,573], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:573}, 576: {is_public:false, clauses:{0:{code:[28,1,15,1,6,0,21,2,1,15,1,7,2,15,1,8,3,20,4,23,1,9,23,1,10,20,10,23,1,11,26,0,16,1,11,5,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,33,8,16,0,0,0,16,0,7,1,20,2,23,1,8,25,0,2,16,0,4,3,16,0,5,4,16,0,6,5,8,0,0,0,8,1,8,1,3,567,9,32,8,8,0,1,0,12,270,1,25,0,7,27,1,3,34,8,8,0,3,0,12,270,1,25,0,4,27,1,3,34,7,9,0,0,9,1,1,9,2,2,9,3,3,13,4,25,0,4,25,0,5,9,6,5,2,4,576], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,16,0,0,0,16,0,1,1,20,2,23,1,8,25,0,2,15,1,9,3,16,0,4,4,16,0,5,5,8,0,3,0,12,270,1,25,1,9,27,1,3,34,6,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,576], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:576}, 577: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,5,1,16,1,5,2,15,1,6,3,16,1,6,4,31,5], key:0}, 1:{code:[30,0,20,0,23,1,10,23,1,11,19,574,10,23,1,12,23,1,13,23,1,14,19,396,13,23,1,15,20,1,23,1,16,23,1,17,19,417,16,23,1,18,19,396,18,24,1,15,15,1,19,2,15,1,20,3,15,1,21,4,8,1,11,0,8,1,17,1,8,1,19,2,12,396,23,24,1,15,12,578,22,24,1,12,26,947,24,1,23,13,3,24,1,22,25,1,20,8,1,21,4,4,577], key:1}}, clause_keys:[0, 1], next_key:2, key:577}, 579: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,5,0,20,1,23,1,6,23,1,7,19,574,6,23,1,8,23,1,9,23,1,10,16,1,9,2,8,1,5,0,8,1,8,1,3,10,1,32,0,2,5], key:0}, 1:{code:[30,0,15,1,6,0,20,1,23,1,7,23,1,8,15,1,9,2,8,1,6,0,8,1,8,1,8,1,9,2,4,579], key:1}}, clause_keys:[0, 1], next_key:2, key:579}, 580: {is_public:false, clauses:{0:{code:[28,1,17,529,0,15,1,3,1,16,1,3,2,31,5], key:0}, 1:{code:[30,0,15,1,3,0,20,1,26,712,23,1,4,16,1,4,2,5], key:1}}, clause_keys:[0, 1], next_key:2, key:580}, 581: {is_public:false, clauses:{0:{code:[28,1,15,1,3,0,8,1,3,0,10,0,1,4,10], key:0}, 1:{code:[30,0,15,1,2,0,8,1,2,0,4,4], key:1}}, clause_keys:[0, 1], next_key:2, key:581}, 582: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,8,0,15,1,9,1,16,1,9,2,15,1,10,3,15,1,11,4,20,5,23,1,12,23,1,13,19,429,12,25,1,8,25,1,10,16,1,13,6,8,1,8,0,3,581,1,32,0,2,5], key:0}, 1:{code:[29,2,1,60,0,33,0,15,1,8,0,15,1,9,1,16,1,9,2,15,1,10,3,15,1,11,4,20,5,23,1,12,23,1,13,19,433,12,25,1,8,25,1,10,16,1,13,6,8,1,8,0,3,5,1,32,0,2,5], key:1}, 2:{code:[29,3,1,60,0,33,0,15,1,8,0,15,1,9,1,16,1,9,2,15,1,10,3,15,1,11,4,20,5,23,1,12,23,1,13,19,434,12,25,1,8,25,1,10,16,1,13,6,8,1,8,0,3,6,1,32,0,2,5], key:2}, 3:{code:[29,4,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,33,7,16,0,0,0,16,0,3,1,16,0,4,2,16,0,1,3,16,0,2,4,16,0,5,5,16,0,6,6,8,0,0,0,3,3,8,32,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,2,4,583], key:3}, 4:{code:[29,5,1,60,0,60,1,60,2,60,3,60,4,60,5,20,0,23,1,14,23,1,15,15,1,16,1,16,0,2,2,15,1,17,3,16,0,3,4,20,5,23,1,18,23,1,19,19,432,18,25,1,17,16,0,5,6,13,20,24,1,15,26,0,13,0,24,1,14,24,1,20,8,1,16,1,8,0,1,2,8,0,3,3,8,1,19,4,8,0,4,5,8,0,0,6,3,584,6,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,585], key:4}, 5:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,15,1,9,0,16,0,7,1,16,0,2,2,15,1,10,3,16,0,3,4,20,5,23,1,11,25,0,8,19,431,11,23,1,12,25,1,10,19,178,12,23,1,13,25,0,9,16,0,5,6,8,1,9,0,13,1,24,1,13,25,0,6,3,13,10,8,0,6,0,9,9,1,3,453,10,9,6,0,9,7,1,8,0,1,2,8,0,3,3,9,8,4,8,0,4,5,8,0,0,6,3,584,9,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,585], key:5}}, clause_keys:[0, 1, 2, 3, 4, 5], next_key:6, key:582}, 583: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,16,1,14,4,20,5,23,1,15,23,1,16,19,428,15,23,1,17,25,1,12,16,1,16,6,8,1,11,0,8,1,14,1,8,1,17,2,7,18,3,3,589,1,32,0,2,5], key:0}, 1:{code:[29,2,1,60,0,33,0,15,1,10,0,15,1,11,1,15,1,12,2,15,1,13,3,20,4,23,1,14,25,1,13,19,578,14,25,1,10,26,945,23,1,15,19,396,15,23,1,16,20,5,23,1,17,23,1,18,19,427,17,23,1,19,25,1,11,19,396,19,24,1,16,16,1,18,6,8,1,10,0,8,1,12,1,12,396,2,24,1,16,3,579,1,32,0,2,5], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,15,1,7,0,15,1,8,1,15,1,9,2,16,0,0,3,20,4,23,1,10,25,0,1,19,578,10,25,1,7,26,945,25,0,2,20,5,23,1,11,23,1,12,19,427,11,25,0,2,25,1,8,16,1,12,6,31,9,0,0,9,1,1,9,2,2,2,4,588], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:583}, 584: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,7,1,16,1,7,2,15,1,8,3,15,1,9,4,16,1,9,5,17,0,6,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,33,7,20,0,23,1,8,25,0,0,16,0,1,1,16,0,2,2,16,0,3,3,20,4,23,1,9,25,0,4,19,439,9,24,1,8,16,0,5,5,16,0,6,6,8,1,8,0,3,581,8,32,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,2,4,584], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,33,7,20,0,23,1,8,25,0,0,16,0,1,1,16,0,2,2,16,0,3,3,20,4,23,1,9,25,0,4,19,440,9,24,1,8,16,0,5,5,16,0,6,6,8,1,8,0,3,5,8,32,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,2,4,584], key:2}, 3:{code:[29,4,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,33,7,20,0,23,1,8,25,0,0,16,0,1,1,16,0,2,2,16,0,3,3,20,4,23,1,9,25,0,4,19,441,9,24,1,8,16,0,5,5,16,0,6,6,8,1,8,0,3,6,8,32,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,2,4,584], key:3}, 4:{code:[29,5,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,33,10,20,0,25,0,7,25,0,0,16,0,8,1,16,0,2,2,16,0,3,3,16,0,9,4,16,0,5,5,16,0,6,6,8,0,7,0,3,3,11,32,10,9,7,0,8,0,3,1,9,8,2,8,0,1,3,9,9,4,8,0,4,5,3,586,10,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,2,4,584], key:4}, 5:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,20,0,23,1,7,25,0,0,16,0,7,1,16,0,2,2,16,0,3,3,20,4,23,1,8,25,0,4,19,436,8,25,0,8,16,0,5,5,20,6,23,1,9,25,0,6,19,587,9,25,0,8,24,1,7,31,9,7,0,8,0,1,1,9,8,2,3,588,9,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,2,4,584], key:5}}, clause_keys:[0, 1, 2, 3, 4, 5], next_key:6, key:584}, 585: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,6,1,16,1,6,2,15,1,7,3,15,1,8,4,16,1,8,5,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,20,0,23,1,13,25,0,0,19,587,13,23,1,14,23,1,15,15,1,16,1,16,0,2,2,16,0,3,3,15,1,17,4,16,0,5,5,8,1,15,0,8,1,16,1,8,0,1,2,8,1,14,3,8,0,3,4,8,1,17,5,8,0,4,6,3,582,6,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,585], key:1}}, clause_keys:[0, 1], next_key:2, key:585}, 586: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,33,7,16,0,3,0,15,1,10,1,16,0,4,2,16,0,5,3,16,0,2,4,16,0,6,5,8,0,3,0,8,0,4,1,8,0,1,2,8,0,0,3,3,589,8,32,7,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,2,4,590], key:0}, 1:{code:[29,2,1,60,0,33,0,15,1,9,0,15,1,10,1,15,1,11,2,20,3,23,1,12,25,1,11,19,578,12,25,1,9,26,961,23,1,13,19,396,13,23,1,14,20,4,23,1,15,23,1,16,19,436,15,23,1,17,19,396,17,24,1,14,16,1,16,5,8,1,9,0,8,1,10,1,12,396,2,24,1,14,3,579,1,32,0,2,5], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,15,1,6,0,15,1,7,1,16,0,0,2,20,3,23,1,8,25,0,1,19,578,8,25,1,6,26,961,25,0,2,20,4,23,1,9,23,1,10,19,436,9,25,0,2,16,1,10,5,31,9,0,0,9,1,1,9,2,2,2,4,588], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:586}, 588: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,20,0,23,1,5,23,1,6,19,534,5,23,1,7,20,1,23,1,8,24,1,6,19,534,8,23,1,9,19,398,2,24,1,7,8,1,9,0,12,270,1,24,1,7,27,1,3,34,1,32,0,2,5], key:0}, 1:{code:[30,0,20,0,23,1,6,23,1,7,20,1,24,1,6,23,1,8,15,1,9,2,8,1,7,0,8,1,8,1,8,1,9,2,4,588], key:1}}, clause_keys:[0, 1], next_key:2, key:588}, 589: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,6,0,20,1,23,1,7,23,1,8,19,578,7,23,1,9,23,1,10,23,1,11,16,1,11,2,16,1,10,3,8,1,9,0,8,1,6,1,3,10,1,32,0,2,5], key:0}, 1:{code:[30,0,15,1,8,0,20,1,23,1,9,23,1,10,15,1,11,2,15,1,12,3,8,1,8,0,8,1,10,1,8,1,11,2,8,1,12,3,4,589], key:1}}, clause_keys:[0, 1], next_key:2, key:589}, 590: {is_public:false, clauses:{0:{code:[28,1,17,961,0,15,1,7,1,20,2,23,1,8,23,1,9,19,437,8,25,1,7,15,1,10,3,15,1,11,4,16,1,11,5,16,1,9,6,31,5], key:0}, 1:{code:[29,2,17,970,0,15,1,7,1,20,2,23,1,8,23,1,9,19,437,8,25,1,7,15,1,10,3,15,1,11,4,16,1,11,5,16,1,9,6,31,5], key:1}, 2:{code:[29,3,17,947,0,19,398,1,23,1,7,20,2,23,1,8,23,1,9,19,437,8,23,1,10,19,398,10,24,1,7,15,1,11,3,15,1,12,4,16,1,12,5,16,1,9,6,31,5], key:2}, 3:{code:[30,0,15,1,10,0,15,1,11,1,20,2,23,1,12,23,1,13,19,438,12,25,1,11,15,1,14,3,15,1,15,4,15,1,16,5,16,1,13,6,8,1,14,0,8,1,15,1,8,1,16,2,4,593], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:590}, 591: {is_public:false, clauses:{0:{code:[28,1,15,1,10,0,15,1,11,1,15,1,12,2,19,398,3,23,1,13,15,1,14,4,20,5,23,1,15,23,1,16,19,418,15,23,1,17,25,1,14,19,398,17,24,1,13,15,1,18,6,15,1,19,7,16,1,19,8,16,1,16,9,5], key:0}, 1:{code:[29,2,17,945,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,20,5,23,1,14,23,1,15,19,418,14,25,1,12,25,1,13,15,1,16,6,15,1,17,7,16,1,17,8,16,1,15,9,5], key:1}, 2:{code:[29,3,17,961,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,20,5,23,1,14,23,1,15,19,418,14,25,1,12,25,1,13,15,1,16,6,15,1,17,7,16,1,17,8,16,1,15,9,5], key:2}, 3:{code:[29,4,1,60,0,33,0,17,947,0,15,1,13,1,15,1,14,2,15,1,15,3,15,1,16,4,20,5,23,1,17,23,1,18,19,418,17,25,1,15,25,1,16,15,1,19,6,15,1,20,7,16,1,20,8,16,1,18,9,8,1,19,0,8,1,14,1,8,1,13,2,3,594,1,32,0,2,5], key:3}, 4:{code:[30,0,17,947,0,15,1,10,1,15,1,11,2,15,1,12,3,15,1,13,4,20,5,23,1,14,23,1,15,19,419,14,25,1,12,25,1,13,15,1,16,6,15,1,17,7,16,1,17,8,16,1,15,9,5], key:4}}, clause_keys:[0, 1, 2, 3, 4], next_key:5, key:591}, 592: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,33,10,16,0,6,0,16,0,2,1,16,0,4,2,16,0,1,3,16,0,7,4,16,0,8,5,16,0,5,6,16,0,9,7,8,0,6,0,8,0,7,1,8,0,3,2,8,0,0,3,3,589,11,32,10,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,9,8,8,9,9,9,2,4,591], key:0}, 1:{code:[29,2,1,60,0,33,0,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,15,1,15,4,20,5,23,1,16,25,1,15,19,578,16,25,1,11,26,947,23,1,17,19,396,17,23,1,18,20,6,23,1,19,23,1,20,19,416,19,23,1,21,25,1,13,19,396,21,24,1,18,16,1,20,7,8,1,11,0,8,1,14,1,12,396,2,24,1,18,3,579,1,32,0,2,5], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,15,1,8,0,15,1,9,1,15,1,10,2,15,1,11,3,16,0,0,4,20,5,23,1,12,25,0,1,19,578,12,25,1,8,26,947,25,0,2,20,6,23,1,13,23,1,14,19,416,13,25,0,2,25,1,10,16,1,14,7,31,9,0,0,9,1,1,9,2,2,2,4,588], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:592}, 593: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,5,0,20,1,23,1,6,23,1,7,19,578,6,23,1,8,26,947,23,1,9,20,2,23,1,10,24,1,7,19,578,10,24,1,8,26,947,24,1,9,8,1,5,0,8,1,8,1,3,10,1,32,0,2,5], key:0}, 1:{code:[29,2,1,60,0,33,0,15,1,5,0,20,1,23,1,6,23,1,7,19,578,6,23,1,8,23,1,9,23,1,10,20,2,23,1,11,24,1,7,19,578,11,24,1,8,26,961,24,1,10,8,1,5,0,8,1,8,1,3,10,1,32,0,2,5], key:1}, 2:{code:[30,0,15,1,6,0,20,1,23,1,7,23,1,8,20,2,24,1,7,23,1,9,8,1,6,0,8,1,8,1,8,1,9,2,4,593], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:593}, 594: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,33,2,15,1,5,0,16,0,1,1,20,2,23,1,6,23,1,7,19,574,6,23,1,8,23,1,9,23,1,10,19,575,10,25,0,0,8,1,5,0,8,1,8,1,3,10,3,32,2,9,0,0,9,1,1,2,4,35], key:0}, 1:{code:[30,0,15,1,6,0,15,1,7,1,20,2,23,1,8,23,1,9,8,1,6,0,8,1,7,1,8,1,9,2,4,594], key:1}}, clause_keys:[0, 1], next_key:2, key:594}, 595: {is_public:false, clauses:{0:{code:[28,1,21,0,0,15,1,4,1,15,1,5,2,16,1,4,3,31,5], key:0}, 1:{code:[29,2,15,1,4,0,20,1,23,1,5,23,1,6,19,534,5,23,1,7,15,1,8,2,20,3,23,1,9,24,1,6,19,534,9,25,1,8,31,5], key:1}, 2:{code:[30,0,15,1,8,0,20,1,23,1,9,23,1,10,15,1,11,2,20,3,24,1,9,23,1,12,8,1,8,0,8,1,10,1,8,1,11,2,8,1,12,3,4,595], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:595}, 596: {is_public:false, clauses:{0:{code:[28,1,15,1,5,0,17,0,1,17,0,2,15,1,6,3,16,1,6,4,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,33,6,16,0,0,0,20,1,23,1,7,25,0,1,19,574,7,23,1,8,23,1,9,23,1,10,19,575,10,23,1,11,16,0,2,2,16,0,5,3,16,0,4,4,8,1,11,0,8,0,0,1,3,37,7,32,6,8,0,3,0,12,214,1,25,0,5,27,1,3,34,6,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,596], key:1}, 2:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,16,0,0,0,20,1,23,1,5,25,0,1,20,2,24,1,5,25,0,2,16,0,3,3,16,0,4,4,31,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,2,4,596], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:596}, 597: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,60,15,19,188,0,25,0,13,25,0,0,16,0,1,1,16,0,10,2,16,0,3,3,16,0,11,4,16,0,12,5,16,0,14,6,16,0,7,7,16,0,15,8,16,0,9,9,31,9,13,0,8,0,1,1,8,0,10,2,10,366,3,8,0,11,4,8,0,12,5,9,14,6,8,0,6,7,9,15,8,8,0,8,9,3,598,16,8,0,2,0,12,270,1,25,0,10,27,1,3,34,13,9,10,0,9,11,1,8,0,4,2,9,12,3,8,0,5,4,3,596,13,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,9,8,8,9,9,9,2,4,597], key:0}, 1:{code:[30,0,15,1,20,0,15,1,21,1,15,1,22,2,15,1,23,3,15,1,24,4,15,1,25,5,15,1,26,6,15,1,27,7,15,1,28,8,15,1,29,9,8,1,20,0,8,1,21,1,8,1,22,2,8,1,23,3,8,1,24,4,8,1,25,5,8,1,26,6,8,1,27,7,8,1,28,8,8,1,29,9,4,598], key:1}}, clause_keys:[0, 1], next_key:2, key:597}, 598: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,19,545,0,25,0,3,25,0,6,16,0,8,1,16,0,9,2,16,0,0,3,16,0,10,4,16,0,2,5,16,0,14,6,16,0,12,7,16,0,13,8,16,0,5,9,31,8,0,6,0,8,0,1,1,3,453,15,8,0,7,0,8,0,1,1,3,453,15,8,0,7,0,10,341,1,3,519,15,8,0,9,0,9,14,1,8,0,1,2,8,0,11,3,3,595,15,9,6,0,9,7,1,9,8,2,9,9,3,14,0,4,9,10,5,9,11,6,9,12,7,9,13,8,8,0,4,9,3,599,14,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,600], key:0}, 1:{code:[29,2,19,555,0,23,1,13,23,1,14,15,1,15,1,15,1,16,2,15,1,17,3,15,1,18,4,15,1,19,5,15,1,20,6,20,7,23,1,21,25,1,20,19,578,21,24,1,14,26,945,23,1,22,19,396,22,23,1,23,20,8,23,1,24,23,1,25,19,445,24,24,1,13,23,1,26,19,396,26,24,1,23,16,1,25,9,8,1,14,0,8,1,18,1,12,396,2,24,1,23,4,579], key:1}, 2:{code:[29,3,19,551,0,26,403,15,1,10,1,21,0,2,17,366,3,15,1,11,4,15,1,12,5,15,1,13,6,16,1,13,7,20,8,26,742,23,1,14,16,1,14,9,31,5], key:2}, 3:{code:[29,4,19,551,0,26,403,15,1,10,1,21,0,2,17,980,3,15,1,11,4,15,1,12,5,15,1,13,6,16,1,13,7,20,8,26,742,23,1,14,20,14,26,714,23,1,15,16,1,15,9,31,5], key:3}, 4:{code:[29,5,1,60,0,60,1,60,2,19,548,0,25,0,0,15,1,10,1,15,1,11,2,17,366,3,16,0,1,4,15,1,12,5,15,1,13,6,16,1,13,7,20,8,23,1,14,23,1,15,19,443,14,23,1,16,19,396,16,25,0,2,16,1,15,9,31,9,0,0,9,1,1,12,396,2,25,0,2,2,4,601], key:4}, 5:{code:[29,6,1,60,0,60,1,60,2,19,548,0,25,0,0,15,1,10,1,15,1,11,2,17,980,3,16,0,1,4,15,1,12,5,15,1,13,6,16,1,13,7,20,8,23,1,14,23,1,15,19,443,14,23,1,16,19,396,16,25,0,2,20,15,26,712,23,1,17,20,17,26,714,23,1,18,16,1,18,9,31,9,0,0,9,1,1,12,396,2,25,0,2,2,4,601], key:5}, 6:{code:[29,7,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,60,15,60,16,19,551,0,23,1,10,19,602,10,25,0,8,25,0,16,15,1,11,1,16,0,9,2,16,0,0,3,16,0,10,4,16,0,1,5,16,0,15,6,16,0,12,7,16,0,13,8,16,0,5,9,31,9,16,0,13,1,25,0,14,25,0,6,3,13,17,8,0,6,0,8,0,3,1,3,453,16,8,0,9,0,9,15,1,8,0,3,2,8,0,11,3,3,595,16,9,14,0,8,0,3,1,8,0,8,2,8,0,2,3,3,500,15,8,0,2,0,8,0,3,1,8,0,7,2,3,520,14,9,6,0,9,7,1,9,8,2,9,9,3,14,0,4,9,10,5,9,11,6,9,12,7,9,13,8,8,0,4,9,3,599,14,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,603], key:6}, 7:{code:[29,8,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,60,15,60,16,19,551,0,25,0,16,16,0,8,1,16,0,9,2,16,0,0,3,16,0,10,4,16,0,1,5,16,0,15,6,16,0,12,7,16,0,13,8,16,0,5,9,31,9,16,0,13,1,25,0,14,25,0,6,3,13,17,8,0,6,0,8,0,3,1,3,453,16,8,0,9,0,9,15,1,8,0,3,2,8,0,11,3,3,595,16,9,14,0,8,0,3,1,8,0,8,2,8,0,2,3,3,500,15,8,0,2,0,8,0,3,1,8,0,7,2,3,520,14,9,6,0,9,7,1,9,8,2,9,9,3,14,0,4,9,10,5,9,11,6,9,12,7,9,13,8,8,0,4,9,3,599,14,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,2,4,603], key:7}, 8:{code:[29,9,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,19,188,0,25,0,0,25,0,1,16,0,2,1,16,0,3,2,16,0,4,3,16,0,5,4,16,0,6,5,16,0,7,6,16,0,8,7,16,0,9,8,16,0,10,9,31,12,188,0,25,0,0,25,0,1,9,2,1,9,3,2,9,4,3,9,5,4,9,6,5,9,7,6,9,8,7,9,9,8,9,10,9,2,4,597], key:8}, 9:{code:[30,0,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,15,1,15,4,15,1,16,5,15,1,17,6,15,1,18,7,15,1,19,8,15,1,20,9,12,604,0,26,982,25,1,11,4,125], key:9}}, clause_keys:[0, 1, 2, 3, 4, 5, 6, 7, 8, 9], next_key:10, key:598}, 599: {is_public:false, clauses:{0:{code:[28,1,17,0,0,17,0,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,15,1,14,6,16,1,14,7,15,1,15,8,16,1,15,9,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,20,0,23,1,21,25,0,0,20,1,23,1,22,25,0,1,16,0,2,2,16,0,3,3,16,0,10,4,16,0,5,5,15,1,23,6,16,0,7,7,16,0,13,8,16,0,9,9,8,1,21,0,8,1,22,1,8,0,2,2,8,0,3,3,12,398,4,25,0,10,8,0,5,5,8,1,23,6,8,0,6,7,8,0,12,8,8,0,8,9,8,0,11,10,3,606,14,12,607,0,25,0,11,3,196,14,9,11,0,9,12,1,9,13,2,3,608,14,8,0,4,0,12,270,1,25,0,10,27,1,3,34,11,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,9,8,8,9,9,9,2,4,599], key:1}}, clause_keys:[0, 1], next_key:2, key:599}, 600: {is_public:false, clauses:{0:{code:[28,1,17,980,0,15,1,9,1,15,1,10,2,15,1,11,3,15,1,12,4,15,1,13,5,8,1,10,0,8,1,12,1,12,406,14,25,1,11,25,1,9,13,2,24,1,14,25,1,13,4,580], key:0}, 1:{code:[30,0,17,366,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,15,1,12,5,8,1,11,0,12,401,13,25,1,10,25,1,8,25,1,9,13,1,24,1,13,25,1,12,4,9], key:1}}, clause_keys:[0, 1], next_key:2, key:600}, 601: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,6,0,15,1,7,1,15,1,8,2,8,1,6,0,8,1,7,1,8,1,8,2,3,579,1,32,0,2,5], key:0}, 1:{code:[30,0,15,1,4,0,15,1,5,1,15,1,6,2,12,605,0,25,1,4,25,1,5,4,125], key:1}}, clause_keys:[0, 1], next_key:2, key:601}, 603: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,17,980,0,16,0,0,1,16,0,2,2,16,0,3,3,16,0,1,4,16,0,4,5,31,9,0,0,9,1,1,12,178,4,25,0,2,25,0,3,12,415,3,24,1,4,13,2,24,1,3,25,0,4,2,4,580], key:0}, 1:{code:[30,0,17,366,0,15,1,6,1,15,1,7,2,15,1,8,3,20,4,23,1,9,23,1,10,19,414,9,23,1,11,25,1,6,19,178,11,25,1,7,25,1,8,16,1,10,5,5], key:1}}, clause_keys:[0, 1], next_key:2, key:603}, 606: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,60,3,60,4,33,4,16,0,0,0,16,0,1,1,16,0,2,2,15,1,12,3,15,1,13,4,15,1,14,5,15,1,15,6,16,1,15,7,20,8,23,1,16,23,1,17,19,421,16,25,0,3,25,1,13,16,1,17,9,15,1,18,10,8,0,0,0,3,581,5,32,4,9,0,0,9,1,1,9,2,2,9,3,3,7,11,4,8,0,4,5,7,12,6,7,13,7,7,14,8,7,15,9,7,16,10,2,41,119,11,28,2147483859,1,60,0,60,1,60,2,60,3,60,4,33,3,16,0,0,0,16,0,4,1,16,0,1,2,16,0,2,3,15,1,13,4,15,1,14,5,15,1,15,6,15,1,16,7,15,1,17,8,15,1,18,9,15,1,19,10,8,0,0,0,3,4,5,9,4,0,10,991,1,3,9,5,32,3,9,0,0,14,0,1,9,1,2,9,2,3,2,4,500,30,0,15,1,13,0,15,1,14,1,15,1,15,2,15,1,16,3,15,1,17,4,15,1,18,5,15,1,19,6,15,1,20,7,15,1,21,8,15,1,22,9,15,1,23,10,8,1,18,0,8,1,14,1,8,1,13,2,8,1,19,3,8,1,20,4,8,1,21,5,8,1,22,6,8,1,15,7,8,1,23,8,8,1,16,9,41,300,10,28,2147484093,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,33,9,15,1,12,0,16,0,7,1,16,0,8,2,16,0,3,3,16,0,2,4,16,0,6,5,16,0,4,6,16,0,5,7,16,0,1,8,16,0,0,9,8,0,7,0,3,5,10,32,9,9,8,0,13,1,25,0,3,25,0,2,3,13,9,8,0,2,0,8,0,6,1,3,269,8,8,0,4,0,12,270,1,25,0,6,25,0,7,3,34,8,9,3,0,9,4,1,9,5,2,8,0,1,3,3,500,6,9,0,0,13,1,25,0,1,25,0,2,2,4,13,30,0,15,1,11,0,15,1,12,1,15,1,13,2,15,1,14,3,15,1,15,4,15,1,16,5,15,1,17,6,15,1,18,7,15,1,19,8,15,1,20,9,8,1,13,0,8,1,20,1,4,9], key:0}, 1:{code:[29,2,1,60,0,33,0,15,1,12,0,15,1,13,1,15,1,14,2,15,1,15,3,15,1,16,4,15,1,17,5,15,1,18,6,16,1,18,7,20,8,23,1,19,23,1,20,19,425,19,25,1,12,25,1,16,16,1,20,9,15,1,21,10,8,1,12,0,3,5,1,32,0,2,5], key:1}, 2:{code:[29,3,1,60,0,33,0,15,1,12,0,15,1,13,1,15,1,14,2,15,1,15,3,15,1,16,4,15,1,17,5,15,1,18,6,16,1,18,7,20,8,23,1,19,23,1,20,19,426,19,25,1,12,25,1,16,16,1,20,9,15,1,21,10,8,1,12,0,3,6,1,32,0,2,5], key:2}, 3:{code:[29,4,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,33,8,16,0,0,0,15,1,12,1,15,1,13,2,16,0,1,3,16,0,2,4,16,0,3,5,16,0,4,6,16,0,5,7,16,0,6,8,16,0,7,9,15,1,14,10,8,0,0,0,3,3,9,32,8,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,2,4,592], key:3}, 4:{code:[29,5,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,20,0,25,0,0,25,0,1,15,1,11,1,16,0,2,2,16,0,3,3,16,0,8,4,16,0,6,5,16,0,4,6,16,0,5,7,16,0,7,8,16,0,9,9,16,0,10,10,31,13,11,25,0,1,26,0,13,0,25,0,0,24,1,11,9,2,1,9,3,2,9,4,3,9,5,4,9,6,5,9,7,6,12,424,12,25,0,8,13,7,24,1,12,23,1,13,8,1,13,8,9,9,9,9,10,10,2,4,611], key:4}, 5:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,15,1,13,0,16,0,12,1,16,0,1,2,16,0,2,3,16,0,9,4,16,0,5,5,16,0,3,6,16,0,4,7,16,0,6,8,16,0,10,9,16,0,11,10,8,1,13,0,13,1,25,0,13,25,0,0,3,13,14,8,0,0,0,8,0,8,1,3,453,14,7,6,0,9,12,1,9,13,2,8,0,8,3,8,0,1,4,8,0,7,5,40,186,6,14,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,12,178,12,25,0,7,25,0,8,12,423,11,24,1,12,25,0,9,13,7,24,1,11,23,1,13,8,1,13,8,9,10,9,9,11,10,2,4,611,28,2147483898,1,60,0,60,1,60,2,60,3,60,4,33,4,15,1,9,0,15,1,10,1,16,0,0,2,16,0,1,3,16,0,2,4,16,0,3,5,8,1,10,0,40,286,1,5,32,4,9,0,0,9,1,1,9,2,2,9,3,3,2,4,500,30,0,15,1,7,0,15,1,8,1,15,1,9,2,15,1,10,3,15,1,11,4,15,1,12,5,8,1,9,0,8,1,12,1,4,9,28,2147483952,15,1,2,0,8,1,2,0,12,612,1,25,1,2,4,9,30,0,15,1,2,0,8,1,2,0,10,991,1,4,9], key:5}}, clause_keys:[0, 1, 2, 3, 4, 5], next_key:6, key:606}, 608: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,33,2,15,1,4,0,16,0,0,1,16,0,1,2,8,1,4,0,3,3,3,32,2,9,0,0,9,1,1,2,4,9], key:0}, 1:{code:[30,0,15,1,5,0,15,1,6,1,15,1,7,2,8,1,6,0,8,1,7,1,4,609], key:1}}, clause_keys:[0, 1], next_key:2, key:608}, 609: {is_public:false, clauses:{0:{code:[254,0,15,1,6,0,15,1,7,1,8,1,6,0,8,1,7,1,10,0,2,10,0,3,4,613], key:0}}, clause_keys:[0], next_key:1, key:609}, 610: {is_public:false, clauses:{0:{code:[254,0,15,1,21,0,15,1,22,1,15,1,23,2,15,1,24,3,19,398,4,23,1,25,15,1,26,5,15,1,27,6,15,1,28,7,15,1,29,8,15,1,30,9,8,1,21,0,8,1,22,1,8,1,23,2,8,1,24,3,12,398,4,24,1,25,8,1,26,5,8,1,27,6,8,1,28,7,8,1,29,8,8,1,30,9,7,31,10,4,606], key:0}}, clause_keys:[0], next_key:1, key:610}, 611: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,11,1,15,1,12,2,15,1,13,3,16,1,13,4,15,1,14,5,15,1,15,6,16,1,15,7,15,1,16,8,16,1,16,9,15,1,17,10,31,5], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,33,11,20,0,23,1,12,25,0,0,16,0,1,1,16,0,2,2,16,0,3,3,16,0,4,4,16,0,5,5,16,0,6,6,16,0,7,7,20,8,23,1,13,25,0,8,19,439,13,24,1,12,16,0,9,9,16,0,10,10,8,1,12,0,3,581,12,32,11,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,9,8,8,9,9,9,9,10,10,2,4,611], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,33,11,20,0,23,1,12,25,0,0,16,0,1,1,16,0,2,2,16,0,3,3,16,0,4,4,16,0,5,5,16,0,6,6,16,0,7,7,20,8,23,1,13,25,0,8,19,440,13,24,1,12,16,0,9,9,16,0,10,10,8,1,12,0,3,5,12,32,11,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,9,8,8,9,9,9,9,10,10,2,4,611], key:2}, 3:{code:[29,4,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,33,13,20,0,25,0,10,25,0,0,16,0,1,1,16,0,2,2,16,0,11,3,16,0,4,4,16,0,5,5,16,0,6,6,16,0,7,7,16,0,12,8,16,0,9,9,17,1004,10,8,0,10,0,3,3,14,32,13,9,10,0,8,0,5,1,9,11,2,8,0,3,3,9,12,4,8,0,8,5,3,586,13,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,9,8,8,9,9,9,7,11,10,2,4,611], key:3}, 4:{code:[30,0,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,60,8,60,9,60,10,60,11,60,12,60,13,60,14,20,0,25,0,11,25,0,0,16,0,1,1,16,0,2,2,15,1,14,3,16,0,4,4,16,0,5,5,16,0,14,6,16,0,7,7,20,8,23,1,15,25,0,8,19,437,15,25,0,12,16,0,9,9,16,0,10,10,8,1,14,0,8,0,13,1,8,0,12,2,3,588,15,9,11,0,10,341,1,8,0,1,2,8,0,2,3,9,12,4,8,0,5,5,9,13,6,8,0,3,7,9,14,8,8,0,6,9,8,0,10,10,3,606,15,9,0,0,9,1,1,9,2,2,9,3,3,9,4,4,9,5,5,9,6,6,9,7,7,9,8,8,9,9,9,9,10,10,2,4,611], key:4}}, clause_keys:[0, 1, 2, 3, 4], next_key:5, key:611}, 613: {is_public:false, clauses:{0:{code:[28,1,1,60,0,60,1,60,2,33,2,15,1,5,0,16,1,5,1,16,0,0,2,16,0,1,3,8,1,5,0,3,3,3,32,2,12,614,0,25,0,0,25,0,1,3,196,2,9,0,0,9,1,1,2,4,615], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,33,7,20,0,23,1,6,25,0,0,19,437,6,23,1,7,19,398,7,25,0,4,20,1,25,0,3,25,0,1,16,0,5,2,16,0,6,3,8,0,2,0,12,398,8,25,0,4,12,437,1,24,1,8,3,9,8,7,4,0,7,5,1,8,0,4,2,8,0,6,3,40,152,4,8,8,0,2,0,7,4,1,7,5,2,8,0,5,3,40,225,4,8,32,7,9,0,0,9,1,1,12,178,5,25,0,3,25,0,4,12,214,4,25,0,2,24,1,5,13,2,24,1,4,25,0,5,9,6,3,2,4,613,28,2147483853,1,60,0,33,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,12,178,10,25,1,7,25,1,8,12,214,0,25,1,6,24,1,10,8,1,9,1,3,116,1,32,0,2,4,61,30,0,15,1,5,0,15,1,6,1,15,1,7,2,15,1,8,3,4,62,28,2147483926,1,60,0,33,0,15,1,6,0,15,1,7,1,15,1,8,2,15,1,9,3,12,178,10,25,1,7,25,1,8,12,214,0,25,1,6,24,1,10,8,1,9,1,3,116,1,32,0,2,4,61,30,0,15,1,5,0,15,1,6,1,15,1,7,2,15,1,8,3,4,62], key:1}, 2:{code:[29,3,1,60,0,60,1,60,2,60,3,60,4,60,5,60,6,60,7,33,7,20,0,23,1,6,25,0,0,19,436,6,23,1,7,19,398,7,25,0,5,20,1,25,0,4,25,0,1,16,0,2,2,16,0,6,3,8,0,3,0,12,398,8,25,0,5,12,436,1,24,1,8,3,9,8,32,7,9,0,0,9,1,1,9,2,2,12,178,5,25,0,4,25,0,5,12,214,4,25,0,3,24,1,5,13,3,24,1,4,25,0,6,2,4,613], key:2}, 3:{code:[30,0,20,0,23,1,8,23,1,9,20,1,24,1,8,23,1,10,15,1,11,2,15,1,12,3,8,1,9,0,8,1,10,1,8,1,11,2,8,1,12,3,4,613], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:613}, 615: {is_public:false, clauses:{0:{code:[28,1,1,60,0,16,0,0,0,17,0,1,31,9,0,0,2,4,616], key:0}, 1:{code:[30,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,4,617], key:1}}, clause_keys:[0, 1], next_key:2, key:615}, 616: {is_public:false, clauses:{0:{code:[28,1,17,0,0,5], key:0}, 1:{code:[30,0,20,0,23,1,2,23,1,3,19,214,2,23,1,4,23,1,5,19,178,5,24,1,4,23,1,6,8,1,3,0,4,616], key:1}}, clause_keys:[0, 1], next_key:2, key:616}, 617: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,8,1,3,0,4,618], key:0}, 1:{code:[29,2,1,60,0,60,1,60,2,33,2,20,0,23,1,4,25,0,0,19,214,4,23,1,5,23,1,6,19,178,6,23,1,7,23,1,8,16,0,1,1,12,178,9,24,1,5,24,1,8,12,214,0,24,1,7,24,1,9,8,0,1,1,3,116,3,32,2,9,0,0,9,1,1,2,4,617], key:1}, 2:{code:[30,0,20,0,23,1,4,23,1,5,19,214,4,23,1,6,23,1,7,19,178,7,24,1,6,23,1,8,15,1,9,1,8,1,5,0,8,1,9,1,4,617], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:617}, 618: {is_public:false, clauses:{0:{code:[28,1,17,0,0,5], key:0}, 1:{code:[29,2,1,60,0,20,0,23,1,1,25,0,0,19,214,1,23,1,2,23,1,3,19,178,3,24,1,2,23,1,4,31,9,0,0,2,4,618], key:1}, 2:{code:[30,0,20,0,23,1,2,23,1,3,8,1,3,0,4,618], key:2}}, clause_keys:[0, 1, 2], next_key:3, key:618}, 619: {is_public:false, clauses:{0:{code:[28,1,17,0,0,31,5], key:0}, 1:{code:[30,0,1,60,0,60,1,33,1,20,0,23,1,2,25,0,0,8,1,2,0,3,513,2,32,1,9,0,0,2,4,619], key:1}}, clause_keys:[0, 1], next_key:2, key:619}, 620: {is_public:false, clauses:{0:{code:[254,0,1,60,0,16,0,0,0,8,0,0,0,3,623,1,8,0,0,0,12,461,2,27,0,12,462,1,27,0,24,1,2,3,625,1,8,0,0,0,3,624,1,7,2,0,9,0,1,2,41,53,2,28,2147483742,1,60,0,60,1,60,2,33,2,16,0,0,0,16,0,1,1,8,0,0,0,8,0,1,1,3,505,3,32,2,9,0,0,9,1,1,2,4,507,30,0,15,1,3,0,15,1,4,1,4,62], key:0}}, clause_keys:[0], next_key:1, key:620}, 623: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,0,0,8,0,1,0,40,36,1,2,13,1,25,0,0,25,0,1,12,622,0,24,1,1,2,4,230,28,2147483709,1,60,0,33,0,15,1,3,0,12,622,0,25,1,3,3,232,1,32,0,2,4,62,30,0,15,1,2,0,8,1,2,0,10,0,1,4,9], key:0}}, clause_keys:[0], next_key:1, key:623}, 624: {is_public:false, clauses:{0:{code:[254,0,1,60,0,15,1,2,0,13,3,25,1,2,25,0,0,12,622,0,24,1,3,3,232,1,12,622,0,25,0,0,2,4,230], key:0}}, clause_keys:[0], next_key:1, key:624}, 625: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,0,0,16,0,2,1,8,0,0,0,8,0,1,1,10,0,2,3,70,3,9,0,0,9,1,1,9,2,2,2,4,626], key:0}}, clause_keys:[0], next_key:1, key:625}, 626: {is_public:false, clauses:{0:{code:[28,1,1,60,0,15,1,3,0,17,1024,1,16,0,0,2,31,9,0,0,7,3,1,7,4,2,2,41,30,3,28,2147483730,1,60,0,60,1,60,2,33,2,15,1,5,0,16,0,0,1,16,0,1,2,8,1,5,0,12,461,6,27,0,12,462,1,27,0,24,1,6,3,9,3,32,2,9,1,0,2,41,101,1,30,0,15,1,4,0,15,1,5,1,15,1,6,2,10,1025,0,4,125,28,2147483776,1,60,0,16,0,0,0,12,526,0,25,0,0,3,232,1,9,0,0,3,627,1,2,4,61,30,0,15,1,2,0,4,62], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,33,2,16,0,0,0,15,1,6,1,15,1,7,2,8,1,6,0,8,1,7,1,8,0,1,2,3,463,3,32,2,9,0,0,9,1,1,2,4,625], key:1}}, clause_keys:[0, 1], next_key:2, key:626}, 627: {is_public:false, clauses:{0:{code:[254,0,1,60,0,33,0,15,1,2,0,8,1,2,0,3,183,1,32,0,2,5], key:0}}, clause_keys:[0], next_key:1, key:627}, 628: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,33,0,16,0,2,0,15,1,5,1,8,1,5,0,10,357,1,8,0,1,2,3,235,3,8,0,1,0,10,1028,1,10,0,2,3,79,3,8,0,1,0,10,1029,1,13,2,25,0,2,26,0,3,79,3,8,0,1,0,10,1030,1,10,0,2,3,79,2,8,0,1,0,3,98,2,8,0,1,0,10,1031,1,10,0,2,3,79,2,9,1,0,3,236,2,32,0,2,5], key:0}}, clause_keys:[0], next_key:1, key:628}, 629: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,16,0,1,0,8,0,0,0,3,97,2,9,0,0,9,1,1,2,4,628], key:0}}, clause_keys:[0], next_key:1, key:629}, 630: {is_public:false, clauses:{0:{code:[28,1,17,0,0,5], key:0}, 1:{code:[30,0,1,60,0,20,0,23,1,2,25,0,0,8,1,2,0,3,183,1,9,0,0,2,4,630], key:1}}, clause_keys:[0, 1], next_key:2, key:630}, 631: {is_public:false, clauses:{0:{code:[28,1,17,0,0,5], key:0}, 1:{code:[30,0,1,60,0,20,0,23,1,2,25,0,0,8,1,2,0,3,632,1,9,0,0,2,4,631], key:1}}, clause_keys:[0, 1], next_key:2, key:631}, 632: {is_public:false, clauses:{0:{code:[254,0,1,60,0,15,1,3,0,8,1,3,0,8,0,0,1,3,74,1,9,0,0,3,633,1,10,1039,0,2,4,64], key:0}}, clause_keys:[0], next_key:1, key:632}, 633: {is_public:false, clauses:{0:{code:[254,0,1,60,0,16,0,0,0,8,0,0,0,3,634,1,9,0,0,2,4,78], key:0}}, clause_keys:[0], next_key:1, key:633}, 634: {is_public:false, clauses:{0:{code:[254,0,1,60,0,15,1,4,0,8,1,4,0,10,356,1,8,0,0,2,3,77,1,8,0,0,0,3,620,1,9,0,0,2,4,236], key:0}}, clause_keys:[0], next_key:1, key:634}, 635: {is_public:false, clauses:{0:{code:[28,1,17,0,0,17,0,1,5], key:0}, 1:{code:[30,0,1,60,0,60,1,20,0,23,1,4,25,0,0,20,1,23,1,5,25,0,1,8,1,4,0,8,1,5,1,3,191,2,9,0,0,9,1,1,2,4,635], key:1}}, clause_keys:[0, 1], next_key:2, key:635}, 636: {is_public:false, clauses:{0:{code:[28,1,17,0,0,17,0,1,5], key:0}, 1:{code:[30,0,1,60,0,60,1,20,0,23,1,4,25,0,0,20,1,23,1,5,25,0,1,19,214,5,24,1,4,23,1,6,8,1,4,0,8,1,6,1,3,103,2,9,0,0,9,1,1,2,4,636], key:1}}, clause_keys:[0, 1], next_key:2, key:636}, 637: {is_public:false, clauses:{0:{code:[28,1,17,0,0,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,20,0,23,1,3,25,0,0,19,214,3,25,0,1,23,1,4,8,1,4,0,8,0,2,1,3,359,3,8,0,1,0,3,332,3,9,2,0,3,633,3,8,0,1,0,3,333,2,12,621,0,25,0,1,3,264,2,12,621,0,25,0,1,3,175,2,9,0,0,2,4,637], key:1}}, clause_keys:[0, 1], next_key:2, key:637}, 638: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,17,0,0,10,1048,0,3,64,1,32,0,2,5], key:0}, 1:{code:[30,0,15,1,2,0,8,1,2,0,4,639], key:1}}, clause_keys:[0, 1], next_key:2, key:638}, 639: {is_public:false, clauses:{0:{code:[28,1,17,0,0,31,5], key:0}, 1:{code:[30,0,1,60,0,20,0,23,1,2,25,0,0,19,559,2,23,1,3,23,1,4,12,559,0,24,1,3,24,1,4,3,64,1,9,0,0,2,4,639], key:1}}, clause_keys:[0, 1], next_key:2, key:639}, 640: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,60,3,60,4,15,1,4,0,8,1,4,0,8,0,4,1,8,0,3,2,3,66,5,8,0,2,0,3,135,5,9,4,0,3,183,5,3,281,4,8,0,1,0,3,135,4,3,90,4,9,3,0,3,638,4,8,0,0,0,12,214,3,25,0,1,25,0,2,12,642,2,27,1000,24,1,3,12,643,1,24,1,2,27,1000,3,34,3,13,3,26,1060,26,0,13,2,25,0,0,24,1,3,13,0,26,1061,24,1,2,10,596,1,3,644,1,10,596,0,2,4,64], key:0}}, clause_keys:[0], next_key:1, key:640}, 641: {is_public:false, clauses:{0:{code:[254,0,1,15,1,2,0,8,1,2,0,3,640,0,2,4,62], key:0}}, clause_keys:[0], next_key:1, key:641}, 644: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,60,2,16,0,0,0,16,0,1,1,8,0,2,0,3,43,3,9,0,0,9,1,1,9,2,2,2,4,645], key:0}}, clause_keys:[0], next_key:1, key:644}, 645: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,15,1,4,2,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,20,0,23,1,5,25,0,0,16,0,1,1,16,0,2,2,8,0,2,0,8,1,5,1,3,258,3,9,0,0,9,1,1,9,2,2,2,4,646], key:1}}, clause_keys:[0, 1], next_key:2, key:645}, 646: {is_public:false, clauses:{0:{code:[28,1,17,0,0,15,1,3,1,15,1,4,2,5], key:0}, 1:{code:[30,0,1,60,0,60,1,60,2,60,3,20,0,25,0,3,25,0,0,16,0,1,1,16,0,2,2,8,0,2,0,8,0,1,1,3,258,4,8,0,2,0,9,3,1,3,258,4,9,0,0,9,1,1,9,2,2,2,4,646], key:1}}, clause_keys:[0, 1], next_key:2, key:646}, 647: {is_public:false, clauses:{0:{code:[254,0,1,60,0,15,1,5,0,15,1,6,1,8,1,5,0,8,0,0,1,8,1,6,2,3,66,1,9,0,0,2,4,183], key:0}}, clause_keys:[0], next_key:1, key:647}, 648: {is_public:true, clauses:{}, clause_keys:[], next_key:0, key:648}, 649: {is_public:false, clauses:{0:{code:[254,0,17,813,0,5], key:0}}, clause_keys:[0], next_key:1, key:649}, 650: {is_public:false, clauses:{0:{code:[254,0,17,814,0,5], key:0}}, clause_keys:[0], next_key:1, key:650}, 651: {is_public:false, clauses:{0:{code:[254,0,17,815,0,5], key:0}}, clause_keys:[0], next_key:1, key:651}, 652: {is_public:false, clauses:{0:{code:[28,1,17,815,0,5], key:0}, 1:{code:[30,0,19,214,0,26,815,26,814,5], key:1}}, clause_keys:[0, 1], next_key:2, key:652}, 657: {is_public:false, clauses:{0:{code:[254,0,19,167,0,23,1,2,23,1,3,19,653,2,23,1,4,23,1,5,19,167,1,23,1,6,23,1,7,19,654,6,24,1,4,19,565,7,23,1,8,26,127,19,566,8,23,1,9,26,127,19,563,9,23,1,10,23,1,11,26,127,19,550,10,23,1,12,24,1,3,23,1,13,23,1,14,19,655,12,26,1073,23,1,15,20,15,24,1,4,26,0,19,656,14,24,1,13,24,1,5,5], key:0}}, clause_keys:[0], next_key:1, key:657}, 664: {is_public:false, clauses:{0:{code:[254,0,15,1,2,0,8,1,2,0,4,648], key:0}}, clause_keys:[0], next_key:1, key:664}, 665: {is_public:false, clauses:{0:{code:[254,0,1,3,658,0,10,1077,0,3,166,0,3,659,0,7,2,0,13,4,23,1,5,26,0,13,1,23,1,3,24,1,4,40,73,2,0,2,4,660,254,0,1,60,0,16,0,0,0,15,1,3,1,12,654,0,25,0,0,8,1,3,1,3,661,1,9,0,0,2,41,139,1,28,2147483764,1,60,0,33,0,15,1,5,0,20,1,23,1,6,23,1,7,20,7,23,1,8,26,0,8,1,6,0,8,1,8,1,40,40,2,1,32,0,2,4,662,30,0,15,1,3,0,20,1,23,1,4,23,1,5,20,5,23,1,6,26,0,4,663,28,2147483810,1,60,0,33,0,15,1,3,0,8,1,3,0,3,664,1,32,0,2,4,662,30,0,15,1,2,0,4,663], key:0}}, clause_keys:[0], next_key:1, key:665}, 667: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,2,0,16,1,2,1,3,663,1,32,0,10,1085,0,10,0,1,2,4,666], key:0}, 1:{code:[30,0,1,15,1,4,0,15,1,5,1,10,1088,0,13,6,25,1,4,26,0,13,1,25,1,5,24,1,6,3,666,0,10,1089,0,2,4,668], key:1}}, clause_keys:[0, 1], next_key:2, key:667}, 669: {is_public:false, clauses:{0:{code:[28,1,5], key:0}, 1:{code:[30,0,5], key:1}}, clause_keys:[0, 1], next_key:2, key:669}, 672: {is_public:false, clauses:{0:{code:[28,1,17,1092,0,17,402,1,7,5,0,12,670,1,24,1,5,13,2,26,813,26,0,4,671], key:0}, 1:{code:[29,2,17,1094,0,17,402,1,7,5,0,12,673,1,24,1,5,13,2,26,814,26,0,4,671], key:1}, 2:{code:[29,3,17,1095,0,17,402,1,7,5,0,12,674,1,24,1,5,13,2,26,815,26,0,4,671], key:2}, 3:{code:[29,4,17,1096,0,17,402,1,7,5,0,12,675,1,24,1,5,12,214,7,26,815,26,814,13,6,24,1,7,26,0,13,2,26,815,24,1,6,4,671], key:3}, 4:{code:[29,5,1,60,0,33,0,17,743,0,17,402,1,3,669,1,32,0,2,5], key:4}, 5:{code:[29,6,1,60,0,17,344,0,17,402,1,7,5,0,12,677,1,24,1,5,8,0,0,2,3,671,1,9,0,0,13,4,26,1099,26,0,13,3,26,1098,24,1,4,13,2,26,571,24,1,3,13,1,26,566,24,1,2,2,4,678], key:5}, 6:{code:[29,7,17,910,0,17,402,1,41,11,0,254,0,1,60,0,33,0,3,669,1,32,0,2,5], key:6}, 7:{code:[29,8,1,60,0,60,1,33,0,17,39,0,17,402,1,10,1102,0,7,5,1,14,0,2,3,679,2,3,680,2,10,1102,0,8,0,1,1,12,270,2,25,0,1,27,1,3,679,2,8,0,1,0,3,166,2,9,1,0,14,5,1,3,681,2,32,0,2,5], key:7}, 8:{code:[29,9,19,346,0,27,1,17,402,1,41,13,0,28,2147483676,1,60,0,33,0,3,662,1,32,0,2,4,662,30,0,4,663], key:8}, 9:{code:[29,10,19,346,0,27,2,17,125,1,41,13,0,28,2147483676,1,60,0,33,0,3,663,1,32,0,2,4,662,30,0,4,663], key:9}, 10:{code:[29,11,1,60,0,19,346,0,27,3,17,402,1,8,0,0,0,40,27,1,1,9,0,0,2,4,682,28,2147483699,1,60,0,33,0,15,1,4,0,8,1,4,0,40,59,1,1,32,0,2,4,662,30,0,15,1,2,0,4,663,28,2147483733,1,60,0,33,0,15,1,3,0,8,1,3,0,10,566,1,3,683,1,32,0,2,4,662,30,0,15,1,2,0,4,663], key:10}, 11:{code:[29,12,19,684,0,27,1,17,125,1,14,0,0,12,685,1,26,566,26,1098,7,5,2,4,686], key:11}, 12:{code:[29,13,19,684,0,27,2,17,402,1,14,1,0,12,685,1,26,566,26,1098,10,566,2,4,686], key:12}, 13:{code:[29,14,19,684,0,27,3,17,402,1,14,2,0,12,687,1,26,566,26,1098,26,571,10,1098,2,4,686], key:13}, 14:{code:[29,15,19,684,0,27,4,17,125,1,14,4,0,12,687,1,26,566,26,1098,26,571,7,5,2,4,686], key:14}, 15:{code:[29,16,19,684,0,27,5,17,402,1,14,3,0,12,687,1,26,566,26,1098,26,571,10,571,2,4,686], key:15}, 16:{code:[29,17,19,688,0,27,1,17,402,1,10,566,0,10,566,1,4,689], key:16}, 17:{code:[29,18,19,688,0,27,2,17,402,1,12,690,0,23,1,4,23,1,5,12,690,1,23,1,6,24,1,6,4,689], key:17}, 18:{code:[29,19,19,688,0,27,3,17,125,1,12,690,0,23,1,4,24,1,4,12,690,1,23,1,5,23,1,6,4,689], key:18}, 19:{code:[29,20,19,688,0,27,4,17,125,1,12,691,0,23,1,4,12,692,5,24,1,4,12,691,1,24,1,5,4,689], key:19}, 20:{code:[29,21,19,688,0,27,5,17,125,1,7,4,0,12,692,1,24,1,4,4,689], key:20}, 21:{code:[29,22,1,60,0,60,1,19,688,0,27,6,17,402,1,8,0,1,0,8,0,0,1,3,689,2,9,0,0,12,692,1,25,0,1,2,4,689], key:21}, 22:{code:[29,23,1,60,0,19,693,0,27,1,17,402,1,12,214,6,23,1,7,23,1,8,12,214,5,24,1,6,23,1,9,12,214,0,24,1,5,23,1,10,12,694,1,26,1109,24,1,7,24,1,8,24,1,9,24,1,10,8,0,0,2,3,671,1,9,0,0,12,214,4,27,0,27,0,12,214,3,24,1,4,27,9,12,214,2,24,1,3,26,596,12,214,8,27,0,27,1,12,214,7,24,1,8,27,8,12,214,6,24,1,7,26,570,12,214,12,27,0,27,2,12,214,11,24,1,12,27,7,12,214,10,24,1,11,26,1110,12,214,16,27,0,27,3,12,214,15,24,1,16,27,6,12,214,14,24,1,15,26,1111,12,214,20,27,0,27,4,12,214,19,24,1,20,27,5,12,214,18,24,1,19,26,1112,12,214,24,27,0,27,5,12,214,23,24,1,24,27,4,12,214,22,24,1,23,26,1113,12,214,28,27,0,27,6,12,214,27,24,1,28,27,3,12,214,26,24,1,27,26,1114,12,214,32,27,0,27,7,12,214,31,24,1,32,27,2,12,214,30,24,1,31,26,1115,12,214,36,27,0,27,8,12,214,35,24,1,36,27,1,12,214,34,24,1,35,26,1116,12,214,40,27,0,27,9,12,214,39,24,1,40,27,0,12,214,38,24,1,39,26,1109,12,214,44,27,1,27,0,12,214,43,24,1,44,27,8,12,214,42,24,1,43,26,596,12,214,48,27,1,27,1,12,214,47,24,1,48,27,7,12,214,46,24,1,47,26,1117,12,214,52,27,1,27,2,12,214,51,24,1,52,27,6,12,214,50,24,1,51,26,1118,12,214,56,27,1,27,3,12,214,55,24,1,56,27,5,12,214,54,24,1,55,26,1119,12,214,60,27,1,27,4,12,214,59,24,1,60,27,4,12,214,58,24,1,59,26,1120,12,214,64,27,1,27,5,12,214,63,24,1,64,27,3,12,214,62,24,1,63,26,1121,12,214,68,27,1,27,6,12,214,67,24,1,68,27,2,12,214,66,24,1,67,26,1122,12,214,72,27,1,27,7,12,214,71,24,1,72,27,1,12,214,70,24,1,71,26,1123,12,214,76,27,1,27,8,12,214,75,24,1,76,27,0,12,214,74,24,1,75,26,1124,12,214,80,27,2,27,0,12,214,79,24,1,80,27,7,12,214,78,24,1,79,26,596,12,214,84,27,2,27,1,12,214,83,24,1,84,27,6,12,214,82,24,1,83,26,537,12,214,88,27,2,27,2,12,214,87,24,1,88,27,5,12,214,86,24,1,87,26,1125,12,214,92,27,2,27,3,12,214,91,24,1,92,27,4,12,214,90,24,1,91,26,1126,12,214,96,27,2,27,4,12,214,95,24,1,96,27,3,12,214,94,24,1,95,26,1127,12,214,100,27,2,27,5,12,214,99,24,1,100,27,2,12,214,98,24,1,99,26,1128,12,214,104,27,2,27,6,12,214,103,24,1,104,27,1,12,214,102,24,1,103,26,1129,12,214,108,27,2,27,7,12,214,107,24,1,108,27,0,12,214,106,24,1,107,26,1130,12,214,112,27,3,27,0,12,214,111,24,1,112,27,6,12,214,110,24,1,111,26,596,12,214,116,27,3,27,1,12,214,115,24,1,116,27,5,12,214,114,24,1,115,26,1131,12,214,120,27,3,27,2,12,214,119,24,1,120,27,4,12,214,118,24,1,119,26,1132,12,214,124,27,3,27,3,12,214,123,24,1,124,27,3,12,214,122,24,1,123,26,1133,12,214,128,27,3,27,4,12,214,127,24,1,128,27,2,12,214,126,24,1,127,26,1134,12,214,132,27,3,27,5,12,214,131,24,1,132,27,1,12,214,130,24,1,131,26,1135,12,214,136,27,3,27,6,12,214,135,24,1,136,27,0,12,214,134,24,1,135,26,1136,12,214,140,27,4,27,0,12,214,139,24,1,140,27,5,12,214,138,24,1,139,26,596,12,214,144,27,4,27,1,12,214,143,24,1,144,27,4,12,214,142,24,1,143,26,565,12,214,148,27,4,27,2,12,214,147,24,1,148,27,3,12,214,146,24,1,147,26,1137,12,214,152,27,4,27,3,12,214,151,24,1,152,27,2,12,214,150,24,1,151,26,1138,12,214,156,27,4,27,4,12,214,155,24,1,156,27,1,12,214,154,24,1,155,26,1139,12,214,160,27,4,27,5,12,214,159,24,1,160,27,0,12,214,158,24,1,159,26,1140,12,214,164,27,5,27,0,12,214,163,24,1,164,27,4,12,214,162,24,1,163,26,596,12,214,168,27,5,27,1,12,214,167,24,1,168,27,3,12,214,166,24,1,167,26,567,12,214,172,27,5,27,2,12,214,171,24,1,172,27,2,12,214,170,24,1,171,26,1141,12,214,176,27,5,27,3,12,214,175,24,1,176,27,1,12,214,174,24,1,175,26,1142,12,214,180,27,5,27,4,12,214,179,24,1,180,27,0,12,214,178,24,1,179,26,1143,12,214,184,27,6,27,0,12,214,183,24,1,184,27,3,12,214,182,24,1,183,26,596,12,214,188,27,6,27,1,12,214,187,24,1,188,27,2,12,214,186,24,1,187,26,1144,12,214,192,27,6,27,2,12,214,191,24,1,192,27,1,12,214,190,24,1,191,26,1145,12,214,196,27,6,27,3,12,214,195,24,1,196,27,0,12,214,194,24,1,195,26,1146,12,214,200,27,7,27,0,12,214,199,24,1,200,27,2,12,214,198,24,1,199,26,596,12,214,204,27,7,27,1,12,214,203,24,1,204,27,1,12,214,202,24,1,203,26,570,12,214,208,27,7,27,2,12,214,207,24,1,208,27,0,12,214,206,24,1,207,26,1110,12,214,212,27,8,27,0,12,214,211,24,1,212,27,1,12,214,210,24,1,211,26,596,12,214,216,27,8,27,1,12,214,215,24,1,216,27,0,12,214,214,24,1,215,26,1117,12,214,220,27,9,27,0,12,214,219,24,1,220,27,0,12,214,218,24,1,219,26,596,13,217,24,1,218,26,0,13,213,24,1,214,24,1,217,13,209,24,1,210,24,1,213,13,205,24,1,206,24,1,209,13,201,24,1,202,24,1,205,13,197,24,1,198,24,1,201,13,193,24,1,194,24,1,197,13,189,24,1,190,24,1,193,13,185,24,1,186,24,1,189,13,181,24,1,182,24,1,185,13,177,24,1,178,24,1,181,13,173,24,1,174,24,1,177,13,169,24,1,170,24,1,173,13,165,24,1,166,24,1,169,13,161,24,1,162,24,1,165,13,157,24,1,158,24,1,161,13,153,24,1,154,24,1,157,13,149,24,1,150,24,1,153,13,145,24,1,146,24,1,149,13,141,24,1,142,24,1,145,13,137,24,1,138,24,1,141,13,133,24,1,134,24,1,137,13,129,24,1,130,24,1,133,13,125,24,1,126,24,1,129,13,121,24,1,122,24,1,125,13,117,24,1,118,24,1,121,13,113,24,1,114,24,1,117,13,109,24,1,110,24,1,113,13,105,24,1,106,24,1,109,13,101,24,1,102,24,1,105,13,97,24,1,98,24,1,101,13,93,24,1,94,24,1,97,13,89,24,1,90,24,1,93,13,85,24,1,86,24,1,89,13,81,24,1,82,24,1,85,13,77,24,1,78,24,1,81,13,73,24,1,74,24,1,77,13,69,24,1,70,24,1,73,13,65,24,1,66,24,1,69,13,61,24,1,62,24,1,65,13,57,24,1,58,24,1,61,13,53,24,1,54,24,1,57,13,49,24,1,50,24,1,53,13,45,24,1,46,24,1,49,13,41,24,1,42,24,1,45,13,37,24,1,38,24,1,41,13,33,24,1,34,24,1,37,13,29,24,1,30,24,1,33,13,25,24,1,26,24,1,29,13,21,24,1,22,24,1,25,13,17,24,1,18,24,1,21,13,13,24,1,14,24,1,17,13,9,24,1,10,24,1,13,13,5,24,1,6,24,1,9,13,1,24,1,2,24,1,5,2,4,678], key:22}, 23:{code:[29,24,1,60,0,19,693,0,27,2,17,402,1,12,214,5,23,1,6,23,1,7,12,214,0,24,1,5,23,1,8,12,694,1,26,1109,27,2,24,1,6,24,1,7,24,1,8,8,0,0,2,3,671,1,9,0,0,12,214,3,27,0,27,7,12,214,2,24,1,3,26,596,12,214,6,27,1,27,6,12,214,5,24,1,6,26,537,12,214,9,27,2,27,5,12,214,8,24,1,9,26,1125,12,214,12,27,3,27,4,12,214,11,24,1,12,26,1126,12,214,15,27,4,27,3,12,214,14,24,1,15,26,1127,12,214,18,27,5,27,2,12,214,17,24,1,18,26,1128,12,214,21,27,6,27,1,12,214,20,24,1,21,26,1129,12,214,24,27,7,27,0,12,214,23,24,1,24,26,1130,13,22,24,1,23,26,0,13,19,24,1,20,24,1,22,13,16,24,1,17,24,1,19,13,13,24,1,14,24,1,16,13,10,24,1,11,24,1,13,13,7,24,1,8,24,1,10,13,4,24,1,5,24,1,7,13,1,24,1,2,24,1,4,2,4,678], key:23}, 24:{code:[29,25,1,60,0,19,693,0,27,3,17,402,1,12,214,5,23,1,6,23,1,7,12,214,0,24,1,5,23,1,8,12,694,1,26,1109,24,1,6,27,2,24,1,7,24,1,8,8,0,0,2,3,671,1,9,0,0,12,214,3,27,0,27,7,12,214,2,24,1,3,26,1110,12,214,6,27,1,27,6,12,214,5,24,1,6,26,1118,12,214,9,27,2,27,5,12,214,8,24,1,9,26,1125,12,214,12,27,3,27,4,12,214,11,24,1,12,26,1132,12,214,15,27,4,27,3,12,214,14,24,1,15,26,1137,12,214,18,27,5,27,2,12,214,17,24,1,18,26,1141,12,214,21,27,6,27,1,12,214,20,24,1,21,26,1145,12,214,24,27,7,27,0,12,214,23,24,1,24,26,1110,13,22,24,1,23,26,0,13,19,24,1,20,24,1,22,13,16,24,1,17,24,1,19,13,13,24,1,14,24,1,16,13,10,24,1,11,24,1,13,13,7,24,1,8,24,1,10,13,4,24,1,5,24,1,7,13,1,24,1,2,24,1,4,2,4,683], key:24}, 25:{code:[29,26,1,60,0,19,693,0,27,4,17,402,1,12,214,5,23,1,6,23,1,7,12,214,0,24,1,5,23,1,8,12,694,1,26,1109,24,1,6,24,1,7,27,2,24,1,8,8,0,0,2,3,671,1,9,0,0,12,214,3,27,0,27,7,12,214,2,24,1,3,26,1115,12,214,6,27,1,27,6,12,214,5,24,1,6,26,1122,12,214,9,27,2,27,5,12,214,8,24,1,9,26,1128,12,214,12,27,3,27,4,12,214,11,24,1,12,26,1134,12,214,15,27,4,27,3,12,214,14,24,1,15,26,1138,12,214,18,27,5,27,2,12,214,17,24,1,18,26,1141,12,214,21,27,6,27,1,12,214,20,24,1,21,26,1144,12,214,24,27,7,27,0,12,214,23,24,1,24,26,596,13,22,24,1,23,26,0,13,19,24,1,20,24,1,22,13,16,24,1,17,24,1,19,13,13,24,1,14,24,1,16,13,10,24,1,11,24,1,13,13,7,24,1,8,24,1,10,13,4,24,1,5,24,1,7,13,1,24,1,2,24,1,4,2,4,683], key:25}, 26:{code:[29,27,1,60,0,19,693,0,27,5,17,402,1,12,214,5,23,1,6,23,1,7,12,214,0,24,1,5,23,1,8,12,694,1,26,1109,24,1,6,24,1,7,24,1,8,26,1110,8,0,0,2,3,671,1,9,0,0,12,214,3,27,0,27,2,12,214,2,24,1,3,27,7,12,214,6,27,7,27,2,12,214,5,24,1,6,27,0,13,4,24,1,5,26,0,13,1,24,1,2,24,1,4,2,4,683], key:26}, 27:{code:[29,28,1,60,0,19,693,0,27,6,17,402,1,12,214,0,23,1,5,23,1,6,12,694,1,26,1109,27,2,24,1,5,27,2,24,1,6,8,0,0,2,3,671,1,9,0,0,12,214,2,27,5,26,1128,13,1,24,1,2,26,0,2,4,683], key:27}, 28:{code:[29,29,1,60,0,19,693,0,27,7,17,402,1,12,214,0,23,1,5,23,1,6,12,694,1,26,1109,24,1,5,27,2,27,2,24,1,6,8,0,0,2,3,671,1,9,0,0,12,214,2,27,5,26,1141,13,1,24,1,2,26,0,2,4,678], key:28}, 29:{code:[29,30,1,60,0,19,693,0,27,8,17,402,1,12,214,0,23,1,5,23,1,6,12,694,1,26,1109,27,2,27,2,24,1,5,24,1,6,8,0,0,2,3,671,1,9,0,0,12,214,2,27,5,26,1125,13,1,24,1,2,26,0,2,4,678], key:29}, 30:{code:[29,31,1,60,0,19,693,0,27,9,17,402,1,7,5,0,12,694,1,26,1109,27,2,27,2,27,2,24,1,5,8,0,0,2,3,671,1,9,0,0,10,0,1,2,4,678], key:30}, 31:{code:[29,32,1,60,0,19,695,0,27,1,17,402,1,8,0,0,0,12,270,1,27,1,27,2,3,696,1,9,0,0,14,3,1,2,4,678], key:31}, 32:{code:[29,33,1,60,0,19,695,0,27,2,17,402,1,8,0,0,0,12,642,4,27,3,27,9,12,270,1,24,1,4,27,1,3,696,1,9,0,0,14,28,1,2,4,678], key:32}, 33:{code:[29,34,19,695,0,27,3,19,198,1,23,1,4,19,697,4,26,1150,23,1,5,19,178,5,26,1151,27,0,7,6,0,12,270,1,27,3,26,1151,4,696], key:33}, 34:{code:[29,35,1,60,0,19,695,0,27,4,17,402,1,8,0,0,0,12,698,1,27,2,27,7,3,696,1,9,0,0,14,7,1,2,4,678], key:34}, 35:{code:[29,36,1,60,0,19,695,0,27,5,17,402,1,8,0,0,0,12,701,1,26,1156,3,696,1,8,0,0,0,51,0,1,3,702,1,9,0,0,51,1,1,2,4,703], key:35}, 36:{code:[29,37,1,60,0,19,704,0,27,1,17,402,1,12,705,0,26,1160,8,0,0,1,3,706,1,9,0,0,13,2,26,1160,26,0,13,1,26,1102,24,1,2,2,4,678], key:36}, 37:{code:[29,38,1,60,0,19,704,0,27,2,17,402,1,10,1102,0,8,0,0,1,3,706,1,9,0,0,13,1,26,1102,26,0,2,4,678], key:37}, 38:{code:[29,39,1,60,0,19,704,0,27,3,17,402,1,8,0,0,0,13,4,26,1160,26,0,13,1,26,1102,24,1,4,3,706,1,9,0,0,12,705,1,26,1160,2,4,678], key:38}, 39:{code:[29,40,1,60,0,19,704,0,27,4,17,402,1,8,0,0,0,13,5,26,1162,26,0,13,4,26,1160,24,1,5,13,1,26,1102,24,1,4,3,706,1,9,0,0,12,685,1,26,1160,26,1162,2,4,678], key:39}, 40:{code:[29,41,1,60,0,19,704,0,27,5,17,402,1,13,5,26,571,26,0,13,4,26,1098,24,1,5,13,0,26,566,24,1,4,8,0,0,1,3,706,1,9,0,0,13,5,26,571,26,0,13,4,26,1098,24,1,5,13,3,24,1,4,26,0,13,2,26,566,24,1,3,13,1,26,429,24,1,2,2,4,678], key:40}, 41:{code:[29,42,1,60,0,60,1,60,2,60,3,17,1170,0,17,402,1,8,0,2,0,10,1153,1,3,683,4,8,0,3,0,3,713,4,9,3,0,8,0,0,1,8,0,1,2,40,76,3,4,9,2,0,10,1153,1,3,678,3,9,1,0,10,1153,1,3,678,2,9,0,0,10,402,1,2,4,678,28,2147483821,1,60,0,60,1,60,2,60,3,60,4,60,5,16,0,5,0,16,0,3,1,16,0,4,2,13,4,25,0,4,26,0,13,0,25,0,3,24,1,4,8,0,2,1,3,714,6,40,358,0,6,8,0,0,0,3,715,6,40,286,0,6,43,0,1,9,5,0,8,0,0,1,3,716,6,9,0,0,8,0,1,1,9,2,2,9,3,3,9,4,4,2,41,290,5,29,2147483886,1,60,0,60,1,60,2,60,3,33,3,15,1,5,0,16,0,1,1,16,0,0,2,8,1,5,0,3,717,4,8,0,2,0,3,718,4,32,3,9,1,0,12,198,1,25,0,2,3,683,3,9,0,0,10,1153,1,3,683,1,2,4,719,30,0,1,60,0,60,1,33,0,15,1,5,0,15,1,6,1,16,0,1,2,8,1,6,0,10,125,1,3,683,2,3,720,2,9,1,0,10,1153,1,3,683,2,32,0,2,4,662,254,0,4,707,28,2147484002,1,60,0,60,1,60,2,15,1,6,0,15,1,7,1,16,0,1,2,16,0,2,3,16,0,0,4,8,1,6,0,8,1,7,1,3,683,3,9,2,0,10,402,1,3,683,3,9,1,0,3,721,2,9,0,0,10,1153,1,2,4,683,30,0,4,663,254,0,1,60,0,15,1,3,0,16,0,0,1,8,1,3,0,10,403,1,3,683,1,9,0,0,10,1153,1,3,683,1,2,4,722], key:41}, 42:{code:[29,43,1,60,0,60,1,60,2,60,3,60,4,33,2,17,1181,0,17,402,1,8,0,3,0,10,1153,1,3,683,5,8,0,4,0,3,713,5,9,4,0,8,0,0,1,8,0,1,2,40,96,3,5,9,3,0,10,1153,1,3,678,4,8,0,0,0,3,682,3,8,0,1,0,3,682,3,32,2,9,1,0,10,1153,1,3,678,2,9,0,0,10,403,1,2,4,678,28,2147483841,1,60,0,60,1,60,2,60,3,60,4,60,5,16,0,5,0,16,0,3,1,16,0,4,2,13,4,25,0,4,26,0,13,0,25,0,3,24,1,4,8,0,2,1,3,714,6,40,378,0,6,8,0,0,0,3,715,6,40,306,0,6,43,0,1,9,5,0,8,0,0,1,3,716,6,9,0,0,8,0,1,1,9,2,2,9,3,3,9,4,4,2,41,310,5,29,2147483906,1,60,0,60,1,60,2,60,3,33,3,15,1,5,0,16,0,1,1,16,0,0,2,8,1,5,0,3,717,4,8,0,2,0,3,718,4,32,3,9,1,0,12,198,1,25,0,2,3,683,3,9,0,0,10,1153,1,3,683,1,2,4,719,30,0,1,60,0,60,1,33,0,15,1,5,0,15,1,6,1,16,0,1,2,8,1,6,0,10,125,1,3,683,2,3,720,2,9,1,0,10,1153,1,3,683,2,32,0,2,4,662,254,0,4,708,28,2147484022,1,60,0,60,1,60,2,15,1,6,0,15,1,7,1,16,0,1,2,16,0,2,3,16,0,0,4,8,1,6,0,8,1,7,1,3,683,3,9,2,0,10,402,1,3,683,3,9,1,0,3,721,2,9,0,0,10,1153,1,2,4,683,30,0,4,663,254,0,1,60,0,15,1,3,0,16,0,0,1,8,1,3,0,10,403,1,3,683,1,9,0,0,10,1153,1,3,683,1,2,4,722], key:42}, 43:{code:[29,44,1,60,0,60,1,17,1182,0,17,125,1,8,0,1,0,10,1153,1,3,683,2,8,0,0,0,3,713,2,9,0,0,7,3,1,9,1,2,2,41,43,3,28,2147483788,1,60,0,60,1,60,2,60,3,60,4,60,5,16,0,5,0,16,0,3,1,16,0,4,2,13,4,25,0,4,26,0,13,0,25,0,3,24,1,4,8,0,2,1,3,714,6,40,356,0,6,8,0,0,0,3,715,6,40,274,0,6,43,0,1,9,5,0,8,0,0,1,3,716,6,9,0,0,8,0,1,1,9,2,2,9,3,3,9,4,4,2,41,278,5,29,2147483863,1,60,0,60,1,60,2,60,3,33,3,15,1,5,0,16,0,0,1,16,0,1,2,8,1,5,0,3,717,4,8,0,2,0,3,718,4,32,3,8,0,0,0,12,198,1,25,0,2,3,683,3,9,1,0,10,1153,1,3,711,2,9,0,0,10,125,1,3,711,1,2,4,719,30,0,1,60,0,60,1,60,2,33,0,15,1,5,0,16,0,1,1,16,0,2,2,8,0,1,0,10,125,1,3,683,3,3,720,3,9,2,0,10,1153,1,3,711,3,9,1,0,10,125,1,3,711,2,32,0,2,4,662,254,0,4,709,28,2147484000,1,60,0,60,1,60,2,15,1,6,0,15,1,7,1,16,0,2,2,16,0,0,3,16,0,1,4,8,1,6,0,8,1,7,1,3,683,3,8,0,0,0,10,402,1,3,683,3,9,2,0,3,721,3,9,1,0,10,1153,1,3,711,2,9,0,0,10,125,1,2,4,711,30,0,4,663,254,0,1,60,0,60,1,16,0,0,0,16,0,1,1,8,0,0,0,10,403,1,3,683,2,9,1,0,10,1153,1,3,711,2,9,0,0,10,125,1,3,711,1,2,4,722], key:43}, 44:{code:[29,45,1,60,0,60,1,17,1185,0,17,402,1,8,0,1,0,3,713,2,9,1,0,7,4,1,8,0,0,2,8,1,4,3,40,47,4,2,9,0,0,10,1166,1,2,4,711,28,2147483737,1,60,0,60,1,16,0,0,0,15,1,5,1,15,1,6,2,15,1,7,3,8,0,1,0,3,715,2,3,723,2,9,0,0,9,1,1,2,4,716,30,0,1,60,0,60,1,60,2,15,1,5,0,16,0,1,1,16,0,2,2,16,0,1,3,8,1,5,0,3,717,3,8,0,0,0,3,718,3,9,0,0,9,1,1,9,2,2,9,1,3,2,41,144,4,28,2147483844,1,60,0,60,1,60,2,33,2,15,1,6,0,16,0,1,1,16,0,0,2,16,0,1,3,8,1,6,0,8,0,1,1,3,683,3,32,2,3,724,2,9,0,0,9,1,1,2,4,683,30,0,15,1,5,0,15,1,6,1,15,1,7,2,15,1,8,3,4,719], key:44}, 45:{code:[29,46,17,1192,0,17,402,1,4,730], key:45}, 46:{code:[30,0,1,17,1193,0,17,402,1,10,1194,0,13,1,27,10,26,0,3,731,0,10,1196,0,13,1,27,9,26,0,3,731,0,10,1194,0,13,1,27,10,26,0,3,731,0,10,1197,0,13,5,27,111,26,0,13,4,27,111,24,1,5,13,3,27,102,24,1,4,13,2,27,10,24,1,3,13,1,27,9,24,1,2,2,4,731], key:46}}, clause_keys:[0, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20, 21, 22, 23, 24, 25, 26, 27, 28, 29, 30, 31, 32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46], next_key:47, key:672}, 676: {is_public:false, clauses:{0:{code:[28,1,17,566,0,5], key:0}, 1:{code:[29,2,17,571,0,5], key:1}, 2:{code:[29,3,17,1098,0,5], key:2}, 3:{code:[30,0,17,1099,0,5], key:3}}, clause_keys:[0, 1, 2, 3], next_key:4, key:676}, 700: {is_public:false, clauses:{0:{code:[254,0,19,550,0,26,127,23,1,2,23,1,3,23,1,4,19,550,2,26,127,26,127,23,1,5,23,1,6,19,559,6,23,1,7,26,1153,19,559,4,23,1,8,26,1153,19,188,1,26,403,23,1,9,19,699,9,23,1,10,19,214,10,24,1,3,24,1,8,5], key:0}}, clause_keys:[0], next_key:1, key:700}, 707: {is_public:false, clauses:{0:{code:[254,0,5], key:0}}, clause_keys:[0], next_key:1, key:707}, 708: {is_public:false, clauses:{0:{code:[28,1,5], key:0}, 1:{code:[30,0,5], key:1}}, clause_keys:[0, 1], next_key:2, key:708}, 709: {is_public:false, clauses:{0:{code:[254,0,4,662], key:0}}, clause_keys:[0], next_key:1, key:709}, 710: {is_public:false, clauses:{0:{code:[254,0,10,1166,0,4,668], key:0}}, clause_keys:[0], next_key:1, key:710}, 711: {is_public:false, clauses:{0:{code:[28,1,1,60,0,33,0,15,1,4,0,15,1,5,1,8,1,4,0,8,1,5,1,3,678,1,32,0,2,5], key:0}, 1:{code:[30,0,15,1,3,0,15,1,4,1,12,712,0,25,1,3,25,1,4,4,668], key:1}}, clause_keys:[0, 1], next_key:2, key:711}, 723: {is_public:false, clauses:{0:{code:[254,0,1,60,0,60,1,8,0,1,0,10,1153,1,3,683,2,8,0,0,0,3,713,2,9,0,0,7,3,1,9,1,2,40,43,3,2,10,1183,0,2,4,668,28,2147483788,1,60,0,60,1,60,2,60,3,60,4,60,5,16,0,5,0,16,0,3,1,16,0,4,2,13,4,25,0,4,26,0,13,0,25,0,3,24,1,4,8,0,2,1,3,714,6,40,362,0,6,8,0,0,0,3,715,6,40,278,0,6,43,0,1,9,5,0,8,0,0,1,3,716,6,9,0,0,8,0,1,1,9,2,2,9,3,3,9,4,4,2,41,282,5,29,2147483865,1,60,0,60,1,60,2,60,3,33,3,15,1,5,0,16,0,0,1,16,0,1,2,8,1,5,0,3,717,4,8,0,2,0,3,718,4,32,3,8,0,0,0,12,198,1,25,0,2,3,683,3,9,1,0,10,1153,1,3,711,2,9,0,0,12,198,1,26,1166,3,711,1,2,4,719,30,0,1,60,0,60,1,60,2,33,0,15,1,5,0,16,0,1,1,16,0,2,2,8,0,1,0,10,125,1,3,683,3,3,720,3,9,2,0,10,1153,1,3,711,3,9,1,0,12,198,1,26,1166,3,711,2,32,0,2,4,662,254,0,4,710,28,2147484006,1,60,0,60,1,60,2,15,1,6,0,15,1,7,1,16,0,2,2,16,0,0,3,16,0,1,4,8,1,6,0,8,1,7,1,3,683,3,8,0,0,0,10,402,1,3,683,3,9,2,0,3,721,3,9,1,0,10,1153,1,3,711,2,9,0,0,12,198,1,26,1166,2,4,711,30,0,4,663,254,0,1,60,0,60,1,16,0,0,0,16,0,1,1,8,0,0,0,10,403,1,3,683,2,9,1,0,10,1153,1,3,711,2,9,0,0,12,198,1,26,1166,3,711,1,2,4,722], key:0}}, clause_keys:[0], next_key:1, key:723}, 725: {is_public:false, clauses:{0:{code:[28,1,21,1,0,17,566,1,5], key:0}, 1:{code:[30,0,21,1,0,17,1098,1,5], key:1}}, clause_keys:[0, 1], next_key:2, key:725}, 727: {is_public:false, clauses:{0:{code:[254,0,15,1,2,0,8,1,2,0,4,726], key:0}}, clause_keys:[0], next_key:1, key:727}, 730: {is_public:false, clauses:{0:{code:[254,0,12,728,2,27,1,23,1,1,13,4,26,1098,26,0,13,3,26,566,24,1,4,12,729,0,24,1,1,24,1,2,24,1,3,4,727], key:0}}, clause_keys:[0], next_key:1, key:730}};
foreign_predicates = {0: predicate_acyclic_term, 1: predicate_subsumes_term, 2: predicate_compare, 3: predicate_var, 4: predicate_atom, 5: predicate_integer, 6: predicate_float, 7: predicate_compound, 8: predicate_ground, 9: predicate_unify, 10: predicate_match, 11: predicate_functor, 12: predicate_arg, 13: predicate_univ, 14: predicate_copy_term, 15: predicate_halt, 16: predicate_current_prolog_flag, 17: predicate_set_prolog_flag, 18: predicate_repeat, 19: predicate_atom_length, 20: predicate_atom_concat, 21: predicate_sub_atom, 22: predicate_char_code, 23: predicate_atom_chars, 24: predicate_atom_codes, 25: predicate_number_chars, 26: predicate_number_codes, 27: predicate_char_conversion, 28: predicate_current_char_conversion, 29: predicate_current_predicate, 30: predicate_term_gt, 31: predicate_term_egt, 32: predicate_term_lt, 33: predicate_term_elt, 34: predicate_is, 35: predicate_gt, 36: predicate_lt, 37: predicate_elt, 38: predicate_egt, 39: predicate_eq, 40: predicate_ne, 41: predicate_set_input, 42: predicate_set_output, 43: predicate_current_output, 44: predicate_current_input, 45: predicate_get_char, 46: predicate_get_code, 47: predicate_peek_char, 48: predicate_peek_code, 49: predicate_put_char, 50: predicate_put_code, 51: predicate_get_byte, 52: predicate_peek_byte, 53: predicate_put_byte, 54: predicate_flush_output, 55: predicate_at_end_of_stream, 56: predicate_set_stream_position, 57: predicate_stream_property, 58: predicate_current_stream, 59: predicate_write_term, 60: predicate_current_op, 61: predicate_fail, 62: predicate_true, 63: predicate_term_variables, 64: writeln, 65: predicate_gensym, 66: atom_to_term, 67: predicate_clause, 68: predicate_abolish, 69: predicate_retract_clause, 70: read_term, 71: predicate_open, 72: predicate_close, 73: predicate_op, 74: atom_to_memory_file, 75: memory_file_to_atom, 76: new_memory_file, 77: open_memory_file, 78: free_memory_file, 79: predicate_format, 80: predicate_flag, 81: predicate_memory_file_description, 82: reset_compile_buffer, 83: emit_code, 84: predicate_lookup_atom, 85: predicate_lookup_float, 86: predicate_lookup_functor, 87: add_clause_to_predicate, 88: add_clause_to_aux, 89: prepend_clause_to_predicate, 90: predicate_flush_stdout, 91: predicate_debug, 92: predicate_nodebug, 93: predicate_jmp, 94: predicate_generate_initialization_goal, 95: predicate_generate_system_goal, 96: predicate_define_dynamic_predicate, 97: predicate_compiled_state_boot_code, 98: predicate_dump_tables, 99: predicate_add_module_export, 100: predicate_method_export, 101: predicate_request_result, 102: predicate_handle_result, 103: predicate_fetch_promise, 104: predicate_trace_unify, 105: predicate_trace_set, 106: predicate_trace_value, 107: predicate_trace_set_info, 108: predicate_suspend_set, 109: predicate_get_terminal_char, 110: predicate_trace_set_retry, 111: predicate_trace_retry_value, 112: predicate_trace_set_prompt, 113: predicate_get_backtrack_frame, 114: predicate_set_backtrack_frame, 115: predicate_trace_instruction_set, 116: member, 117: mark_top_choicepoint, 118: unmark_choicepoint, 119: unmark_top_choicepoint, 120: get_current_block, 121: install_new_block, 122: reset_block, 123: unwind_stack, 124: clean_up_block, 125: predicate_throw, 126: get_exception, 127: clear_exception, 128: recorda, 129: recordz, 130: recorded, 131: erase, 132: record_term, 133: predicate_gc, 134: predicate_statistics, 135: predicate_wam_duration, 136: predicate_eval_javascript, 137: predicate_eval_javascript, 138: predicate_remove_dom_element_class, 139: predicate_replace_dom_element_class, 140: predicate_toggle_dom_element_class, 141: predicate_set_dom_element_attribute_value, 142: predicate_dom_element_attribute_value, 143: predicate_create_dom_element, 144: predicate_create_dom_text_node, 145: predicate_append_dom_node_child, 146: predicate_insert_before_dom_node, 147: predicate_dom_select_element, 148: predicate_dom_select_all_elements, 149: predicate_dom_object_property, 150: predicate_dom_object_method, 151: predicate_dom_object_method, 152: predicate_dom_object_type, 153: predicate_dom_create_object, 154: predicate_dom_create_object, 155: predicate_dom_type_reference, 156: predicate_dom_release_object, 157: predicate_set_dom_object_property, 158: predicate_set_dom_object_property, 159: predicate_alert, 160: predicate_dom_window, 161: predicate_dom_type_property, 162: predicate_dom_type_method, 163: predicate_dom_type_parent};
system = [];
initialization = [];
module_exports = {2 : [[3,1],[5,2],[7,3],[9,1],[11,1],[13,1],[15,1],[17,1],[19,1],[21,2],[23,2],[25,3],[27,3],[29,2],[31,2],[33,0],[33,1],[35,2],[37,2],[39,0],[41,2],[43,3],[45,5],[47,2],[49,2],[51,2],[53,2],[55,2],[57,2],[59,2],[61,1],[63,2],[65,2],[67,2],[69,2],[71,2],[73,2],[75,2],[77,2],[79,2],[81,2],[83,2],[85,1],[87,1],[89,1],[91,1],[93,1],[93,2],[95,1],[95,2],[97,1],[97,2],[99,2],[101,1],[101,2],[103,1],[103,2],[105,1],[105,2],[107,1],[107,2],[109,1],[109,2],[111,0],[111,1],[113,1],[115,2],[117,2],[119,1],[121,2],[121,3],[123,3],[125,0],[127,0],[129,2],[131,1],[133,2],[135,3],[137,2],[139,1],[141,2],[143,2],[143,3],[145,3],[145,4],[147,1],[147,2],[149,3],[151,2],[153,2],[155,1],[157,3],[159,1],[161,2],[161,3],[163,3],[165,2],[167,0],[169,2],[171,2],[173,2],[175,3],[177,3],[179,4],[181,3],[183,0],[185,0],[187,0],[189,1],[191,1],[193,1],[195,1],[197,1],[199,1],[201,2],[203,2],[205,1],[207,2],[209,2],[211,2],[213,1],[215,1],[217,1],[219,1],[221,1],[223,1],[225,1],[227,1],[229,1],[231,1],[233,1],[235,2],[237,2],[239,1],[241,0],[243,1],[245,1],[247,1],[249,0],[251,1],[253,1],[255,1],[257,0],[259,3],[261,3],[263,3],[265,1],[267,2],[269,0],[271,0],[273,1],[275,1],[275,2],[277,2],[279,3],[281,3],[283,3],[285,3],[287,2],[289,2],[291,2],[293,3],[295,2],[297,2],[299,4],[301,2],[301,3],[303,2],[305,2],[305,3],[307,4],[309,1],[311,3],[311,4],[313,1],[315,1],[317,4],[319,5],[321,2],[331,3],[332,1],[333,1],[334,1],[335,1],[336,1],[337,1],[338,1],[339,1],[340,1],[341,1],[342,0],[343,2],[344,3],[345,3],[346,3],[347,1],[348,1],[349,1],[350,2],[351,2],[352,2],[353,1],[354,1],[355,2],[356,1],[356,2],[357,1],[357,2],[358,2],[359,1],[359,2],[360,1],[361,1],[362,2],[363,2],[364,2],[365,3],[366,1],[366,2],[366,3],[366,4],[366,5],[366,6],[366,7],[366,8],[425,4],[425,5],[484,0],[485,0],[486,0],[487,1],[599,1],[600,1],[601,1],[602,2],[603,2],[604,3],[623,1],[645,2]], 330 : [[33,0],[93,1],[95,1],[97,1],[101,1],[103,1],[105,1],[107,1],[109,1],[111,0],[121,2],[143,2],[145,3],[147,1],[161,2],[331,3],[332,1],[333,1],[334,1],[335,1],[336,1],[337,1],[338,1],[339,1],[340,1],[341,1],[342,0],[343,2],[344,3],[345,3],[346,3],[347,1],[348,1],[349,1],[350,2],[351,2],[352,2],[353,1],[354,1],[355,2],[356,1],[356,2],[357,1],[357,2],[358,2],[359,1],[359,2],[360,1],[361,1],[362,2],[363,2],[364,2],[365,3],[366,1],[366,2],[366,3],[366,4],[366,5],[366,6],[366,7],[366,8]], 483 : [[484,0],[485,0],[486,0],[487,1]], 598 : [[599,1],[600,1],[601,1],[602,2],[603,2],[604,3]], 623 : [[425,4],[425,5],[623,1]], 644 : [[645,2]], 647 : [[648,1],[649,1],[650,2]], 651 : [[652,2]], 653 : [[169,2],[171,2],[173,2],[175,3],[177,3],[179,4],[339,1],[654,3],[655,2],[656,2],[657,2],[658,5],[659,2],[660,2],[661,1],[662,1],[663,1],[664,3],[665,2]], 748 : [[749,4],[750,2],[751,2],[752,2],[753,1]]};
}
var stdout_buffer = "";

stdout = function (msg)
{
    // if(! debugging) {
    //     return;
    // }

    var lines = (stdout_buffer + msg).split('\n');
    for (var i = 0; i < lines.length-1; i++)
    {
        console.log(lines[i]);
    }
    stdout_buffer = lines[lines.length-1];
};

predicate_flush_stdout = function ()
{
    if (stdout_buffer !== "")
        stdout("\n");
    return true;
};

function alert(msg) {
    console.log('alert:' + msg);
}
module.exports = {proscriptls_toplevel};
