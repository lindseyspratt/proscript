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
    debug_msg("Read expression: " + expression_to_string(expression));
    
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
    debug_msg("A term has been created ( " + VAL(t1) + " ). Reading it back from the heap gives: " + term_to_string(t1));
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
            debug_msg("Expression terminated with fullstop")
        }
        else
        {
            debug("Error: Expression terminated with >" + p + "<");
        }
        debug_msg(expression_to_string(e));
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
    debug_msg("Expression: " + expression_to_string({list:arglist, tail:[]}));
    debug_msg("Bindings have been created ( " + VAL(t2) + " ). Reading it back from the heap gives: " + term_to_string(t2));
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

