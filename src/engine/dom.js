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

        debug_msg("Is retry! Setting cursor back to " + cursor);
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

        debug_msg("Not a retry");
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

        debug_msg("Is retry! Setting cursor back to " + cursor);
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

        debug_msg("Not a retry");
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
    let ftor = VAL(lookup_functor("consult_atom", 1));
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

    let ftor = VAL(lookup_functor("consult", 1));
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
    var ftor = VAL(lookup_functor("call_atom", 2));
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
