/*
The general approach to providing access to Javascript objects
through Prolog predicates is described in object_property.js.

This file implements access to Javascript object methods.


 */

// object_method(Element, add_event_listener(click, bar(thing))).
// method:EventAgent.addEventListener, arguments:[string, goal_function]
// object_method_no_return(objectJS, EventAgent.addEventListener, [eventJS, handlerFunction]);

function predicate_dom_object_method(object, methodStructure) {
    if (TAG(object) !== TAG_STR) {
        instantiation_error(object);
    }
    if (TAG(methodStructure) !== TAG_STR && TAG(methodStructure) !== TAG_ATM) {
        instantiation_error(methodStructure);
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

    let spec = getInterfaceItemSpec(objectType, 'method', methodName);
    if (spec.returns && spec.returns.type !== 'boolean') {
        arity--; // the last argument to the methodStructure is for the return value.
    }

    let specArguments = spec.arguments;
    let applyArguments = [];
    for (var i = 0; i < arity; i++) {
        let specArgument = specArguments[i];
        let applyArgumentContainer = {};
        if (convert_method_argument(memory[VAL(methodStructure) + i + 1], specArgument, applyArgumentContainer)) {
            applyArguments.push(applyArgumentContainer.value);
        } else {
            return false;
        }
    }

    if (spec.returns) {
        let resultJS = object_method_return(objectJS, spec.name, applyArguments);
        let resultContainer = {};
        if(convert_result(resultJS, spec.returns, resultContainer)) {
            let resultPL = resultContainer.value;
            if (spec.returns.type === 'boolean') {
                return resultPL;
            } else {
                return unify(resultPL, memory[VAL(methodStructure) + arity + 1]);
            }
        } else {
            return false;
        }
    } else {
        object_method_no_return(objectJS, spec.name, applyArguments);
        return true;
    }
}

function convert_method_argument(term, spec, resultContainer, reportError) {
    if(TAG(term) === TAG_REF) {
        return instantiation_error(term);
    }

    let arg;
    if(typeof spec.type === 'object') {
        // union of multiple types
        for(let subtype of spec.type) {
            if(convert_method_argument(term, {type: subtype}, resultContainer, false)) {
                return true;
            }
        }
        return type_error('union: ' + spec.type, term);
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
        let container = {};
        if (getIntegerPropertyValue(term, container, reportError)) {
            arg = container.value;
        } else {
            return false;
        }
    } else if(spec.type === 'number') {
        let container = {};
        if (getNumberPropertyValue(term, container, reportError)) {
            arg = container.value;
        } else {
            return false;
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

        var keyValuePairPL = memory[VAL(list)];
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

            let keyPL = memory[VAL(keyValuePairPL) + 1];
            if(TAG(keyPL) !== TAG_ATM) {
                return type_error('key - value: key should be an atom.', keyPL);
            }
            let keyJS = atable[VAL(keyPL)];

            // TODO: extend value to allow any JSON-ish type - atom, number, boolean, list/JSON array, or keyValue list/JSON object.
            let valuePL = memory[VAL(keyValuePairPL) + 2];
            if(TAG(valuePL) !== TAG_ATM) {
                return type_error('key - value: value should be an atom.', valuePL);
            }

            options[keyJS] = atable[VAL(valuePL)];

            list = memory[VAL(list) + 1];
        }
    }

    optionsContainer.value = options;
    return true;
}

function convert_result(resultJS, spec, resultContainer) {
    let resultPL;
    if(spec.type === 'atom') {
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
        memory[state.H++] = PL_put_integer(resultJS.left);
        memory[state.H++] = PL_put_integer(resultJS.top);
        memory[state.H++] = PL_put_integer(resultJS.right);
        memory[state.H++] = PL_put_integer(resultJS.bottom);
        memory[state.H++] = PL_put_integer(resultJS.x);
        memory[state.H++] = PL_put_integer(resultJS.y);
        memory[state.H++] = PL_put_integer(resultJS.width);
        memory[state.H++] = PL_put_integer(resultJS.height);
    } else {
        return type_error('method result specification type', lookup_atom(spec.type));
    }
    resultContainer.value = resultPL;
    return resultPL;
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
