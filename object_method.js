/*
The general approach to providing access to Javascript objects
through Prolog predicates is described in object_property.js.

This file implements access to Javascript object methods.


 */

var objectMethodSpecs = new Map(); // key is object type, e.g. element

// object_method(Element, add_event_listener(click, bar(thing))).
// method:EventAgent.addEventListener, arguments:[string, goal_function]
// object_method_no_return(objectJS, EventAgent.addEventListener, [eventJS, handlerFunction]);

function predicate_object_method(object, methodStructure) {
    if (TAG(object) !== TAG_STR) {
        instantiation_error(object);
    }
    if (TAG(methodStructure) !== TAG_STR) {
        instantiation_error(method);
    }

    var objectContainer = {};
    if (!get_object_container(object, objectContainer)) {
        return false;
    }
    let objectType = objectContainer.type;
    var objectJS = objectContainer.value;

    let methodName = atable[ftable[VAL(memory[VAL(methodStructure)])][0]];
    let spec = get_method_spec(objectType, methodName);
    var arity = ftable[VAL(memory[VAL(methodStructure)])][1];
    if(spec.returns && ! spec.returns.type === 'boolean') {
        arity --; // the last argument to the methodStructure is for the return value.
    }

    let specArguments = spec.arguments;
    let applyArguments = [];
    for (var i = 0; i < arity; i++)
    {
        let specArgument = specArguments[i];
        let applyArgument = convert_method_argument(memory[VAL(methodStructure)+i+1], specArgument);
        applyArguments.push(applyArgument);
    }

    if(spec.returns) {
        let resultJS = object_method_return(objectJS, spec.name, applyArguments);
        let resultPL = convert_result(resultJS, spec.returns);
        if(spec.returns.type === 'boolean') {
            return resultPL;
        } else {
            return unify(resultPL, memory[VAL(methodStructure) + arity + 1]);
        }
    } else {
        object_method_no_return(objectJS, spec.name, applyArguments);
        return true;
    }
}

function convert_method_argument(term, spec) {
    let arg;
    if(spec.type === 'string') {
        if (TAG(term) === TAG_ATM) {
            arg = PL_atom_chars(term);
        } else {
            arg = format_term(term, {quoted:true});
        }
    } else if(spec.type === 'string_codes') {
        if (TAG(term) === TAG_ATM) {
            arg = codes_to_string(term);
        } else {
            // error
        }
    } else if(spec.type === 'boolean') {
        if (TAG(term) === TAG_ATM) {
            let value = PL_atom_chars(term);
            arg = (value === 'true' ? true : value === 'false' ? false : (throw 'boolean must be "true" or "false": ' + value));
        } else {
            // error
        }
    } else if(spec.type === 'position') {
        if (TAG(term) === TAG_ATM) {
            let arg = PL_atom_chars(term);
            if(["afterbegin", "afterend", "beforebegin", "beforeend"].indexOf(arg) === -1) {
                domain_error("not_valid_insert_adjacent_mode", mode);
                // error
            }

        } else {
            // error
        }
    } else if(spec.type === 'goal_function') {
        let goal;
        if (TAG(term) === TAG_ATM) {
            goal = PL_atom_chars(term);
        } else if (TAG(term) === TAG_STR) {
            goal = format_term(term, {quoted:true});
        } else {
            // error
        }

        let arg = function () {
            proscript(goal)
        }
    } else {
        // error
    }

    return arg;
}

function convert_result(resultJS, spec) {
    let resultPL;
    if(spec.type === 'atom') {
        resultPL = lookup_atom(resultJS);
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
        // error
    }
    return resultPL;
}

function object_method_no_return() {
    let object = arguments[0];
    let object_method = arguments[1];
    let method_arguments = [];
    for(let ofst = 2;ofst < arguments.length;ofst++) {
        method_arguments[ofst] = arguments[ofst+1];
    }
    Reflect.apply(object_method, object, method_arguments);
}

function object_method_return() {
    let object = arguments[0];
    let object_method = arguments[1];
    let method_arguments = [];
    for(let ofst = 2;ofst < arguments.length;ofst++) {
        method_arguments[ofst] = arguments[ofst+1];
    }
    return Reflect.apply(object_method, object, method_arguments);
}

var eventTargetMethodSpecs = new Map([
    ['addEventListener',{name:'addEventListener',arguments:[{type:'string'},{type:'goal_function'}]}]
]);

objectMethodSpecs.set('eventtarget',eventTargetMethodSpecs);

var nodeMethodSpecs = new Map([
    ['cloneNode',{name:'cloneNode',arguments:[{type:'boolean'}],returns:{type:'object'}}],
    ['compareDocumentPosition',{name:'compareDocumentPosition',arguments:[{type:'object'}],returns:{type:'number'}}],
    ['contains',{name:'contains',arguments:[{type:'object'}],returns:{type:'boolean'}}],
    ['isEqualNode',{name:'isEqualNode',arguments:[{type:'object'}],returns:{type:'boolean'}}],
    ['normalize',{name:'normalize',arguments:[]}]
]);

objectMethodSpecs.set('node',nodeMethodSpecs);

var elementMethodSpecs = new Map([
    ['getBoundingClientRect',{name:'getBoundingClientRect',arguments:[],returns:{type:'dom_rect'}}],
    ['insertAdjacentElement',{name:'insertAdjacentElement',arguments:[{type:'position'},{type:'object'}]}],
    ['insertAdjacentHTML',{name:'insertAdjacentHTML',arguments:[{type:'position'},{type:'string_codes'}]}],
    ['insertAdjacentText',{name:'insertAdjacentText',arguments:[{type:'position'},{type:'string_codes'}]}],
    ['scrollIntoView',{name:'scrollIntoView',arguments:[{type:'boolean'}]}]
]);

objectMethodSpecs.set('element',elementMethodSpecs);

var htmlElementMethodSpecs = new Map([
    ['blur',{name:'blur',arguments:[]}],
    ['click',{name:'click',arguments:[]}],
    ['focus',{name:'focus',arguments:[]}]
]);

objectMethodSpecs.set('htmlelement',htmlElementMethodSpecs);
