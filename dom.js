/*
predicates to read and modify the javascript DOM for HTML
 */

/** @namespace Map */

var idsToElements = new Map();
var elementsToIDs = new Map();
var deavCursors = new Map();
var deavCursorCounter = 0;
var desaCursors = new Map();
var desaCursorCounter = 0;


function predicate_set_dom_element_attribute_value(element, attribute, value) {
    if (TAG(element) !== TAG_STR) {
        instantiation_error(element);
    }

    if (TAG(attribute) !== TAG_ATM) {
        instantiation_error(attribute);
    }

    if (TAG(value) !== TAG_ATM) {
        instantiation_error(value);
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
        instantiation_error(element);
    }

    if (TAG(value) !== TAG_ATM) {
        instantiation_error(value);
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
        instantiation_error(element);
    }

    if (TAG(oldValue) !== TAG_ATM) {
        instantiation_error(oldValue);
    }

    if (TAG(value) !== TAG_ATM) {
        instantiation_error(value);
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
        instantiation_error(element);
    }

    if (TAG(value) !== TAG_ATM) {
        instantiation_error(value);
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
        return bind(action, lookup_atom(actionJS))
    } else {
        if (TAG(action) !== TAG_ATM) {
            instantiation_error(action);
        }

        actionJS = atable[VAL(action)];
        if(actionJS === 'add') {
            flag = true;
        } else if(actionJS === 'remove') {
            flag = false;
        } else {
            domain_error(action);
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
        cursor = {
            elements: setupElements(element, attribute, value),
            attributes: setupAttributes(element, attribute),
            values: setupValues(element, attribute, value)
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
                cursor.values = setupValuesFromJSElementAndAttribute(elementJS, attributeJS, value);
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

function setupValues(element, attribute, value) {
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
            instantiation_error(element);
        }

        if (TAG(attribute) !== TAG_ATM) {
            instantiation_error(attribute);
        }

        var elementObject = {};
        if (!get_element_object(element, elementObject)) {
            return undefined;
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
    return values;
}

function setupValuesFromJSElementAndAttribute(elementJS, attributeJS, value) {
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
    var elementMapID = lookup_element(elementJS);
    var lookupElement = lookup_atom(elementMapID);
    // '$element'(lookupElement)
    var ftor = lookup_functor('$element', 1);
    var elementPL = alloc_structure(ftor);
    memory[state.H++] = lookupElement;
    return elementPL;
}

function lookup_element(elementTerm) {
    var elementMapID = elementsToIDs.get(elementTerm);
    if (elementMapID) {
        return elementMapID;
    }

    elementMapID = 'emi' + elementsToIDs.size;
    elementsToIDs.set(elementTerm, elementMapID);
    idsToElements.set(elementMapID, elementTerm);
    return elementMapID;
}

// ElementStructure = '$element'(emi1234)
// elementIDObject = {"value":"emi1234"}
// elementObject = {"value":element}

function get_element_object(term, ref) {
    var elementIDObject = {};
    if (!get_element_id_object(term, elementIDObject)) {
        return false;
    }
    ref.value = idsToElements.get(elementIDObject.value);
    return true;
}

// ElementStructure = '$element'(emi1234)
// X = VAL(term)
// ftor = memory[X]
// V = memory[X+1] = atomOfst for element key
//
// F = ftable[ftor][0]
// atable[F] = "$element"
//
// E = atable[VAL(V)] = "emi1234"
//
// s = {"value": E}

function get_element_id_object(term, s) {
    if (TAG(term) !== TAG_STR)
        return type_error("element", term);
    var ftor = VAL(memory[VAL(term)]);
    if (atable[ftable[ftor][0]] === "$element" && ftable[ftor][1] === 1) {
        var arg = memory[VAL(term) + 1];
        if (TAG(arg) !== TAG_ATM)
            return type_error("element_arg");
        s.value = atable[VAL(arg)];
        return true;
    }
    return type_error("element", term);
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

function codes_to_string(codes) {
    var string = '';

    var list = codes;

    while(list !== NIL) {
        if(TAG(list) !== TAG_LST) {
            instantiation_error(list);
        }

        var codePL = memory[VAL(list)];
        if(TAG(codePL) !== TAG_INT) {
            instantiation_error(codePL);
        } else {
            string += String.fromCharCode(codePL);
            list = memory[VAL(list) + 1];
        }
    }
    return string;
}

function predicate_alert(term) {
    var termJS = format_term(term, {});
    alert(termJS);
    return true;
}

function predicate_create_dom_element(tag, element) {
    if(TAG(tag) !== TAG_ATM) {
        instantiation_error(tag);
    }

    if(TAG(element) !== TAG_REF) {
        instantiation_error(element);
    }

    var tagJS = atable[VAL(tag)];
    var elementJS = document.createElement(tagJS);
    var elementPL = create_element_structure(elementJS);

    return unify(element, elementPL);
}

function predicate_create_dom_text_node(text, element) {
    if(TAG(text) !== TAG_LST) {
        instantiation_error(text);
    }

    if(TAG(element) !== TAG_REF) {
        instantiation_error(element);
    }

    var textJS = codes_to_string(text);
    var elementJS = document.createTextNode(textJS);
    var elementPL = create_element_structure(elementJS);

    return unify(element, elementPL);

}

function predicate_append_dom_node_child(element, child) {
    if(TAG(child) !== TAG_STR) {
        instantiation_error(child);
    }

    if(TAG(element) !== TAG_STR) {
        instantiation_error(element);
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
        instantiation_error(parent);
    }

    if (TAG(before) !== TAG_STR) {
        instantiation_error(before);
    }

    if (TAG(element) !== TAG_STR) {
        instantiation_error(element);
    }

    var parentObject = {};
    if (!get_element_object(parent, parentObject)) {
        existence_error("element", parent);
    }
    var parentJS = parentObject.value;

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        existence_error("element", element);
    }
    var elementJS = elementObject.value;

    var beforeObject = {};
    if (!get_element_object(before, beforeObject)) {
        existence_error("element", before);
    }
    var beforeJS = beforeObject.value;

    parentJS.insertBefore(beforeJS, elementJS);

    return true;

}

function predicate_dom_select_element(query, element) {
    if (TAG(element) !== TAG_STR && TAG(element) !== TAG_REF) {
        instantiation_error(element);
    }

    if (TAG(query) !== TAG_LST) {
        instantiation_error(query);
    }

    var queryJS = codes_to_string(query);

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
        cursor = {
            elements: setupElementsForSelectAll(query)
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

function setupElementsForSelectAll(query) {

    var queryJS = codes_to_string(query);

    return document.querySelectorAll(queryJS);
}

// proscript_init generally is only used once in a web page to set up the proscript globals.
// Additional calls of Prolog queries should use proscript to avoid overwriting the global data,
// particularly the predicates from assertions.

function proscript_init(queryJS) {
    load_state();
    proscript(queryJS);
}

// proscript calls the given query using the existing global data,
// particularly the current predicates definitions. This allows the
// asserta/assertz clauses to persist across calls of proscript.

function proscript(queryJS) {
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
            stdout("false.\n");
        }
    }
    catch (anything)
    {
        if(console && console.log) {
            console.log(anything);
        }
        debug("Error. " + anything);
    }
}

function debug(msg) {
    if(debugging) {
        alert(msg);
    }
}
