var depCursors = new Map();
var depCursorCounter = 0;

// dom_element_property(E, tag, T) binds T to all tags of E.
// property must be a bound value (i.e. not TAG(property) === TAG_REF)
function predicate_dom_element_property(element, property, value) {
    var cursor;
    var cursorIDPL;
    var cursorIDJS;

    if (state.foreign_retry) {
        cursorIDPL = state.foreign_value;
        cursorIDJS = atable[VAL(cursorIDPL)];
        cursor = depCursors.get(cursorIDJS);

        debug_msg("Is retry! Setting cursor back to " + cursor);
    }
    else {
        cursor = {
            elements: setupElementsForPropertyValue(element, property, value),
            property_values: setupPropertyValues(element, property, value)
        };
        cursorIDJS = 'crs' + depCursorCounter++;
        depCursors.set(cursorIDJS, cursor);
        cursorIDPL = lookup_atom(cursorIDJS);

        debug_msg("Not a retry");
        create_choicepoint();
    }

    update_choicepoint_data(cursorIDPL);

    if (cursor.elements && cursor.elements.length > 0) {
        var elementJS = cursor.elements[0];
        var elementPL = create_element_structure(elementJS);

        if (!cursor.property_values) {
            cursor.property_values = setupPropertyValuesFromJSElement(elementJS, property, value);
        }

        if (cursor.property_values && cursor.property_values.length > 0) {
            var classJS = cursor.property_values.pop();
            var classPL = propertyValueToPL(property, classJS);
            return unify(value, classPL) &&
                unify(element, elementPL);
        }

        // All classNames for current elementJS have been processed.
        // Set the cursor.tags to undefined to force recalculation of tags
        // with next element.
        // Move to the next element by removing elements[0].

        cursor.property_values = undefined;
        cursor.elements = cursor.elements.slice(1);
        return false; // go to next choice (of element)
    } else {
        destroy_choicepoint();
        return false;
    }
}


function setupElementsForPropertyValue(element, property, value) {
    if (TAG(property) === TAG_REF) {
        instantiation_error(property);
    } else {
        var propertyJS = atable[VAL(property)];
    }

    if (TAG(element) !== TAG_REF) {
        if (TAG(element) !== TAG_STR) {
            instantiation_error(element);
        }
        var elementObject = {};
        if (!get_element_object(element, elementObject)) {
            return undefined;
        }
        var elementJS = elementObject.value;
        var elements = [];
        elements.push(elementJS);
        return elements;
    } else if (TAG(value) !== TAG_REF) {
        return setupElementsForBoundPropertyValue(propertyJS, value);
    } else {
        return Array.from(document.querySelectorAll('*'));
    }
}

function setupPropertyValuesFromJSElement(elementJS, property, value) {
    if (TAG(property) === TAG_REF) {
        instantiation_error(property);
    } else {
        var propertyJS = atable[VAL(property)];
    }
    var values;
    if (TAG(value) !== TAG_REF) {
        values = setupPropertyValuesFromBoundValue(property, propertyJS, value);
    } else {
        values = setupPropertyValuesFromJSElement1(elementJS, property, propertyJS);
    }
    return values;
}

function setupPropertyValues(element, property, value) {
    if (TAG(property) === TAG_REF) {
        instantiation_error(property);
    } else {
        var propertyJS = atable[VAL(property)];
    }
    var values;
    if (TAG(value) !== TAG_REF) {
        values = setupPropertyValuesFromBoundValue(property, propertyJS, value);
    } else if (TAG(element) !== TAG_REF) {
        if (TAG(element) !== TAG_STR) {
            instantiation_error(element);
        }
        var elementObject = {};
        if (!get_element_object(element, elementObject)) {
            return undefined;
        }
        var elementJS = elementObject.value;
        values = setupPropertyValuesFromJSElement1(elementJS, property, propertyJS);
    } else {
        values = undefined;
    }
    return values;
}

function propertyValueToJS(type, value) {
    if(type === 'atom') {
        return getAtomPropertyValue(value);
    } else if(type === 'number') {
        return getNumberPropertyValue(value);
    } else if(type === 'string') {
        return getStringPropertyValue(value);
    } else if(type === 'element') {
        return getElementPropertyValue(value);
    } else {
        domain_error(type);
    }
}

function getAtomPropertyValue(value) {
    return atable[VAL(value)];
}

function getNumberPropertyValue(value) {
    return VAL(value);
}

function getStringPropertyValue(value) {
    return codes_to_string(value);
}

function getElementPropertyValue(value) {
    var valueElementObject = {};
    if (!get_element_object(value, valueElementObject)) {
        return undefined;
    }
    return valueElementObject.value;
}

function propertyValueToPL(property, valueJS) {
    var propertySpec = propertyMap.get(atable[VAL(property)]);
    if (propertySpec) {
        if (propertySpec.type === 'atom') {
            return getAtomPLPropertyValue(valueJS);
        } else if (propertySpec.type === 'number') {
            return getNumberPLPropertyValue(valueJS);
        } else if (type === 'string') {
            return getStringPLPropertyValue(valueJS);
        } else if (type === 'element') {
            return getElementPLPropertyValue(valueJS);
        } else {
            domain_error(type);
        }
    } else {
        domain_error(property);
    }
}

function getAtomPLPropertyValue(valueJS) {
    return lookup_atom(valueJS);
}

function getNumberPLPropertyValue(valueJS) {
    return valueJS ^ (TAG_INT << WORD_BITS);
}

function getStringPLPropertyValue(valueJS) {
    return string_to_codes(valueJS);
}

function getElementPLPropertyValue(valueJS) {
    return lookup_element(valueJS);
}

function setupElementsForBoundPropertyValue(propertyJS, value) {
    var propertySpec = propertyMap.get(propertyJS);
    if(propertySpec) {
        var valueJS = propertyValueToJS(propertySpec.type, value);
        return propertySpec.elements(valueJS);
    } else {
        domain_error(property);
    }
}

function setupPropertyValuesFromBoundValue(property, propertyJS, value) {
    var propertySpec = propertyMap.get(propertyJS);
    if(propertySpec) {
        var values = [];
        var valueJS = propertyValueToJS(propertySpec.type, value);
        values.push(valueJS);
        return values;
    } else {
        domain_error(property);
    }
}

function setupPropertyValuesFromJSElement1(elementJS, property, propertyJS) {
    var propertySpec = propertyMap.get(propertyJS);
    if(propertySpec) {
        return propertySpec.elementValuesFunction(elementJS);
    } else {
        domain_error(property);
    }
}

function predicate_set_dom_element_property(element, property, value) {
    if (TAG(element) !== TAG_STR) {
        instantiation_error(element);
    }

    var elementObject = {};
    if (!get_element_object(element, elementObject)) {
        return false;
    }
    var elementJS = elementObject.value;

    if (TAG(property) === TAG_REF) {
        instantiation_error(property);
    } else {
        var propertyJS = atable[VAL(property)];
    }

    var propertySpec = propertyMap.get(propertyJS);
    if(propertySpec) {
        propertySpec.setValue(property, elementJS, value);
    } else {
        domain_error(property);
    }
}

function TagProperty() {
    var that = {};
    that.name = "tag";
    that.type = 'atom';
    that.elements = function(valueJS) {
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

function InnerHTMLProperty() {
    var that = {};
    that.name = "inner";
    that.type = 'string'; // list of integer character codes
    that.elements = function(valueJS) {
        return Array.from(document.querySelectorAll('*')); // return all elements for later filtering by unification
    };
    that.elementValuesFunction = function(elementJS) {
        var values = [];
        values.push(elementJS.innerHTML);
        return values;
    };
    that.setValue = function(property, elementJS, value) {
        /** @namespace elementJS.innerHTML */
        elementJS.innerHTML = codes_to_string(value);
    };
    return that;
}

function ChildProperty() {
    var that = {};
    that.name = "child";
    that.type = 'element';
    that.elements = function(valueJS) {
        var elements = [];
        elements.push(valueJS.parentElement);
        return elements;
    };
    that.elementValuesFunction = function(elementJS) {
        /** @namespace elementJS.children */
        return elementJS.children;
    };
    that.setValue = function(property, elementJS, value) {
        domain_error(property);
    };
    return that;
}

function SimpleProperty(type, propertyName) {
    var that = {};
    that.name = propertyName;
    that.type = type;
    that.elements = function(valueJS) {
        return Array.from(document.querySelectorAll('*')); // return all elements for later filtering by unification
    };
    that.elementValuesFunction = function(elementJS) {
        var values = [];
        values.push(elementJS[propertyName]);
        return values;
    };
    that.setValue = function(property, elementJS, value) {
        domain_error(property);
    };
    return that;
}

var propertyMap = new Map([
    ['tag', TagProperty()],
    ['inner', InnerHTMLProperty()],
    ['child', ChildProperty()],
    ['childElementCount', SimpleProperty('number','childElementCount')],
    ['clientHeight', SimpleProperty('number','clientHeight')]
]);
