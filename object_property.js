
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

        debug_msg("Is retry! Setting cursor back to " + cursor);
    }
    else {
        cursor = {
            objects: setupObjectsForPropertyValue(type, object, property, value),
            property_values: setupPropertyValues(type, object, property, value)
        };
        cursorIDJS = 'crs' + dopCursorCounter++;
        dopCursors.set(cursorIDJS, cursor);
        cursorIDPL = lookup_atom(cursorIDJS);

        debug_msg("Not a retry");
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
            var valuePL = propertyValueToPL(typeJS, property, valueJS);
            return unify(value, valuePL) &&
                unify(object, elementPL);
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


function setupObjectsForPropertyValue(type, object, property, value) {
    if (TAG(property) === TAG_REF) {
        instantiation_error(property);
    } else {
        var propertyJS = atable[VAL(property)];
    }

    if (TAG(object) !== TAG_REF) {
        if (TAG(object) !== TAG_STR) {
            instantiation_error(object);
        }
        var objectContainer = {};
        if (!get_object_container(object, objectContainer)) {
            return undefined;
        }
        var objectContainers = [];
        objectContainers.push(objectContainer);
        return objectContainers;
    } else if (TAG(value) !== TAG_REF && TAG(type) === TAG_ATM) {
        return setupObjectsForBoundPropertyValue(atable[VAL(type)], propertyJS, value);
    } else if (TAG(type) === TAG_ATM && atable[VAL(type)] === 'element') {
        return objectsToContainers(document.querySelectorAll('*'), 'element');
    } else {
        return undefined;
    }
}

function objectsToContainers(objects, typeJS) {
    let objectContainers = [];

    for(let object of objects) {
        objectContainers.push({value:object, type:'element'});
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
    if (TAG(property) === TAG_REF) {
        instantiation_error(property);
    } else {
        var propertyJS = atable[VAL(property)];
    }
    var values;
    if (TAG(value) !== TAG_REF && TAG(type) === TAG_ATM) {
        values = setupPropertyValuesFromBoundValue(atable[VAL(type)], property, propertyJS, value);
    } else if (TAG(object) !== TAG_REF) {
        if (TAG(object) !== TAG_STR) {
            instantiation_error(object);
        }
        var objectContainer = {};
        if (!get_object_container(object, objectContainer)) {
            return undefined;
        }
        let objectJS = objectContainer.value;
        let typeJS = objectContainer.type;
        if(type === TAG_ATM && atable[VAL(type)] !== typeJS) {
            throw 'incompatible object types. Param type is ' + atable[VAL(type)] + ' and object ' + JSON.stringify(objectJS) + ' is tagged with type ' + typeJS;
        }
        values = setupPropertyValuesFromJSElement1(typeJS, objectJS, property, propertyJS);
    } else {
        values = undefined;
    }
    return values;
}

function propertyValueToJS(type, value) {
    if(type === 'atom') {
        return getAtomPropertyValue(value);
    } else if(type === 'boolean') {
        return getBooleanPropertyValue(value);
    } else if(type === 'number') {
        return getNumberPropertyValue(value);
    } else if(type === 'string') {
        return getStringPropertyValue(value);
    } else if(type === 'object') {
        return getObjectPropertyValue(value);
    } else {
        domain_error(type);
    }
}

function getAtomPropertyValue(value) {
    return atable[VAL(value)];
}

function getBooleanPropertyValue(value) {
    var valueJS = atable[VAL(value)];
    return valueJS === 'true';
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

        var atomPL = memory[VAL(list)];
        if(TAG(atomPL) !== TAG_ATM) {
            instantiation_error(atomPL);
        } else {
            if(string !== '') {
                string += ' ';
            }
            string += atable[VAL(value)];
            list = memory[VAL(list) + 1];
        }
    }

    return string;
}

function getNumberPropertyValue(value) {
    let result;
    if ((VAL(value) & (1 << (WORD_BITS-1))) === (1 << (WORD_BITS-1)))
        result = VAL(value) - (1 << WORD_BITS);
    else
        result = VAL(value);
    return result;
}

function getStringPropertyValue(value) {
    return codes_to_string(value);
}

function getObjectPropertyValue(value) {
    var valueObjectContainer = {};
    if (!get_object_container(value, valueObjectContainer)) {
        return undefined;
    }
    return valueObjectContainer.value;
}

function propertyValueToPL(typeJS, property, valueJS) {
    let propertyJS = PL_get_atom_chars(property);
    let propertySpec = getPropertySpecification(typeJS, propertyJS);

    if (propertySpec) {
        if (propertySpec.type === 'atom') {
            return getAtomPLPropertyValue(valueJS);
        } else if (propertySpec.type === 'number') {
            return getNumberPLPropertyValue(valueJS);
        } else if (propertySpec.type === 'string') {
            return getStringPLPropertyValue(valueJS);
        } else if (propertySpec.type === 'object') {
            return getObjectPLPropertyValue(valueJS);
        } else {
            domain_error(propertySpec.type);
        }
    } else {
        domain_error(property);
    }
}

function getAtomPLPropertyValue(valueJS) {
    return lookup_atom(valueJS);
}

function getNumberPLPropertyValue(valueJS) {
    return (valueJS  & ((1 << WORD_BITS)-1)) ^ (TAG_INT << WORD_BITS); // or ((1 << (WORD_BITS-1))-1)  from predicate_get_code (and others) in stream.js?
}

function getStringPLPropertyValue(valueJS) {
    return string_to_codes(valueJS);
}

function getObjectPLPropertyValue(valueJS) {
    return create_object_structure(valueJS);
}

function setupObjectsForBoundPropertyValue(typeJS, propertyJS, value) {
    let propertySpec = getPropertySpecification(typeJS, propertyJS);
    if (propertySpec) {
        var valueJS = propertyValueToJS(propertySpec.type, value);
        return objectsToContainers(propertySpec.objects(valueJS), typeJS);
    } else {
        domain_error(propertyJS);
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
    return getInterfaceItemSpec(typeJS, 'property', propertyJS);
}

function predicate_set_dom_object_property(object, property, value) {
    if (TAG(object) !== TAG_STR) {
        instantiation_error(object);
    }

    let objectContainer = {};
    if (!get_object_container(object, objectContainer)) {
        return false;
    }
    let elementJS = objectContainer.value;
    let typeJS = objectContainer.type;

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
