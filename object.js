var idsToObjects = new Map();
var idsToTypes = new Map();
var objectsToIDs = new Map();
var goalFunctions = new Map();

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
    ['node', ['eventtarget']],
    ['document', ['node']],
    ['element', ['node']],
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
    ['htmloptionelement', ['htmlelement']],
    ['path2d', []],
    ['uievent', ['event']],
    ['mouseevent', ['uievent']],
    ['textmetrics', []]
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
    "HTMLOptionElement" : 'htmloptionelement',
    "Path2D" : 'path2d',
    "UIEvent" : 'uievent',
    "MouseEvent" : 'mouseevent',
    "TextMetrics" : 'textmetrics'
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
    let argList = deref(memory[VAL(specTermTailPL)]);
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
    result.arguments = argTypesJS;

    let specTermTailTailPL = deref(memory[VAL(specTermTailPL) + 1]);
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

function predicate_dom_create_object(type, object) {
    if(TAG(type) !== TAG_ATM) {
        return type_error('atom', type);
    }
    let typeJS = atable[VAL(type)];

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

    if(constructorName) {
        let objectJS = new window[constructorName]();

        let objectPL = create_object_structure(objectJS, typeJS);
        return unify(object, objectPL);
    } else {
        return domain_error('object type', type);
    }
}

function predicate_dom_release_object(object) {
    if (TAG(object) === TAG_REF) {
        return instantiation_error(object);
    }

    return release_object(object);
}