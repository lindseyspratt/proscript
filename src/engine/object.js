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
    ['window', ['eventtarget', 'windowlocalstorage', 'windowsessionstorage']],
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

        debug_msg("Is retry! Setting cursor back to " + cursor);
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

        debug_msg("Not a retry");
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

        debug_msg("Is retry! Setting cursor back to " + cursor);
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

        debug_msg("Not a retry");
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

        debug_msg("Is retry! Setting cursor back to " + cursor);
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

        debug_msg("Not a retry");
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

        debug_msg("Is retry! Setting cursor back to " + cursor);
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

        debug_msg("Not a retry");
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
