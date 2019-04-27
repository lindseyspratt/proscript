
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
    return VAL(value);
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
    return valueJS ^ (TAG_INT << WORD_BITS);
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
    let stack = [typeJS];
    while (stack.length > 0) {
        let testType = stack.shift(0);
        let webInterface = webInterfaces.get(testType);
        if (webInterface) {
            if (webInterface.properties) {
                let result = webInterface.properties.get(propertyJS);
                if (result) {
                    return result;
                }
            }

            // propertyJS not found. put interface.parents on the
            // bottom of the stack (for breadth-first search of
            // parents)

            let parents = webInterface.parent;
            if (parents) {
                for (let ofst = 0; ofst < parents.length; ofst++) {
                    stack.push(parents[ofst]);
                }
            }
        }
    }
    return undefined;
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
function TagPropertyX() {
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

function ChildNodePropertyX() {
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
    that.setValue = function(property, elementJS, value) {
        domain_error(property);
    };
    return that;
}

function SimpleChildNodePropertyX(propertyName) {
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
        /** @namespace elementJS.firstChild */
        objects.push(elementJS[propertyName]);
        return objects;
    };
    that.setValue = function(property, elementJS, value) {
        domain_error(property);
    };
    return that;
}

function ChildPropertyX() {
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
    that.setValue = function(property, elementJS, value) {
        domain_error(property);
    };
    return that;
}

function SimpleChildPropertyX(propertyName) {
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
        /** @namespace elementJS.firstChild */
        objects.push(elementJS[propertyName]);
        return objects;
    };
    that.setValue = function(property, elementJS, value) {
        domain_error(property);
    };
    return that;
}

/**
 * This prolog 'class' property uses the classList and className HTML Element properties.
 */

function ClassPropertyX() {
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
            valueJS = getAtomPropertyValue(value);
        } else if (TAG(element) === TAG_LST) {
            valueJS = getClassListPropertyValue(value);
        }
        elementJS.className = valueJS;
    };
    return that;
}

function SimplePropertyX(type, propertyName, settable) {
    var that = {};
    that.name = propertyName;
    that.type = type;
    that.objects = function(valueJS) {
        return Array.from(document.querySelectorAll('*')); // return all objects for later filtering by unification
    };
    that.elementValuesFunction = function(elementJS) {
        var values = [];
        values.push(elementJS[propertyName]);
        return values;
    };
    that.setValue = function(property, elementJS, value) {
        if(settable) {
            elementJS[propertyName] = propertyValueToJS(type, value);
        } else {
            domain_error(property);
        }
    };
    return that;
}

var webInterfaces = new Map();

webInterfaces.set('eventtarget', {name: 'eventtarget'});

var nodeInterfaceProperties = new Map([
    // baseURI
    // baseURIObject
    ['childNode', ChildNodePropertyX()], // adapted from childNodes
    ['firstChild', SimpleChildNodePropertyX('firstChild')],
    // isConnected
    ['lastChild', SimpleChildPropertyX('lastChild')],
    //['namespaceURI', SimplePropertyX('string', 'namespaceURI')], // deprecated in Node interface: moved to Element.
    ['nextSibling', SimpleChildPropertyX('nextSibling')],
    ['nodeName', SimplePropertyX('atom','nodeName')],
    ['nodeType', SimplePropertyX('atom','nodeType')],
    ['nodeValue', SimplePropertyX('string','nodeValue', true)],
    // outerText
    ['ownerDocument', SimpleChildPropertyX('ownerDocument')],
    ['parentElement', SimpleChildPropertyX('parentElement')],
    ['parentNode', SimpleChildPropertyX('parentNode')],
    ['previousSibling', SimpleChildPropertyX('previousSibling')],
    ['textContent', SimplePropertyX('string', 'textContent', true)]
]);

webInterfaces.set('node', {name: 'node', parent: ['eventtarget'], properties:nodeInterfaceProperties});

var elementInterfaceProperties = new Map([
    ['accesskey', SimplePropertyX('atom','accesskey', true)],
    // attributes
    ['child', ChildPropertyX()], // adapted from children
    ['childElementCount', SimplePropertyX('number','childElementCount')],
    ['class', ClassPropertyX()], // adapted from classList, className
    ['clientHeight', SimplePropertyX('number','clientHeight')],
    ['clientLeft', SimplePropertyX('number','clientLeft')],
    ['clientTop', SimplePropertyX('number','clientTop')],
    ['clientWidth', SimplePropertyX('number','clientWidth')],
    // currentStyle
    ['firstElementChild', SimpleChildPropertyX('firstElementChild')],
    ['id', SimplePropertyX('atom','id', true)],
    ['innerHTML', SimplePropertyX('string', 'innerHTML', true)],
    ['lang', SimplePropertyX('atom','lang', true)], // ISO 639-1 Language Codes: en, de, ja, ...
    ['lastChild', SimpleChildPropertyX('lastChild')],
    ['lastElementChild', SimpleChildPropertyX('lastElementChild')],
    // name
    ['namespaceURI', SimplePropertyX('string', 'namespaceURI')],
    ['nextElementSibling', SimpleChildPropertyX('nextElementSibling')],
    ['previousElementSibling', SimpleChildPropertyX('previousElementSibling')],
    // runtimeStyle
    ['scrollHeight', SimplePropertyX('number','scrollHeight')],
    ['scrollLeft', SimplePropertyX('number','scrollLeft')],
    // scrollLeftMax
    ['scrollTop', SimplePropertyX('number','scrollTop')],
    // scrollTopMax
    ['scrollWidth', SimplePropertyX('number','scrollWidth')],
    ['tag', TagPropertyX()] // for tagName
]);

webInterfaces.set('element', {name: 'element', parent: ['node'], properties:elementInterfaceProperties});

var htmlElementInterfaceProperties = new Map([
    ['contentEditable', SimplePropertyX('boolean','contentEditable', true)],
    // contextMenu, deprecated
    // dataset
    ['dir', SimplePropertyX('atom','dir', true)], // rtl, ltr, auto
    // hidden
    ['innerText', SimplePropertyX('string', 'innerText', true)],
    ['lang', SimplePropertyX('atom','lang', true)], // ISO 639-1 Language Codes: en, de, ja, ...
    // nonce, experimental
    ['offsetHeight', SimplePropertyX('number','offsetHeight')],
    ['offsetLeft', SimplePropertyX('number','offsetLeft')],
    ['offsetParent', SimplePropertyX('number','offsetParent')],
    ['offsetTop', SimplePropertyX('number','offsetTop')],
    ['offsetWidth', SimplePropertyX('number','offsetWidth')],
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
    ['style', SimplePropertyX('string', 'style', true)],
    ['tabIndex', SimplePropertyX('number','tabIndex')],
    ['title', SimplePropertyX('string', 'title', true)]
]);
webInterfaces.set('htmlelement', {name: 'htmlelement', parent: ['element'], properties:htmlElementInterfaceProperties});

