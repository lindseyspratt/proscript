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

function TagProperty() {
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

function ChildNodeProperty() {
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

function SimpleChildNodeProperty(propertyName) {
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
        let propertyValue = elementJS[propertyName];
        if(typeof propertyValue !== 'undefined' && propertyValue !== null) {
            objects.push(propertyValue);
        }
        return objects;
    };
    that.setValue = function(property, elementJS, value) {
        domain_error(property);
    };
    return that;
}

function ChildProperty() {
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

function SimpleChildProperty(propertyName) {
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
        let value = elementJS[propertyName];
        if(typeof value !== 'undefined' && value !== null) {
            objects.push(value);
        }
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

function ClassProperty() {
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
            let container = {};
            if(getAtomPropertyValue(value, container)) {
                valueJS = container.value;
            } else {
                // leave valueJS undefined.
            }
        } else if (TAG(element) === TAG_LST) {
            valueJS = getClassListPropertyValue(value);
        }
        elementJS.className = valueJS;
    };
    return that;
}

function SimpleProperty(type, propertyName, settable) {
    var that = {};
    that.name = propertyName;
    that.type = type;
    that.objects = function(valueJS) {
        return Array.from(document.querySelectorAll('*')); // return all objects for later filtering by unification
    };
    that.elementValuesFunction = function(elementJS) {
        var values = [];
        let value = elementJS[propertyName];
        if(typeof value !== 'undefined' && value !== null) {
            values.push(value);
        }
        return values;
    };
    that.setValue = function(property, elementJS, value) {
        if(settable) {
            let container = {};
            if(propertyValueToJS(type, value, container, true)) {
                elementJS[propertyName] = container.value;
                return true;
            } else {
                return false;
            }
        } else {
            return domain_error(property);
        }
    };
    return that;
}

var webInterfaces = new Map();

var eventTargetMethodSpecs = new Map([
    ['addEventListener',{name:'addEventListener',arguments:[{type:'string'},{type:'goal_function'}]}],
    ['removeEventListener',{name:'removeEventListener',arguments:[{type:'string'},{type:'goal_function'}]}],
    ['dispatchEvent',{name:'dispatchEvent',arguments:[{type:'event'}],returns:{type:'boolean'}}]
]);

webInterfaces.set('eventtarget',
    {
        name: 'eventtarget',
        methods: eventTargetMethodSpecs
    });

var nodeInterfaceProperties = new Map([
    // baseURI
    // baseURIObject
    ['childNode', ChildNodeProperty()], // adapted from childNodes
    ['firstChild', SimpleChildNodeProperty('firstChild')],
    // isConnected
    ['lastChild', SimpleChildProperty('lastChild')],
    //['namespaceURI', SimpleProperty('string', 'namespaceURI')], // deprecated in Node interface: moved to Element.
    ['nextSibling', SimpleChildProperty('nextSibling')],
    ['nodeName', SimpleProperty('atom','nodeName')],
    ['nodeType', SimpleProperty('number','nodeType')],
    ['nodeValue', SimpleProperty('string','nodeValue', true)],
    // outerText
    ['ownerDocument', SimpleChildProperty('ownerDocument')],
    ['parentElement', SimpleChildProperty('parentElement')],
    ['parentNode', SimpleChildProperty('parentNode')],
    ['previousSibling', SimpleChildProperty('previousSibling')],
    ['textContent', SimpleProperty('string', 'textContent', true)]
]);

var nodeMethodSpecs = new Map([
    ['cloneNode',{name:'cloneNode',arguments:[{type:'boolean'}],returns:{type:'object'}}],
    ['compareDocumentPosition',{name:'compareDocumentPosition',arguments:[{type:'object'}],returns:{type:'number'}}],
    ['contains',{name:'contains',arguments:[{type:'object'}],returns:{type:'boolean'}}],
    ['isEqualNode',{name:'isEqualNode',arguments:[{type:'object'}],returns:{type:'boolean'}}],
    ['normalize',{name:'normalize',arguments:[]}],
    ['removeChild',{name:'removeChild',arguments:[{type:'object'}]}]
]);

webInterfaces.set('node',
    {
        name: 'node',
        parent: ['eventtarget'],
        properties:nodeInterfaceProperties,
        methods:nodeMethodSpecs
    });

var elementInterfaceProperties = new Map([
    ['accessKey', SimpleProperty('atom','accessKey', true)],
    // attributes
    ['child', ChildProperty()], // adapted from children
    ['childElementCount', SimpleProperty('number','childElementCount')],
    ['class', ClassProperty()], // adapted from classList, className
    ['clientHeight', SimpleProperty('number','clientHeight')],
    ['clientLeft', SimpleProperty('number','clientLeft')],
    ['clientTop', SimpleProperty('number','clientTop')],
    ['clientWidth', SimpleProperty('number','clientWidth')],
    // currentStyle
    ['firstElementChild', SimpleChildProperty('firstElementChild')],
    ['id', SimpleProperty('atom','id', true)],
    ['innerHTML', SimpleProperty('string', 'innerHTML', true)],
    ['lastElementChild', SimpleChildProperty('lastElementChild')],
    // name
    ['namespaceURI', SimpleProperty('string', 'namespaceURI')],
    ['nextElementSibling', SimpleChildProperty('nextElementSibling')],
    ['previousElementSibling', SimpleChildProperty('previousElementSibling')],
    // runtimeStyle
    ['scrollHeight', SimpleProperty('number','scrollHeight')],
    ['scrollLeft', SimpleProperty('number','scrollLeft')],
    // scrollLeftMax
    ['scrollTop', SimpleProperty('number','scrollTop')],
    // scrollTopMax
    ['scrollWidth', SimpleProperty('number','scrollWidth')],
    ['tag', TagProperty()] // for tagName
]);

var elementMethodSpecs = new Map([
    ['getBoundingClientRect',{name:'getBoundingClientRect',arguments:[],returns:{type:'dom_rect'}}],
    ['insertAdjacentElement',{name:'insertAdjacentElement',arguments:[{type:'position'},{type:'object'}]}],
    ['insertAdjacentHTML',{name:'insertAdjacentHTML',arguments:[{type:'position'},{type:'string_codes'}]}],
    ['insertAdjacentText',{name:'insertAdjacentText',arguments:[{type:'position'},{type:'string_codes'}]}],
    ['scrollIntoView',{name:'scrollIntoView',arguments:[{type:['boolean', 'options']}]}]
]);

webInterfaces.set('element',
    {
        name: 'element',
        parent: ['node'],
        properties:elementInterfaceProperties,
        methods: elementMethodSpecs
    });

var htmlElementInterfaceProperties = new Map([
    ['contentEditable', SimpleProperty('atom','contentEditable', true)],
    // contextMenu, deprecated
    // dataset
    ['dir', SimpleProperty('atom','dir', true)], // rtl, ltr, auto
    // hidden
    ['innerText', SimpleProperty('string', 'innerText', true)],
    ['lang', SimpleProperty('atom','lang', true)], // ISO 639-1 Language Codes: en, de, ja, ...
    // nonce, experimental
    ['offsetHeight', SimpleProperty('number','offsetHeight')],
    ['offsetLeft', SimpleProperty('number','offsetLeft')],
    ['offsetParent', SimpleProperty('object','offsetParent')],
    ['offsetTop', SimpleProperty('number','offsetTop')],
    ['offsetWidth', SimpleProperty('number','offsetWidth')],
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
    ['style', SimpleProperty('object', 'style', true)], // object has a CSSStyleDeclaration interface.
    ['tabIndex', SimpleProperty('number','tabIndex')],
    ['title', SimpleProperty('string', 'title', true)]
]);

var htmlElementMethodSpecs = new Map([
    ['blur',{name:'blur',arguments:[]}],
    ['click',{name:'click',arguments:[]}],
    ['focus',{name:'focus',arguments:[]}]
]);

webInterfaces.set('htmlelement',
    {name: 'htmlelement',
        parent: ['element'],
        properties:htmlElementInterfaceProperties,
        methods:htmlElementMethodSpecs
    });

var htmlCanvasElementInterfaceProperties = new Map( [
    ['height', SimpleProperty('number', 'height', true)],
    ['width', SimpleProperty('number', 'width', true)]
]);

var htmlCanvasElementMethodSpecs = new Map([
    ['getContext',{name:'getContext',arguments:[{type:'string'}],returns:{type:'object'}}], // arg is '2d' or 'webgl'. return is CanvasRenderingContext2D or WebGLRenderingContext
    ['toBlob',{name:'toBlob',arguments:[{type:'goal_function'},{type:'string'},{type:'float'}]}],
    ['toDataURL',{name:'toDataURL',arguments:[{type:'string'},{type:'float'}],returns:{type:'string_codes'}}], // 2nd arg is between 0 and 1. Result is a data URL.
    ['removeProperty',{name:'removeProperty',arguments:[{type:'string'}],returns:{type:'atom'}}],
    ['setProperty',{name:'setProperty',arguments:[{type:'string'},{type:'string'},{type:'atom'}]}]
]);

webInterfaces.set('htmlcanvaselement',
    {name: 'htmlcanvaselement',
        properties:htmlCanvasElementInterfaceProperties,
        methods:htmlCanvasElementMethodSpecs
    });

var eventInterfaceProperties = new Map( [
    ['bubbles', SimpleProperty('boolean', 'bubbles')],
    ['cancelable', SimpleProperty('boolean', 'cancelable')],
    ['cancelBubble', SimpleProperty('boolean', 'cancelBubble', true)],
    ['composed', SimpleProperty('boolean', 'composed')],
    ['currentTarget', SimpleProperty('object', 'currentTarget')],
    ['defaultPrevented', SimpleProperty('boolean', 'defaultPrevented')],
    ['eventPhase', SimpleProperty('number', 'eventPhase')],
    ['returnValue', SimpleProperty('boolean', 'returnValue', true)],
    ['target', SimpleProperty('object', 'target')],
    ['timeStamp', SimpleProperty('number', 'timeStamp')],
    ['type', SimpleProperty('string', 'type')],
    ['isTrusted', SimpleProperty('boolean', 'isTrusted')]
]);

var eventMethodSpecs = new Map([
    ['composedPath',{name:'composedPath',arguments:[],returns:{type:'object'}}],
    ['preventDefault',{name:'preventDefault',arguments:[]}],
    ['stopImmediatePropagation',{name:'stopImmediatePropagation',arguments:[]}],
    ['stopPropagation',{name:'stopPropagation',arguments:[]}],
    ['setProperty',{name:'setProperty',arguments:[{type:'string'},{type:'string'},{type:'atom'}]}]
]);

webInterfaces.set('event',
    {name: 'event',
        properties:eventInterfaceProperties,
        methods:eventMethodSpecs
    });

var cssStyleDeclarationInterfaceProperties = new Map( [
    ['cssText', SimpleProperty('string', 'cssText', true)], // documented as Attribute, but not listed as Property.
    ['length', SimpleProperty('number', 'length')],
    ['parentRule', SimpleProperty('object', 'parentRule')] // object has a CSSRule interface.
]);


var cssStyleDeclarationMethodSpecs = new Map([
    ['getPropertyPriority',{name:'getPropertyPriority',arguments:[{type:'string'}],returns:{type:'atom'}}],
    ['getPropertyValue',{name:'getPropertyValue',arguments:[{type:'string'}],returns:{type:'atom'}}],
    ['item',{name:'item',arguments:[{type:'integer'}],returns:{type:'atom'}}],
    ['removeProperty',{name:'removeProperty',arguments:[{type:'string'}],returns:{type:'atom'}}],
    ['setProperty',{name:'setProperty',arguments:[{type:'string'},{type:'string'},{type:'atom'}]}]
]);

webInterfaces.set('cssstyledeclaration',
    {name: 'cssstyledeclaration',
        properties:cssStyleDeclarationInterfaceProperties,
        methods:cssStyleDeclarationMethodSpecs
    });

var cssRuleInterfaceProperties = new Map( [
    ['cssText', SimpleProperty('string', 'cssText', true)],
    ['parentStyleSheet', SimpleProperty('object', 'parentStyleSheet')],
    ['parentRule', SimpleProperty('object', 'parentRule')], // documented as Attribute, but not listed as Property.
    ['type', SimpleProperty('number', 'type')], // documented as Attribute, but not listed as Property.
]);

var cssRuleMethodSpecs = new Map([]);

webInterfaces.set('cssrule',
    {name: 'cssrule',
        properties:cssRuleInterfaceProperties,
        methods:cssRuleMethodSpecs
    });

var canvasRenderingContext2DInterfaceProperties = new Map( [
    ['canvas', SimpleProperty('object', 'canvas')],
    ['fillStyle', SimpleProperty(['atom','object'], 'fillStyle', true)], // fillStyle is a string naming a color, a CanvasGradient object, or a CanvasPattern object.
    ['font', SimpleProperty('atom', 'font', true)],
    ['globalAlpha', SimpleProperty('number', 'globalAlpha', true)],
    ['globalCompositeOperation', SimpleProperty('atom', 'globalCompositeOperation', true)],
    ['imageSmoothingEnabled', SimpleProperty('atom', 'imageSmoothingEnabled', true)],
    ['lineCap', SimpleProperty('atom', 'lineCap', true)],
    ['lineDashOffset', SimpleProperty('number', 'lineDashOffset', true)],
    ['lineJoin', SimpleProperty('atom', 'lineJoin', true)],
    ['lineWidth', SimpleProperty('number', 'lineWidth', true)],
    ['miterLimit', SimpleProperty('number', 'miterLimit', true)],
    ['shadowBlur', SimpleProperty('number', 'shadowBlur', true)],
    ['shadowColor', SimpleProperty('atom', 'shadowColor', true)],
    ['shadowOffsetX', SimpleProperty('number', 'shadowOffsetX', true)],
    ['shadowOffsetY', SimpleProperty('number', 'shadowOffsetY', true)],
    ['strokeStyle', SimpleProperty(['atom','object'], 'strokeStyle', true)], // strokeStyle is a string naming a color, a CanvasGradient object, or a CanvasPattern object.
    ['textAlign', SimpleProperty('atom', 'textAlign', true)],
    ['textBaseline', SimpleProperty('atom', 'textBaseline', true)]
]);

var canvasRenderingContext2DMethodSpecs = new Map([
    ['arc',{name:'arc',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'boolean'}]}],
    ['arcTo',{name:'arcTo',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['beginPath',{name:'beginPath',arguments:[]}],
    ['bezierCurveTo',{name:'bezierCurveTo',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['clearRect',{name:'clearRect',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['clip',{name:'clip',arguments:[{type:'string'}]}],
    ['clipPath',{name:'clip',arguments:[{type:'object'},{type:'string'}]}], // object is Path2D
    ['closePath',{name:'closePath',arguments:[]}],
    ['createImageData',{name:'createImageData',arguments:[{type:'number'},{type:'number'}],returns:{type:'object'}}],
    ['createImageDataCopy',{name:'createImageData',arguments:[{type:'object'}],returns:{type:'object'}}], // object is ImageData
    ['createLinearGradient',{name:'createLinearGradient',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'}],returns:{type:'object'}}],
    ['createPattern',{name:'createPattern',arguments:[{type:'object'},{type:'string'}],returns:{type:'object'}}], // input object is CanvasImageSource, returns CanvasPattern
    ['createRadialGradient',{name:'createRadialGradient',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'}],returns:{type:'object'}}], // returns CanvasGradient
    ['drawFocusIfNeeded',{name:'drawFocusIfNeeded',arguments:[{type:'object'}]}], // object is Element
    ['drawFocusIfNeededPath',{name:'drawFocusIfNeeded',arguments:[{type:'object'},{type:'object'}]}], // object is Path2D
    ['drawImage',{name:'drawImage',arguments:[{type:'object'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['ellipse',{name:'ellipse',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'boolean'}]}],
    ['fill',{name:'fill',arguments:[{type:'string'}]}],
    ['fillPath',{name:'fill',arguments:[{type:'object'},{type:'string'}]}], // object is Path2D
    ['fillRect',{name:'fillRect',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['fillText',{name:'fillText',arguments:[{type:'string'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['getImageData',{name:'getImageData',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'}],returns:{type:'object'}}],
    ['getLineDash',{name:'getLineDash',arguments:[],returns:{type:{arrayType:'number'}}}],
    ['isPointInPath',{name:'isPointInPath',arguments:[{type:'number'},{type:'number'},{type:'string'}],returns:{type:'boolean'}}],
    ['isPointInPathX',{name:'isPointInPath',arguments:[{type:'object'},{type:'number'},{type:'number'},{type:'string'}],returns:{type:'boolean'}}],
    ['isPointInStroke',{name:'isPointInStroke',arguments:[{type:'number'},{type:'number'}],returns:{type:'boolean'}}],
    ['isPointInStrokePath',{name:'isPointInStroke',arguments:[{type:'object'},{type:'number'},{type:'number'}],returns:{type:'boolean'}}],
    ['lineTo',{name:'lineTo',arguments:[{type:'number'},{type:'number'}]}],
    ['measureText',{name:'measureText',arguments:[{type:'string_codes'}],returns:{type:'object'}}], // returns TextMetrics
    ['moveTo',{name:'moveTo',arguments:[{type:'number'},{type:'number'}]}],
    ['putImageData',{name:'putImageData',arguments:[{type:'object'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['quadraticCurveTo',{name:'quadraticCurveTo',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['rect',{name:'rect',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['restore',{name:'restore',arguments:[]}],
    ['rotate',{name:'rotate',arguments:[{type:'number'}]}],
    ['save',{name:'save',arguments:[]}],
    ['scale',{name:'scale',arguments:[{type:'number'},{type:'number'}]}],
    ['setLineDash',{name:'setLineDash',arguments:[{type:{arrayType:'number'}}]}],
    ['setTransform',{name:'setTransform',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['stroke',{name:'stroke',arguments:[{type:'object'}]}],
    ['strokeRect',{name:'strokeRect',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['strokeText',{name:'strokeText',arguments:[{type:'string'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['transform',{name:'transform',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'}]}],
    ['translate',{name:'translate',arguments:[{type:'number'},{type:'number'}]}]
]);

webInterfaces.set('canvasrenderingcontext2d',
    {name: 'canvasrenderingcontext2d',
        properties:canvasRenderingContext2DInterfaceProperties,
        methods:canvasRenderingContext2DMethodSpecs
    });

var blobInterfaceProperties = new Map( [
    ['size', SimpleProperty('string', 'size', true)],
    ['type', SimpleProperty('string', 'type')],
]);

var blobMethodSpecs = new Map([
    ['slice',{name:'slice',arguments:[{type:'number'},{type:'number'},{type:'string'}],returns:{type:'object'}}]
]);

webInterfaces.set('blob',
    {name: 'blob',
        properties:blobInterfaceProperties,
        methods:blobMethodSpecs
    });

var imageDataInterfaceProperties = new Map( [
    ['data', SimpleProperty('object', 'data')]
]);

var imageDataMethodSpecs = new Map([
]);

webInterfaces.set('imagedata',
    {name: 'imagedata',
        properties:imageDataInterfaceProperties,
        methods:imageDataMethodSpecs
    });

var uint8ClampedArrayInterfaceProperties = new Map( [
    ['length', SimpleProperty('integer', 'length')]
]);

var uint8ClampedArrayMethodSpecs = new Map([
    ['set', {name:'set',arguments:[{type:{arrayType:'integer'}},{type:'integer'}]}]
]);

webInterfaces.set('uint8clampedarray',
    {name: 'uint8clampedarray',
        properties:uint8ClampedArrayInterfaceProperties,
        methods:uint8ClampedArrayMethodSpecs
    });

var canvasGradientInterfaceProperties = new Map( [
]);

var canvasGradientMethodSpecs = new Map([
    ['addColorStop', {name:'addColorStop',arguments:[{type:'number'},{type:'string'}]}]
]);

webInterfaces.set('canvasgradient',
    {name: 'canvasgradient',
        properties:canvasGradientInterfaceProperties,
        methods:canvasGradientMethodSpecs
    });

var canvasPatternInterfaceProperties = new Map( [
]);

var canvasPatternMethodSpecs = new Map([
]);

webInterfaces.set('canvaspattern',
    {name: 'canvaspattern',
        properties:canvasPatternInterfaceProperties,
        methods:canvasPatternMethodSpecs
    });

var htmlImageElementInterfaceProperties = new Map( [
    ['src', SimpleProperty('string', 'src', true)]
]);

var htmlImageElementMethodSpecs = new Map([
]);

webInterfaces.set('htmlimageelement',
    {name: 'htmlimageelement',
        properties:htmlImageElementInterfaceProperties,
        methods:htmlImageElementMethodSpecs
    });

var path2DInterfaceProperties = new Map( [
]);

var path2DMethodSpecs = new Map([
    ['arc',{name:'arc',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'boolean'}]}],
    ['ellipse',{name:'ellipse',arguments:[{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'number'},{type:'boolean'}]}]
]);

webInterfaces.set('path2d',
    {name: 'path2d',
        properties:path2DInterfaceProperties,
        methods:path2DMethodSpecs
    });

var uiEventInterfaceProperties = new Map( [
]);

var uiEventMethodSpecs = new Map([
]);

webInterfaces.set('uievent',
    {name: 'uievent',
        properties:uiEventInterfaceProperties,
        methods:uiEventMethodSpecs
    });

var mouseEventInterfaceProperties = new Map( [
    ['clientX', SimpleProperty('number', 'clientX')],
    ['clientY', SimpleProperty('number', 'clientY')]
]);

var mouseEventMethodSpecs = new Map([
]);

webInterfaces.set('mouseevent',
    {name: 'mouseevent',
        properties:mouseEventInterfaceProperties,
        methods:mouseEventMethodSpecs
    });

var textMetricsInterfaceProperties = new Map( [
    ['width', SimpleProperty('number', 'width')],
    ['actualBoundingBoxLeft', SimpleProperty('number', 'actualBoundingBoxLeft')],
    ['actualBoundingBoxRight', SimpleProperty('number', 'actualBoundingBoxRight')]

]);

var textMetricsMethodSpecs = new Map([
]);

webInterfaces.set('textmetrics',
    {name: 'textmetrics',
        properties:textMetricsInterfaceProperties,
        methods:textMetricsMethodSpecs
    });
