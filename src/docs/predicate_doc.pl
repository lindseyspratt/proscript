% doc(Functor/Arity, Signatures, Discussion, Tags).
% Signatures = [S1, ...] or just S1
% Sk = (Functor(P1, ...) is Determinisim))
% Determinism = det/semidet/failure/nondet/multi
% Pj = ? < stuff < int = <(?, <(stuff, int))
%   = Instantiation < ArgName < Type, or Instantiation < ArgName, or ArgName < Type, or ArgName.
% Instantiation = '++' | '+' | '-' | '--' | '?' | ':' | '@' | '!'
% Discussion = "Description of the predicate semantics and common uses."
% Tags = [Tag(X1, ...), ...]
% Tag:
% arg(Name, Description)
%   Defines the predicate arguments. Each argument has its own arg tag. Argument declarations normally appear in order used by the predicate.
% throws(Term, Description)
%   Error condition.
% error(Error, Description)
%   As throws(Term, Description), but the exception is embedded in error(Error, Context).
% author(Name)
%   Author of the module or predicate. Multiple entries are used if there are multiple authors.
% version(Version)
%   Version of the module. There is no formal versioning system.
% see(Text)
%   Point to related material. Often contains links to predicates or files.
% deprecated(Alternative)
%   The predicate or module is deprecated. The description specifies what to use in new code.
% compat(Standards_and_systems)
%   When implementing libraries or externally defined interfaces this tag describes to which standard the interface is compatible.
% copyright(Copyright_holder)
%   Copyright notice.
% license(License_conditions)
%   License conditions that apply to the source.
% bug(Bug_description)
%   Known problems with the interface or implementation.
% tbd(Work_to_be_done)
%   Not yet realised behaviour that is anticipated in future versions.
% cat(PredicateCategory)

:- module(predicate_doc, [doc/4]).

% Terms
doc(acyclic_term/1,
    acyclic_term(? < term * term) is det,
    "Term is a acyclic.",
    [arg(term, "any Prolog term"), compat(iso), cat(terms)] ).
doc(subsumes_term/2, subsumes_term(? < term1 * term, ? < term2 * term) is det, "Term1 subsumes Term2.",
    [arg(term1, "any Prolog term"), arg(term2, "any Prolog term"), compat(iso), cat(terms)]).
doc(compare/3, compare(? < indicator * integer, ? < term1 * term, ? < term2 * term) is det,
    "Indicator describes Term1 comparison with Term2. Any two Prolog terms are comparable.
    Indicator is -1 if Term1 is less than Term 2, 0 if Term1 is identical to Term2, and 1 if Term1 is greater than Term2. ",
    [arg(indicator, "-1, 0, or 1"), arg(term1, "any Prolog term"), arg(term2, "any Prolog term"), compat(iso), cat(terms)]).
doc(var/1, var(? < term * term) is det, "Term is a variable.", [arg(term, "any Prolog term"), compat(iso), cat(terms)]).
doc(atom/1, atom(? < term * term) is det, "Term is an atom.", [arg(term, "any Prolog term"), compat(iso), cat(terms)]).
doc(integer/1, integer(? < term * term) is det, "Term is an integer.", [arg(term, "any Prolog term"), compat(iso), cat(terms)]).
doc(float/1, float(? < term * term) is det, "Term is a float (i.e. a number represented with a decimal point).", [arg(term, "any Prolog term"), compat(iso), cat(terms)]).
doc(compound/1, compound(? < term * term) is det, "Term is compound - either a structure or a list.", [arg(term, "any Prolog term"), compat(iso), cat(terms)]).
doc(ground/1, ground(? < term * term) is det, "Term is ground - is not a variable nor is it a compound containing a variable.", [arg(term, "any Prolog term"), compat(iso), cat(terms)]).
doc(= /2, =(? < term1 * term, ? < term2 * term) is det, "Term1 unifies with Term2.",
    [arg(term1, "any Prolog term"), arg(term2, "any Prolog term"), compat(iso), cat(terms)]).
doc(== /2, ==(? < term1 * term, ? < term2 * term) is det, "Term1 matches Term2. Two terms match if they are equal without binding any variables.",
    [arg(term1, "any Prolog term"), arg(term2, "any Prolog term"), compat(iso), cat(terms)]).
doc(functor/3,
    [functor(+ < term * integer, ? < name * term, ? < arity * term) is det,
     functor(- < term * integer, + < name * term, + < arity * term) is det
    ],
    "Term has functor Name and Arity arguments. ",
    [arg(term, "any Prolog term"), arg(name, "an atom"), arg(arity, "an integer"), compat(iso), cat(terms)]).
doc(arg/3, arg(+ < position * integer, + < term * term, ? < arg * term) is det,
    "Argument at Position of Term is Arg. ",
    [arg(position, "an integer"), arg(term, "a term"), arg(arg, "a term"), compat(iso), cat(terms)]).
doc(=.. /2, =..(? < term * term, ? < list * list) is det,
    "Term univ List. List is a list [Functor|Args] where functor(Term, Functor, Arity)
     and Args are the arguments of Term (if any):
     forall(between(1, Arity, N), (arg(N, Term, Arg), nth1(N, Args, Arg))).",
    [arg(term, "any Prolog term"), arg(list, "a list"), compat(iso), cat(terms)]).
doc(copy_term/2, copy_term(? < term1 * term, ? < term2 * term) is det,
    "Term2 is the same as Term1 but with distinct variables such that Term1 and Term2 subsume each other and the
    variables of Term2 have not appeared in the proof tree before evaluating this predicate.",
    [arg(term1, "a Prolog term"), arg(term2, "a Prolog term"), compat(iso), cat(terms)]).

% Runtime
doc(halt/0, halt is det, "Halt the WAM engine. The engine may be restarted - typically after evaluating the backtrack Javascript function.",
    [compat(iso), cat(runtime)]).
doc(current_prolog_flag/2,
    [current_prolog_flag(+ < flag * term, ? < value * term) is det,
     current_prolog_flag(- < flag * term, ? < value * term) is nondet],
    "Prolog Flag has Value. The defined flags are: bounded,
    max_integer, min_integer, integer_rounding_function,
    char_conversion, debug, max_arity, unknown, double_quotes, and dialect.",
    [arg(flag, "a defined flag atom"), arg(value, "a Prolog term"), compat(iso), cat(runtime)]).
doc(set_prolog_flag/2, set_prolog_flag(+ < flag * term, ? < value * term) is det,
    "Set Prolog Flag to Value. Only certain flags are settable: char_conversion, debug, unknown, and double_quotes.",
    [arg(flag, "a defined flag atom"), arg(value, "a Prolog term"), compat(iso), cat(runtime)]).
doc(repeat/0, repeat is nondet, "Always succeeds on initial call and on all redo calls (it backtracks without bound).",
    [compat(iso), cat(runtime)]).

% DOM
doc(remove_dom_element_class/2,
    remove_dom_element_class(element, class),
    "Remove Class from classes defined for Element.",
    [cat(dom)]).
doc(replace_dom_element_class/2,
    replace_dom_element_class(element, class),
    "Replace Class in classes defined for Element.",
    [cat(dom)]).
doc(toggle_dom_element_class/3,
    toggle_dom_element_class(element, class, mode),
    "Toggle Class in classes defined for Element with Mode of either 'add' or 'remove'.",
    [cat(dom)]).
doc(set_dom_element_attribute_value/3, set_dom_element_attribute_value(+ <element*object_id, + <attribute*atom, + <value*atom),
    "Set DOM element attribute to value.",
    [arg(element, " DOM Element object identifier"),
     arg(attribute, "an atom naming an attribute"),
     arg(value, "an atom specifying the value to be assigned to the attribute"),
     cat(dom)]).
doc(dom_element_attribute_value/3,
    [dom_element_attribute_value(+ <element*object_id, + <attribute*atom, + <value*atom) is det,
     dom_element_attribute_value(+ <element*object_id, + <attribute*atom, - <value*atom) is nondet,
     dom_element_attribute_value(+ <element*object_id, - <attribute*atom, + <value*atom) is nondet,
     dom_element_attribute_value(- <element*object_id, + <attribute*atom, + <value*atom) is nondet,
     dom_element_attribute_value(+ <element*object_id, - <attribute*atom, - <value*atom) is nondet,
     dom_element_attribute_value(- <element*object_id, + <attribute*atom, - <value*atom) is nondet,
     dom_element_attribute_value(- <element*object_id, - <attribute*atom, + <value*atom) is nondet,
     dom_element_attribute_value(- <element*object_id, - <attribute*atom, - <value*atom) is nondet],
    "There may be any combination of bindings of Element, Attribute, Value, including none bound.
     If Element is unbound and Attribute and Value are bound then dom_element_attribute_value/3 has two
     strategies for finding Element: if Attribute and Value are bound and Attribute is 'id', 'name', or 'class'
     then use specific Javascript document getElementById(Value), getElementsByName(Value), or getElementsByClass(Value) method
     where Attribute is 'id', 'name', or 'class', respectively; otherwise, get all
     elements using document.querySelectorAll('*') and check each one using element.getAttribute(Attribute)=Value.

     If Attribute is unbound then dom_element_attribute_value/3 checks each possible Element
     using Javascript element.getAttributeNames() to generate all values for Attribute for each Element. For each Attribute and Element value
     the value is checked/retrieved. As above, 'id', 'name',
     and 'class' attributes are handled specially; all other attributes are handled
     using element.getAttribute(Attribute)=Value.",
    [arg(element, " DOM Element object identifier"),
     arg(attribute, "an atom naming an attribute"),
     arg(value, "an atom specifying the value to be assigned to the attribute"),
     cat(dom)]).
doc(create_dom_element/2, create_dom_element(+ < tag * atom, - < element * object_id),
    "Create a DOM element object with tag Tag.",
    [arg(tag, "an HTML tag name (any case)"),
     arg(element, "an object ID structure for an HTML element object."),
     cat(dom)]).
doc(create_dom_text_node/2, create_dom_text_node(+ < text * character_code_list, - < element * object_id),
    "Create a DOM text node object.",
    [arg(text, "a string (represented as a character code list)"),
     arg(element, "an object ID structure for an HTML text node object"),
     cat(dom)]).
doc(append_dom_node_child/2, append_dom_node_child(+ < element * object_id, + < child * object_id),
    "Append a DOM node as a child node.",
    [arg(element, "an object ID structure for an HTML node object"),
     arg(child, "an object ID structure for an HTML node object"),
     cat(dom)]).
doc(insert_before_dom_node/3, insert_before_dom_node(+ < parent * object_id, + < element * object_id, + < before * object_id),
    "Insert child Element before Before element with parent Parent element.",
    [arg(parent, "an object ID structure for an HTML node object"),
     arg(element, "an object ID structure for an HTML node object"),
     arg(before, "an object ID structure for an HTML node object"),
     cat(dom)]).
doc(dom_select_element/2, dom_select_element(+ < query * character_code_list, ? < element * object_id),
    "Select an HTML Element that satisfies the specified Query.",
    [arg(query, "a string (represented as a character code list)"),
     arg(element, "an object ID structure for an HTML element object"),
     cat(dom)]).
doc(dom_select_all_elements/2, dom_select_all_elements(query, element) is nondet,
    "Select each HTML Element that satisfies the specified Query.",
    [arg(query, "a string (represented as a character code list)"),
     arg(element, "an object ID structure for an HTML element object"),
     cat(dom)]).
doc(dom_object_property/4,
    [dom_object_property(+ < type * atom, + < object * object_id, + < property * atom, - < value * term) is nondet,
     dom_object_property(+ < type * atom, - < object * object_id, + < property * atom, + < value * term) is nondet,
     dom_object_property(- < type * atom, + < object * object_id, + < property * atom, + < value * term) is det,
     dom_object_property(+ < type * atom, + < object * object_id, + < property * atom, + < value * term) is det
    ],
    "Determine each Value of Property of Object of Type.
     Property must be a ground value.
     At least two of Type, Object, and Value must be ground.",
    [arg(type, "an atom specifying the object type"),
     arg(object, "an object ID structure for a Web API (or other Javascript) object"),
     arg(property, "an atom naming a defined property for the related object"),
     arg(value, "a term specifying the value of the named property of the related object"),
     cat(dom)]).
doc(dom_object_method/3, dom_object_method(+ < object * object_id, : < methodStructure * callable_term, + < specTerm * list),
    "Evaluate a Javascript method applied to a Javascript Web API object.
     The result of the method (if any) is unified with the last argument
     of the Method structure.
     The Method argument may be qualified with a module name or
     an argument of the Method structure may be qualified. For example:
     dom_object_method(Element, add_event_listener(click, 'foo:bar'(thing)))
     or
     dom_object_method(Element, foo : add_event_listener(click, bar(thing))).
     In the second case, the 'foo' module name is used in conjunction with
     the meta-argument type definition for 'add_event_listener' second argument
     (of '0') to determine
     that 'bar(thing)' should be qualified as 'foo:bar'(thing).
     If the foo module had imported bar/1 from the quux module, then this
     qualification would be 'quux:bar'(thing).
     The SpecTerm is used to define the method call when it is not already defined.
     The SpecTerm has the form [MethodName, ArgTypes, ReturnType] or [MethodName, ArgTypes] if no return.
     ",
    [arg(object, "an object ID structure for a Web API (or other Javascript) object"),
     arg(methodStructure, "a term of the form 'methodName(arg1, arg2, ..)'"),
     arg(specTerm, "either [] or the form [MethodName, ArgTypes, ReturnType] or [MethodName, ArgTypes] if no return"),
     cat(dom)]).
doc(dom_object_method/2, dom_object_method(+ < object * object_id, : < methodStructure * callable_term),
    "This predicate is the same as dom_object_method/3 with a SpecTerm = [].", [cat(dom)]).
doc(dom_object_type/2, dom_object_type(+ < object * object_id, ? < type * atom),
    "Relate a Javascript object to its ProscriptLS type.
    A Web API object that is an instance of HTMLElement has the type 'htmlelement'.",
    [arg(object, "an object ID structure for a Web API (or other Javascript) object"),
     arg(type, "an atom naming the type of the associated object"),
     cat(dom)]).
doc(dom_create_object/3, dom_create_object(+ < type * atom, - < object * object_id, + < spec * list),
    "Create a Javascript object specified by the Type.
    The Type term is the constructor name and arguments (if any).
    The type argument may be of the form ModuleName : Type.
    An argument may be a goal_function type, in which case the ModuleName (inferred or explicit)
    is needed to determine what module holds the predicate(s) of the goal_functor.
    Argument types include: object, string, string_codes, integer, number, boolean, position, goal_function,
    event, and options. Also a type may specify a list or array of items of the same type as 'array_type(Type)', e.g.
    'array_type(integer)' for an array/list of integers.",
    [arg(type, "an atom or structure that specifies the kind of object to create"),
     arg(object, "an object ID structure for a Web API (or other Javascript) object"),
     arg(spec, "a list specifying the types of the arguments of the Type structure (if any)."),
     cat(dom)]).
doc(dom_create_object/2, dom_create_object(+ < type * atom, - < object * object_id),
    "Same as dom_create_object/3 with Spec = [].",
     [arg(type, "an atom that specifies the kind of object to create"),
      arg(object, "an object ID structure for a Web API (or other Javascript) object"),
      cat(dom)]).
doc(dom_release_object/1, dom_release_object(+ < object * object_id),
    "Release internal registration of Javascript structure for Object",
    [arg(object, "an object ID structure for a Web API (or other Javascript) object"), cat(dom)]).
doc(dom_type_reference/4, dom_type_reference(? < type * atom, ? < name * atom, ? < standard * atom, ? < mdn * atom),
    "Relates DOM Web API type and name with reference URLs in W3C standards and Mozilla MDN. This predicate is used in generating ProscriptLS documentation.",
    [arg(type, "an atom that is the ProscriptLS name (all lowercase) of the Web API interface"),
     arg(name, "an atom that is the Web API interface name (proper case)"),
     arg(standard, "an atom that is the URL to a W3C reference for the associated Web API name"),
     arg(mdn, "an atom that is the URL to a Mozilla MDN reference for the associated Web API name"),
    cat(dom)]).
doc(set_dom_object_property/3, set_dom_object_property(object, property, value),
    "Set Property of Object to Value. Property must be in a defined Web API interface for
    ProscriptLS for the interface type for Object or one of its parent types.",
    [arg(object, "object_id of a Javascript object that is an instance of a Web API interface"),
     arg(property, "an atom naming a defined property for the associated object Web API interface"),
     arg(value, "a term of a type appropriate to the associated property"),
     cat(dom)]).
doc(alert/1, alert(+ < term * term), "Display Term in a browser alert dialog.", [arg(term, "any term"), cat(dom)]).
doc(dom_window/1, dom_window(? < window * object_id) is det, "Window is the object ID for the Web API interface HTML Window object.", [arg(window, "the object ID for the HTML window object"), cat(dom)]).
doc(dom_type_property/4, dom_type_property(? < objectType * atom, ? < propertyName * atom, ? < jsName * atom, ? < valueType * atom),
    "Find the related values for a WebInterface API ObjectType, a PropertyName implemented for that type, the Javascript Web API function JsName used
     to implement that property, and the ValueType for that property.
    ",
    [arg(objectType, "an atom naming a Web API interface"),
     arg(propertyName, "an atom naming a property of a Web API interface"),
     arg(jsName, "an atom naming the Javascript function that implements getting the value of a property of a Web API interface"),
     arg(valueType, "an atom specifying the data type returned by a Javascript method"),
     cat(dom)]).
doc(dom_type_method/5, dom_type_method(? < objectType * atom, ? < methodName * atom, ? < implementationName * atom, ? < argumentTypes * list, ? < resultType * atom),
    "Find the related values for a WebInterface API object type, a method name implemented for that type, the Javascript Web API function name used
     to implement that method, the types of arguments for that method, and the result type (possibly undefined).",
    [arg(objectType, "an atom naming a Web API interface"),
     arg(methodName, "an atom naming a method of a Web API interface"),
     arg(implementationName, "an atom naming Javascript function that implements a method of a Web API interface"),
     arg(argumentTypes, "a list of data type specifications for the inputs of an API method."),
     arg(resultType, "an atom specifying the result data type (if any) of an API method"),
     cat(dom)]).
doc(dom_type_parent/2, dom_type_parent(? < objectType * atom, ? < parentType * atom),
    "Web API interface ObjectType has parent interface ParentType.",
    [arg(objectType, "an atom naming a Web API interface"),
     arg(parentType, "an atom naming a Web API interface"),
     cat(dom)]).
