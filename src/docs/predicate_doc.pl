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

doc(acyclic_term/1,
    acyclic_term(? < term * term) is det,
    "Term is a acyclic.",
    [arg(term, "any Prolog term"), compat(iso), cat(terms)] ).
doc(subsumes_term/2, subsumes_term(? < term1 * term, ? < term2 * term) is det, "Term1 subsumes Term2.",
    [arg(term1, "any Prolog term"), arg(term2, "any Prolog term"), compat(iso), cat(terms)]).
doc(var/1, var(? < term * term) is det, "Term is a variable.", [arg(term, "any Prolog term"), compat(iso), cat(terms)]).

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
doc(create_dom_element/2, create_dom_element(a), "replace_dom_element_class", [cat(dom)]).
doc(create_dom_text_node/2, create_dom_text_node(a), "replace_dom_element_class", [cat(dom)]).
doc(append_dom_node_child/2, append_dom_node_child(a), "replace_dom_element_class", [cat(dom)]).
doc(insert_before_dom_node/3, insert_before_dom_node(a), "replace_dom_element_class", [cat(dom)]).
doc(dom_select_element/2, dom_select_element(a), "replace_dom_element_class", [cat(dom)]).
doc(dom_select_all_elements/2, dom_select_all_elements(a), "replace_dom_element_class", [cat(dom)]).
doc(dom_object_property/4, dom_object_property(a), "replace_dom_element_class", [cat(dom)]).
doc(dom_object_method/2, dom_object_method(a), "replace_dom_element_class", [cat(dom)]).
doc(dom_object_method/3, dom_object_method(a), "replace_dom_element_class", [cat(dom)]).
doc(dom_object_type/2, dom_object_type(a), "replace_dom_element_class", [cat(dom)]).
doc(dom_create_object/2, dom_create_object(a), "replace_dom_element_class", [cat(dom)]).
doc(dom_create_object/3, dom_create_object(a), "replace_dom_element_class", [cat(dom)]).
doc(dom_type_reference/4, dom_type_reference(a), "replace_dom_element_class", [cat(dom)]).
doc(dom_release_object/1, dom_release_object(a), "replace_dom_element_class", [cat(dom)]).
doc(set_dom_object_property/3, set_dom_object_property(a), "replace_dom_element_class", [cat(dom)]).
doc(set_dom_object_property/4, set_dom_object_property(a), "replace_dom_element_class", [cat(dom)]).
doc(alert/1, alert(a), "replace_dom_element_class", [cat(dom)]).
doc(dom_window/1, dom_window(a), "replace_dom_element_class", [cat(dom)]).
doc(dom_type_property/4, dom_type_property(a), "replace_dom_element_class", [cat(dom)]).
doc(dom_type_method/5, dom_type_method(a), "replace_dom_element_class", [cat(dom)]).
doc(dom_type_parent/2, dom_type_parent(a), "replace_dom_element_class", [cat(dom)]).
