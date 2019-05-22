
/*
The 'name' path is not required to be sequential. A path of [a,b] names some node
with name 'a' that has a sub-node with name 'b'. A node p1 has children q21 and q22 and q21
has children r31 and r32 and q22 has children s31 and s32. If q22 has name 'a' and s31 has name 'b', then path
of [a,b] would select s31.
Since multiple nodes may have the same name this predicate may set values on multiple leaf nodes.
*/
set_dom_name_path_value([H|T], V) :-
    append("[name=", H, SelectPrefix),
    append(SelectPrefix, "]", Select),
    dom_select_element(Select, E),
    (V = [_|_] -> atom_codes(VA, V);VA = V),
    set_dom_name_path_value(T, E, VA).

set_dom_name_path_value([], E, V) :-
    set_dom_element_attribute_value(E, value, V).
% Set value for leaf in path with a direct child with name H and sub-nodes for path T.
set_dom_name_path_value([H|T], E, V) :-
    dom_object_property(_, E, child, C),
    atom_codes(HA, H),
    dom_element_attribute_value(C, name, HA),
    set_dom_name_path_value(T, C, V).
% Set value for leaf in path with an indirect child with name H and sub-nodes for path T.
set_dom_name_path_value([H|T], E, V) :-
    dom_object_property(_, E, child, C),
    set_dom_name_path_value([H|T], C, V).

dom_page_offset(Top, Left, HTMLElement) :-
    dom_page_offset(0, 0, Top, Left, HTMLElement).

dom_page_offset(TopIN, LeftIN, Top, Left, HTMLElement) :-
    dom_object_property(_, HTMLElement, offsetTop, LTop),
    dom_object_property(_, HTMLElement, offsetLeft, LLeft),
    TopNEXT is TopIN + LTop,
    LeftNEXT is LeftIN + LLeft,
    (dom_object_property(_, HTMLElement, offsetParent, Parent)
      -> dom_page_offset(TopNEXT, LeftNEXT, Top, Left, Parent)
    ;
    Top = TopNEXT,
    Left = LeftNEXT
    ).
