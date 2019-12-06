:- module(object, [
    op(200, fx, *),
    op(200, fx, @),
    op(500, xfx, :>),
    op(500, xfx, <:),
    op(500, xfx, -:>),
    op(500, xfx, <:-),
    op(500, xfx, +:>),
    op(500, xfx, <:+),
    op(500, xfx, *:>),
    op(700, yfx, >->),
    op(700, yfx, >+>),
    op(700, yfx, >*>),
    op(700, yfx, >@>),
    (>>)/2, (>->)/2, (>+>)/2, (>*>)/2, (>@>)/2]).

:- meta_predicate(('>>'((:), (:)), '>*>'((:), (:) ), '>@>'((:), (:) ))).

:- op(200, fx, *).
:- op(200, fx, @).
:- op(500, xfx, :>).
:- op(500, xfx, <:).
:- op(500, xfx, -:>).
:- op(500, xfx, <:-).
:- op(500, xfx, +:>).
:- op(500, xfx, <:+).
:- op(500, xfx, *:>).
:- op(700, yfx, >->).
:- op(700, yfx, >+>).
:- op(700, yfx, >*>).
:- op(700, yfx, >@>).

% The ProscriptLS object language provides a concise way
% to access object properties, methods and Element
% object attributes.
% There are operators for accessing these things individually
% and for accessing sequences of these things.
%
% >->/2 applies an attribute operation to an object:
%   Obj >-> A :> V
%       gets value V for attribute A of object Obj.
%   Obj >-> A <: V
%       sets value V for attribute A of object Obj.
%   Obj >-> [X1, ..., Xn]
%       applies Xi attribute operation in order to Obj.
%       Each Xi is either (A :> V) or (A <: V). E.g.
%       Obj >-> [a :> AV, b <: 1, c:> CV].
%
% >+>/2 applies a propery operation to an object:
%   Obj >+> P :> V
%       gets value V for property P of object Obj.
%   Obj >+> P <: V
%       sets value V for property P of object Obj.
%   Obj >+> [X1, ..., Xn]
%       applies Xi property operation in order to Obj.
%       Each Xi is either (P :> V) or (P <: V). E.g.
%       Obj >+> [a :> AV, b <: 1, c:> CV].
%
% >*>/2 applies a method operation to an object:
%   Obj >*> M :> V
%       gets value V for method M of object Obj.
%   Obj >*> M
%       evaluates method M of object Obj. There is no result value.
%   Obj >*> [X1, ..., Xn]
%       applies Xi method operation in order to Obj.
%       Each Xi is either (M :> V) or (M). E.g.
%       Obj >*> [a :> AV, b(1, 2) :> BV, c].
%   A method may be a single atom such as 'beginPath'
%   or a structure such as 'moveTo(200, 20)'.
%
% >@>/2 applies a Prolog goal to an object where the last argument to the goal is the object:
%   Obj >@> G
%       invokes call(G, Obj)
%   Obj >@> [X1, ..., Xn]
%       applies Xi goal in order to Obj. E.g.
%       Obj >@> [a, b(1, 2)].
%   A goal may be a single atom such as 'foo'
%   or a structure such as 'bar(200, 20)'. These are evaluated using call/2:
%   call(foo, Obj) or call(bar(200, 20), Obj).
%   These calls are the same as evaluating foo(Obj) or bar(200, 20, Obj).
%
% >>/2 applies any combination of attribute get/set, property get/set, and method
% operations.
%   Obj >> * M
%       applies method(s) M to Obj.
%   Obj >> - AV
%       applies attribute operation(s) AV to Obj. AV is either (A :> V) or (A <: V).
%   Obj >> + PV
%       applies property operation(s) PV to Obj. PV is either (P :> V) or (P <: V).
%   Obj >> @ G
%       applies goal(s) G to Obj.
%   Obj >> A -:> V
%       applies attribute operation (A :> V) to Obj.
%   Obj >> A <:- V
%       applies attribute operation (A <: V) to Obj.
%   Obj >> P +:> V
%       applies property operation (P :> V) to Obj.
%   Obj >> P <:+ V
%       applies property operation (P <: V) to Obj.
%   Obj >> M *:> V
%       applies method operation (M :> V) to Obj.
%   Obj >> M (where M is none of the above, is not = (_ :> _), and is not a list)
%       applies method Operation M to Obj.
%   Obj >> [X1, X2,...] applies X1 to Obj, then X2 to Obj, and so on.
%       If the X1 operation is an attribute value specification that identifies
%       an object (such as (id -:> canvas)), then Obj may be unbound initially.
%       It will be bound by X1, and the bound Obj will be used for subsequent applications of Xi.
%       The Xi may be any of the above forms: * M, - AV, + PV, @ G, A -:> V, A <:- V,
%       P +:> V, P <:+ V, and M *:> V. Xi may also be a list.

>>(ObjectExpression, Application) :-
    object_expression(ObjectExpression, Object),
    general_application(Object, Application).

general_application(_, _M : []) :-
    !.
general_application(Obj,  M : [H|T]) :-
    !,
    general_application(Obj,  M : H),
    general_application(Obj,  M : T).
general_application(_Obj,  M : {G}) :-
    !,
    call(M : G).
general_application(Obj, M : (* Method)) :-
    !, % method invocation
    method_application(Obj, M : Method).
general_application(Obj, _M : (+ PV)) :-
    !, % property/value invocation
    property_application(Obj, PV).
general_application(Obj, _M : (- AV)) :-
    !, % attribute/value invocation
    attribute_application(Obj, AV).
general_application(Obj, M : (@ G)) :-
    !, % goal invocation
    goal_application(Obj, M : G).
general_application(Obj, _M : (A -:> V)) :-
    !,
    attribute_application(Obj, A :> V).
general_application(Obj, _M : (P +:> V)) :-
    !,
    property_application(Obj, P :> V).
general_application(Obj, M : (Method *:> V)) :-
    !,
    method_application(Obj, M : Method :> V).
general_application(Obj, _M : (A <:- V)) :-
    !,
    attribute_application(Obj, A <: V).
general_application(Obj, _M : (P <:+ V)) :-
    !,
    property_application(Obj, P <: V).
general_application(Obj, M : Method) :-
    method_application(Obj, M : Method).

>->(ObjectExpression, Application) :-
    object_expression(ObjectExpression, Object),
    attribute_application(Object, Application).

attribute_application(_, []).
attribute_application(Obj, [H|T]) :-
    !,
    attribute_application(Obj, H),
    attribute_application(Obj, T).
attribute_application(_Obj, {G}) :-
    !,
    call(G).
attribute_application(Obj, :>(Attribute, V)) :-
    !,
    dom_element_attribute_value(Obj, Attribute, V).
attribute_application(Obj, <:(Attribute, V)) :-
    set_dom_element_attribute_value(Obj, Attribute, V).

>+>(ObjectExpression, Application) :-
    object_expression(ObjectExpression, Object),
    property_application(Object, Application).

property_application(_, []).
property_application(Obj, [H|T]) :-
    !,
    property_application(Obj, H),
    property_application(Obj, T).
property_application(_Obj, {G}) :-
    !,
    call(G).
property_application(Type-Obj, :>(Property, V)) :-
    !,
    dom_object_property(Type, Obj, Property, V).
property_application(Obj, :>(Property, V)) :-
    !,
    dom_object_property(_, Obj, Property, V).
property_application(Obj, <:(Property, V)) :-
    set_dom_object_property(Obj, Property, V).

>*>(ObjectExpression, Application) :-
    object_expression(ObjectExpression, Object),
    method_application(Object, Application).

method_application(_, _Module : []) :-
    !.
method_application(Obj, Module : [H|T]) :-
    !,
    method_application(Obj, Module : H),
    method_application(Obj, Module : T).
method_application(_Obj, Module : {G}) :-
    !,
    call(Module : G).
method_application(Obj, Module : :>(Method, V)) :-
    !,
    Method =.. [F|As],
    append(As, [V], AV),
    MethodX =.. [F|AV],
    dom_object_method(Obj, Module : MethodX).
method_application(Obj, Module : Method) :-
    dom_object_method(Obj, Module : Method).

>@>(ObjectExpression, Application) :-
    object_expression(ObjectExpression, Object),
    goal_application(Object, Application).

goal_application(_, _ : []) :-
    !.
goal_application(Obj, M : [H|T]) :-
    !,
    goal_application(Obj, M : H),
    goal_application(Obj, M : T).
goal_application(_Obj, M : {G}) :-
    !,
    call(M : G).
goal_application(Obj, Goal) :-
    call(Goal, Obj).

object_expression(M:Expression, Object) :-
    atom(M),
    !,
    bottom_right(Expression, Object),
    evaluate_expression(M:Expression).
object_expression(Expression, Object) :-
    bottom_right(Expression, Object),
    evaluate_expression(user:Expression).

% Expression is a tree of binary and unary nodes.
% the 'result' of evaluating Expression is the last
% term (generally a variable).

bottom_right(Expression, Expression) :-
    var(Expression),
    !.
bottom_right(Expression, Expression) :-
    atomic(Expression),
    !.
bottom_right([_H1, H2|T], Expression) :-
    !,
    bottom_right([H2|T], Expression).
bottom_right([BR], BR) :-
    !.
bottom_right('$obj'(X), '$obj'(X)) :-
    !.
bottom_right(Expression, BR) :-
    Expression =.. [_|As],
    append(_, [X], As),
    bottom_right(X, BR).

evaluate_expression(_:Expression) :-
    var(Expression),
    !.
evaluate_expression(_:Expression) :-
    atomic(Expression),
    !.
evaluate_expression(_:[_|_]) :-
    !.
evaluate_expression(_:[_]) :-
    !.
evaluate_expression(_:'$obj'(_)) :-
    !.
evaluate_expression(Expression) :-
    call(Expression).