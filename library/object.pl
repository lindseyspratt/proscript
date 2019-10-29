:- module(object, [
    op(200, fx, *),
    op(200, fx, @),
    op(500, xfy, :>),
    op(500, xfx, <:),
    op(500, xfy, -:>),
    op(500, xfx, <:-),
    op(500, xfy, +:>),
    op(500, xfx, <:+),
    op(500, xfy, *:>),
    op(700, xfy, >->),
    op(700, xfy, >+>),
    op(700, xfy, >*>),
    op(700, xfy, >@>),
    (>>)/2, (>->)/2, (>+>)/2, (>*>)/2, (>@>)/2]).

:- meta_predicate(('>>'(?, (:)), '>*>'(?, (:) ), '>@>'(?, (:) ))).

:- op(200, fx, *).
:- op(200, fx, @).
:- op(500, xfy, :>).
:- op(500, xfx, <:).
:- op(500, xfy, -:>).
:- op(500, xfx, <:-).
:- op(500, xfy, +:>).
:- op(500, xfx, <:+).
:- op(500, xfy, *:>).
:- op(700, xfy, >->).
:- op(700, xfy, >+>).
:- op(700, xfy, >*>).
:- op(700, xfy, >@>).

% The Proscript object language provides a concise way
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

>>(_, _M : []) :-
    !.
>>(Obj,  M : [H|T]) :-
    !,
    >>(Obj,  M : H),
    >>(Obj,  M : T).
>>(_Obj,  M : {G}) :-
    call(M : G).
>>(Obj, M : (* Method)) :-
    !, % method invocation
    >*>(Obj, M : Method).
>>(Obj, _M : (+ PV)) :-
    !, % property/value invocation
    >+>(Obj, PV).
>>(Obj, _M : (- AV)) :-
    !, % attribute/value invocation
    >->(Obj, AV).
>>(Obj, M : (@ G)) :-
    !, % goal invocation
    >@>(Obj, M : G).
>>(Obj, _M : (A -:> V)) :-
    !,
    >->(Obj, A :> V).
>>(Obj, _M : (P +:> V)) :-
    !,
    >+>(Obj, P :> V).
>>(Obj, _M : (Method *:> V)) :-
    !,
    >*>(Obj, Method :> V).
>>(Obj, _M : (A <:- V)) :-
    !,
    >->(Obj, A <: V).
>>(Obj, _M : (P <:+ V)) :-
    !,
    >+>(Obj, P <: V).
>>(Obj, M : Method) :-
    >*>(Obj, M : Method).


>->(_, []).
>->(Obj, [H|T]) :-
    !,
    >->(Obj, H),
    >->(Obj, T).
>->(_Obj, {G}) :-
    !,
    call(G).
>->(Obj, :>(Attribute, V)) :-
    dom_element_attribute_value(Obj, Attribute, V).
>->(Obj, <:(Attribute, V)) :-
    set_dom_element_attribute_value(Obj, Attribute, V).

>+>(_, []).
>+>(Obj, [H|T]) :-
    !,
    >+>(Obj, H),
    >+>(Obj, T).
>+>(_Obj, {G}) :-
    !,
    call(G).
>+>(Type-Obj, :>(Property, V)) :-
    !,
    dom_object_property(Type, Obj, Property, V).
>+>(Obj, :>(Property, V)) :-
    dom_object_property(_, Obj, Property, V).
>+>(Obj, <:(Property, V)) :-
    set_dom_object_property(Obj, Property, V).

>*>(_, _Module : []) :-
    !.
>*>(Obj, Module : [H|T]) :-
    !,
    >*>(Obj, Module : H),
    >*>(Obj, Module : T).
>*>(_Obj, Module : {G}) :-
    !,
    call(Module : G).
>*>(Obj, Module : :>(Method, V)) :-
    !,
    Method =.. [F|As],
    append(As, [V], AV),
    MethodX =.. [F|AV],
    dom_object_method(Obj, Module : MethodX).
>*>(Obj, Module : Method) :-
    dom_object_method(Obj, Module : Method).

>@>(_, _ : []) :-
    !.
>@>(Obj, M : [H|T]) :-
    !,
    >@>(Obj, M : H),
    >@>(Obj, M : T).
>@>(_Obj, M : {G}) :-
    !,
    call(M : G).
>@>(Obj, Goal) :-
    call(Goal, Obj).
