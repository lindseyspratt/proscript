/*
https://dcrazed.net/creative-javascript-examples/
javascript calculator example:

 var el = function(element) {
    if (element.charAt(0) === "#") { // If passed an ID...
      return document.querySelector(element); // ... returns single element
    }

    return document.querySelectorAll(element); // Otherwise, returns a nodelist
  };
  tokens = 32

  // Variables
  var viewer = el("#viewer"), // Calculator screen where result is displayed
    equals = el("#equals"), // Equal button
    nums = el(".num"), // List of numbers
    ops = el(".ops"), // List of operators
    theNum = "", // Current number
    oldNum = "", // First number
    resultNum, // Result
    operator; // Batman

tokens = 37

  // When: Number is clicked. Get the current number selected
  var setNum = function() {
    if (resultNum) { // If a result was displayed, reset number
      theNum = this.getAttribute("data-num");
      resultNum = "";
    } else { // Otherwise, add digit to previous number (this is a string!)
      theNum += this.getAttribute("data-num");
    }

    viewer.innerHTML = theNum; // Display current number

  };

tokens = 39

  // When: Operator is clicked. Pass number to oldNum and save operator
  var moveNum = function() {
    oldNum = theNum;
    theNum = "";
    operator = this.getAttribute("data-ops");

    equals.setAttribute("data-result", ""); // Reset result in attr
  };

tokens = 31

  // When: Equals is clicked. Calculate result
  var displayNum = function() {

    // Convert string input to numbers
    oldNum = parseFloat(oldNum);
    theNum = parseFloat(theNum);

    // Perform operation
    switch (operator) {
      case "plus":
        resultNum = oldNum + theNum;
        break;

      case "minus":
        resultNum = oldNum - theNum;
        break;

      case "times":
        resultNum = oldNum * theNum;
        break;

      case "divided by":
        resultNum = oldNum / theNum;
        break;

        // If equal is pressed without an operator, keep number and continue
      default:
        resultNum = theNum;
    }

    // If NaN or Infinity returned
    if (!isFinite(resultNum)) {
      if (isNaN(resultNum)) { // If result is not a number; set off by, eg, double-clicking operators
        resultNum = "You broke it!";
      } else { // If result is infinity, set off by dividing by zero
        resultNum = "Look at what you've done";
        el('#calculator').classList.add("broken"); // Break calculator
        el('#reset').classList.add("show"); // And show reset button
      }
    }

    // Display result, finally!
    viewer.innerHTML = resultNum;
    equals.setAttribute("data-result", resultNum);

    // Now reset oldNum & keep result
    oldNum = 0;
    theNum = resultNum;

  };

tokens = 139

  // When: Clear button is pressed. Clear everything
  var clearAll = function() {
    oldNum = "";
    theNum = "";
    viewer.innerHTML = "0";
    equals.setAttribute("data-result", resultNum);
  };

tokens = 29

  /* The click events */

  // Add click event to numbers
  for (var i = 0, l = nums.length; i < l; i++) {
    nums[i].onclick = setNum;
  }

tokens = 28

  // Add click event to operators
  for (var i = 0, l = ops.length; i < l; i++) {
    ops[i].onclick = moveNum;
  }

tokens = 28

  // Add click event to equal sign
  equals.onclick = displayNum;

  // Add click event to clear button
  el("#clear").onclick = clearAll;

  // Add click event to reset button
  el("#reset").onclick = function() {
    window.location = window.location;
  };

tokens = 32
---
total tokens = 395

*/

:- op(prop, xfx, 500).
:- op(set_prop, xfx, 500).

% tokens = 19

set_prop_by_selector(Key, Selector) :-
	dom_select_element(Selector, Node)
	  ->    retractall(Key prop _),
	        asserta(Key prop Node).

set_prop_list_by_selector(Key, Selector) :-
	setof(Node, dom_select_element(Selector, Node), Nodes),
	retractall(Key prop _),
	asserta(Key prop Nodes).

set_prop(Key, Value) :-
	retractall(Key prop _),
	asserta(Key prop Value).

% tokens = 71

setup :-
	set_prop_by_selector(viewer, '#viewer'),
	set_prop_by_selector(equals, '#equals'),
	set_prop_list_by_selector(nums, '.num'),
	set_prop_list_by_selector(ops, '.ops'),
	theNum set_prop [],
	oldNum set_prop [],
	resultNum set_prop undefined,
	operator set_prop undefined.

% tokens = 40

% When: Number is clicked. Get the current number selected
set_num(Button) :-
	resultNum prop R,
	dom_element_attribute_value(Button, 'data-num', A),
	atom_codes(A, ACs),
	(R \= undefined
	  ->	theNum set_prop ACs,
		    resultNum set_prop undefined
	 ;
		theNum prop NCs,
	 	append(NCs, ACs, N2Cs),
		theNum set_prop N2Cs
	),
	viewer prop V,
	set_dom_element_inner_html(V, N2Cs).

% tokens = 59

% When: Operator is clicked. Pass number to oldNum and save operator
move_num(Button) :-
	theNum prop NCs,
	oldNum set_prop NCs,
	theNum set_prop [],
	dom_element_attribute_value(Button, 'data-ops', Operator),
	operator set_prop Operator,
	equals prop E,
	set_dom_element_attribute_value(E, 'data-result', '').

% tokens = 40

to_number(Atom, Number) :-
    atom(Atom)
      -> atom_codes(Atom, Codes),
	     number_codes(Number, Codes)
	;
	Atom = Number.

% tokens = 26

/*
function_spec(FunctionSpec, Result) :-
    arity(FunctionSpec, N),
    N > 0,
    \+ functor(FunctionSpec, '$element')
      -> call(FunctionSpec, Result) % call(a(b), c) == call(a(b, c))
    ;
    FunctionSpec = Result.

f(G) :-
    G =.. [H, S|T],
    function_spec(S, E),
    G2 =.. [H, E|T],
    call(G2).

f set_dom_element_inner_html(prop equals, Value)
-
equals prop E,
set_dom_element_inner_html(E, Value)

f_set_dom_element_inner_html(ElementSpec, Value) :-
    function_spec(ElementSpec, Element),
    set_dom_element_inner_html(Element, Value).
*/

% When: Equals is clicked. Calculate result
displayNum :-
	oldNum prop OldNumTerm,
	theNum prop TheNumTerm,
	to_number(OldNumTerm, OldNum),
	to_number(TheNumTerm, TheNum),
	% Convert string input to numbers
	operator prop Operator,
	(Operator = undefined
	  ->	ResultNum = TheNum
	;
	perform_operation(Operator, Op),
	Expr =.. [Op, OldNum, TheNum],
	ResultNum is Expr
	),
	(ResultNum = 'Infinity'
	  ->	Result = 'Look at what you have done.',
		dom_select_element('#calculator', Calculator),
		append_dom_element_class(Calculator, broken),
		dom_select_element('#reset', Reset),
		append_dom_element_class(Reset, show)
	; ResultNum = Result ),
    	% Display result, finally!
	(number(Result) -> number_codes(Result, ResultCodes)
	;atom_codes(Result, ResultCodes)
	),
	viewer prop Viewer,
	set_dom_element_inner_html(Viewer, ResultCodes),
	equals prop Equals,
	set_dom_element_attribute_value(Equals, data_result, Result),
	% Now reset oldNum & keep result
	oldNum set_prop 0,
	theNum set_prop Result.

% tokens = 137


perform_operation(plus, +).

perform_operation(minus, -).

perform_operation(times, *).

perform_operation('divided by', /).

% tokens = 24

% When: Clear button is pressed. Clear everything
clear_all :-
	viewer prop Viewer,
	set_dom_element_inner_html(Viewer, "0"),
	equals prop Equals,
	resultNum prop Result,
	(number(Result) -> number_codes(Result, ResultCodes)
	;atom_codes(Result, ResultCodes)
	),
	set_dom_element_attribute_value(Equals, data_result, ResultCodes),
	oldNum set_prop '',
	theNum set_prop ''.

% tokens = 53

  /* The click events */
setup_click_events :-
	% Add click event to numbers
	nums prop Nums,
	forall(member(Num, Nums), set_dom_element_attribute_value(Num, onclick, 'prolog(set_num)')),
	% Add click event to operators
	ops prop Ops,
	forall(member(Op, Ops), set_dom_element_attribute_value(Op, onclick, 'prolog(move_num)')),
	equals prop Equals,
	set_dom_element_attribute_value(Equals, onclick, 'prolog(display_num)'),
	% Add click event to clear button
	dom_select_element('#clear', Clear),
	set_dom_element_attribute_value(Clear, onclick, 'prolog(clear_all)'),
	% Add click event to reset button
	dom_select_element('#reset', Reset),
	set_dom_element_attribute_value(Reset, onclick, 'prolog(reset)').

% tokens = 77
% total tokens = 566 (641)
