:- ensure_loaded('../library/data_predicates').

:- initialization(data_predicate_dynamics).

data_predicates(xh, x_history,[old]).
data_predicates(hp, h_ptr,[value]).
data_predicates(q, query, [value]).

prevent_backspace(Event) :-
    Event >> [
        keyCode +:> 8,
        target +:> output_console,
        preventDefault,
        stopPropagation
        ],
    backspace,
    !,
    fail.
prevent_backspace(_Event).

/*
For standalone_console.js:

var stdout_buffer;

function predicate_flush_stdout()
{
    if (stdout_buffer && stdout_buffer.innerHTML !== "")
        stdout("\n");
    return true;
}

function stdout(msg)
{
    if(output_console) {
        output_console.removeChild(stdout_buffer);
        var lines = (stdout_buffer.innerHTML + msg).split('\n');
        for (var i = 0; i < lines.length - 1; i++) {
            debug(lines[i]);
        }
        stdout_buffer.innerHTML = lines[lines.length - 1];
        output_console.appendChild(stdout_buffer);
    } else {
        alert(msg);
    }
}
*/

onload(InitialConsult) :-
    OC >> [id -:> stdout],
    create_dom_element(div, SB),
    SB >+> innerHTML <: "",
    dom_append_node_child(OC, SB),
    (InitialConsult != '' -> consult_for_test;true),
    create_dom_element(div, QN),
    QN >+> [className <: query,
        innerHTML <: "?-"],
    dom_append_node_child(OC, QN),
    scroll_to_bottom.

debug(Msg) :-
    OC >> [id -:> stdout],
    create_dom_element(div, NE),
    append_lists(["<div>", Msg, "</div>"], ExtendedMsg),
    NE >+> innerHTML <: ExtendedMsg,
    dom_append_node_child(OC, NE),
    scroll_to_bottom.

consult_for_test :-
    Code >> [ id -:> code, value +:> CodeAtom],
    consult_atom(CodeAtom).

backspace :-
    query(Value),
    QN >+> [class :> query, innerHTML <: "?-" + Value],
    OC >> [id -:> stdout],
    dom_remove_child(OC, QN),
    dom_append_node_child(OC, QN),
    scroll_to_bottom.

keydown(Event) :-
    Event >+> keyCode :> 38,
    \+ can_backtrack
      -> Event >*> [preventDefault, stopPropagation],
         increment_h_ptr(HPtr),
         x_history_length(XHLength),
         (HPtr >= XHLength
           -> HPtrN is XHLength - 1,
              update_h_ptr(HPtrN)
          ;
          HPtrN = HPtr
         ),
         x_history(HPtrN, Query),
         update_query(Query),
         QN >+> innerHTML <: "?-" + Query,
         dom_remove_child(QN),
         dom_append_node_child(QN),
         scroll_to_bottom
    ;
    Event >+> keyCode :> 40,
    \+ can_backtrack
      -> Event >*> [preventDefault, stopPropagation],
         decrement_h_ptr(HPtr),
         (HPtr < 0
           -> update_h_ptr(-1),
              query(Query)
          ;
          x_history(HPtr, Query),
          update_query(Query)
         ),
         x_history(HPtrN, Query),
         QN >+> innerHTML <: "?-" + Query,
         dom_remove_child(QN),
         dom_append_node_child(QN),
         scroll_to_bottom
    ;
    true.

keypress(Event) :-
    Event >+> [altKey :> '', ctrlKey :> '', metaKey :> ''],
    !,
    Event >+> keyCode :> KeyCode,
    keypress1(Event, KeyCode).

keypress1(Event, 8) :-
    !,
    backspace.
keypress1(Event, 59) :-
    can_backtrack,
    !,
    create_dom_element(div, OQ),
    OQ >> [innerHTML <:+ ";", className <:+ "old_query"],
    OC >-> id :> output_console,
    dom_append_node_child(OC, OQ),
    scroll_to_bottom,
    ??? backtrack -> try_running ...

/*

function keypress(e)
{
    var old_query;

    if (e.altKey || e.ctrlKey || e.metaKey)
        return;
    e.preventDefault();
    e.stopPropagation();
    if (e.keyCode === 8)
    {
        backspace();
    }
    else if (e.keyCode === 59 && can_backtrack)
    {
        old_query = document.createElement('div');
        old_query.innerHTML = ";";
        old_query.className = "old_query";
        output_console.appendChild(old_query);
        scroll_to_bottom();
        if (backtrack())
        {
            try_running();
        }
        else
        {
            stdout("false.\n");
            can_backtrack = false;
            query = "";
            query_node = document.createElement('div');
            query_node.className = "query";
            query_node.innerHTML = "?-";
            output_console.appendChild(query_node);
            scroll_to_bottom();
        }
    }
    else if (e.keyCode === 13 && can_backtrack)
    {
        // Cut choicepoints (?)
        state.B = 0;
        can_backtrack = false;
        query = "";
        query_node = document.createElement('div');
        query_node.className = "query";
        query_node.innerHTML = "?-";
        output_console.appendChild(query_node);
        scroll_to_bottom();
    }
    else if (e.keyCode === 13)
    {
        // call the toplevel handler
        // ARGH. MUST reset registers for new query, especially after failure!
        initialize();
        allocate_first_frame();

        var ftor = VAL(lookup_functor("wam_compiler:repl", 1));
        var pred = predicates[ftor];
        var pi = predicates[ftor].clause_keys[0];
        state.current_predicate = pred;
        code = pred.clauses[pi].code;
        register[0] = lookup_atom(query);
        // Make the query a permanent part of the output
        output_console.removeChild(query_node);
        old_query = document.createElement('div');
        old_query.innerHTML = "?-" + query;
        old_query.className = "old_query";
        output_console.appendChild(old_query);
        x_history.unshift(query);
        h_ptr = -1;
        try_running();
    }
    else
    {
        query += String.fromCharCode(e.keyCode);
        query_node.innerHTML = "?-" + query;
        output_console.removeChild(query_node);
        output_console.appendChild(query_node);
        scroll_to_bottom();
    }
}


*/
