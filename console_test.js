load_state();
initialize();
var can_backtrack = false;
var x_history = [];

jQuery(function ($, undefined) {
    $('#simpletest').terminal(function (command, term) {
        if (command !== '') {
            try {
                top_level(command, this);
                // if (result !== undefined) {
                //     this.echo(new String(result));
                // }
            } catch (e) {
                this.error(String(e));
            }
        } else {
            this.echo('');
        }
    }, {
        greetings: 'Proscript Interpreter',
        name: 'pl_interp',
        height: 600,
        prompt: '| ?- ',
        keypress: key_press_input
    });
});

function test_keypress(e) {
    // characters not handled by this keypress function include: newline, backspace/delete
    // These characters may be being intercepted by an implicit keydown function.

    // Keys in e:
    // originalEvent, type, isDefaultPrevented, target, currentTarget, relatedTarget, timeStamp,
    // jQuery331008416865328782674, delegateTarget, handleObj, data, constructor, isPropagationStopped,
    // isImmediatePropagationStopped, isSimulated, preventDefault, stopPropagation, stopImmediatePropagation,
    // altKey, bubbles, cancelable, changedTouches, ctrlKey, detail, eventPhase, metaKey, pageX, pageY,
    // shiftKey, view, char, charCode, key, keyCode, button, buttons, clientX, clientY, offsetX, offsetY,
    // pointerId, pointerType, screenX, screenY, targetTouches, toElement, touches, which

    alert('Key: ' + e.key + '\n' + 'char: ' + e.char + ', charCode:' + e.charCode + ', keyCode: ' + e.keyCode );
    // return false; // this 'false' prevents the key from being displayed.
}

var stdout;

function predicate_flush_stdout() {
    // no op
    return true;
}

function top_level(query, term) {
    solve_query(query, term);

    top_level_after_solve_query(term);
 }

 function top_level_after_solve_query(term) {
     if (state.suspended) {
         debug_msg("Suspended");
         setup_keypress_for_input(term);
     } else if (can_backtrack) {
         term.push(function (command, term) {
             backtrack_level(command, term);
         }, {
             name: 'backtrack',
             prompt: '  ? '
         });
     }

 }
function backtrack_level(command, term) {
    if (can_backtrack) {
        if (command === ';') {
            try_backtrack();
        } else if (command === 'a') {
            try_backtrack_all();
        } else if (command === 'h') {
            stdout("Action (; for next solution, a for all solutions, RET to stop)");
        } else {
            can_backtrack = false;
            term.pop();
        }
    } else {
        term.pop();
    }
}

var buffer;

function buffered_write(msg, term) {
    let lines = msg.split('\n');
    for (let ofst = 0; ofst < lines.length - 1; ofst++) {
        let line;
        if (ofst === 0 && buffer && buffer !== '') {
            line = buffer + lines[ofst];
            buffer = '';
        } else {
            line = lines[ofst];
        }

        term.echo(line);
    }

    buffer = (buffer ? buffer : '') + lines[lines.length - 1];
}

var keypress_is_active = false;

function setup_keypress_for_input() {
    keypress_is_active = true;
}

function disable_keypress_for_input() {
    keypress_is_active = false;
}

function key_press_input(e, term) {
    if (keypress_is_active) {
        disable_keypress_for_input();
        setTimeout(function () {
            input_buffer.push(e.key);
            if (backtrack()) {
                try_running();
            } else {
                stdout('wam error: backtrack failed after getting new input character. Expected backtrack to be active in WAM suspended for input.');
            }
            top_level_after_solve_query(term)
        }, 0);
        return false; // do not echo the input. It is printed by the debugger.
    }
}

function solve_query(query, term) {
    stdout = function (msg) {
        buffered_write(msg, term);
    };

    // call the toplevel handler
    // ARGH. MUST reset registers for new query, especially after failure!
    initialize();
    allocate_first_frame();

    var ftor = VAL(lookup_functor("repl", 1));
    var pred = predicates[ftor];
    var pi = predicates[ftor].clause_keys[0];
    state.current_predicate = pred;
    code = pred.clauses[pi].code;
    register[0] = lookup_atom(query);
    x_history.unshift(query);
    h_ptr = -1;
    try_running();
}

function debug(msg) {
    stdout('debug: ' + msg + '\n');
}

function try_running() {
    try {
        if (wam()===false) {
            stdout("false.\n");
        }
    } catch (anything) {
        stdout('wam error: ' + anything);
    }

    if (state.B !== 0) {
        debug_msg("Can backtrack");
        can_backtrack = true;
    } else {
        debug_msg("No more solutions after this");
        can_backtrack = false;
    }
}

function try_backtrack() {
    if (backtrack()) {
        try_running();
    } else {
        stdout("no more solutions.\n");
        can_backtrack = false;
    }
}

function try_backtrack_all() {
    while (backtrack()) {
        try_running();
    }

    stdout("no more solutions.\n");
    can_backtrack = false;
}
