"use strict";
//load_state();
//call_directives();
//initialize();
var can_backtrack = false;

jQuery(function ($, undefined) {
    $('#proscriptinterpreter').terminal(function (command, term) {
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
        prompt: '| ?- '
        ,
        onInit: function(term) {
            stdout = function (msg) {
                buffered_write(msg, term);
            };
        }
    });
});

var stdout
= function(msg){
    alert(msg);
};

function predicate_flush_stdout() {
    // no op
    return true;
}

function top_level(query, term) {
    solve_query(query, term);

    top_level_after_solve_query(term);
 }

function top_level_after_solve_query(term) {
    if (promise_requests.size > 0) {
        stdout( 'top_level:' + promise_requests.size + ' promise requests.\n');
    }

    if (state.instruction_suspended) {
        debug_msg("Instruction suspended");
        setup_term_for_instruction_input(term);
    } else if (state.suspended) {
        debug_msg("Suspended");
        setup_term_for_input(term);
    } else if (can_backtrack) {
        term.push(function (command, term) {
            backtrack_level(command, term);
        }, {
            name: 'backtrack',
            prompt: '  ? '
        });
    }
}

function setup_term_for_instruction_input(term) {

    term.push(function (command, term) {
        trace_instruction_level(command, term);
    }, {
        name: 'trace-instruction',
        prompt: $.terminal.escape_formatting(state.trace_instruction_prompt + ' : ')
    });
}

function trace_instruction_level(command, term) {
    input_buffer.push(command);
    try_running();

    trace_instruction_level_after_running(term);
}

function trace_instruction_level_after_running(term) {
    if (state.instruction_suspended) {
        instruction_suspend_set('false');
        term.set_prompt($.terminal.escape_formatting(state.trace_instruction_prompt + ' : '));
    } else {
        term.pop();
        if(state.trace_prompt) {
            term.set_prompt($.terminal.escape_formatting(state.trace_prompt + ' ? '));
        }
    }
}

function setup_term_for_input(term) {

    //stdout('current term=' + term.name() +'\n');

    if(term.name() !== 'trace') {
        term.push(function (command, term) {
            trace_level(command, term);
        }, {
            name: 'trace',
            prompt: $.terminal.escape_formatting(state.trace_prompt + ' ? ')
        });
    } else {
        term.set_prompt($.terminal.escape_formatting(state.trace_prompt + ' ? '));
    }
}

function trace_level(command, term) {
    input_buffer.push(command);
    if (backtrack()) {
        try_running();
    } else {
        stdout('wam error: backtrack failed after getting new input command. Expected backtrack to be active in WAM suspended for input.\n');
    }
    trace_level_after_backtrack(term);
}

function trace_level_after_backtrack(term) {
    if (promise_requests.size > 0) {
        stdout( 'trace_level: ' + promise_requests.size + ' promise requests.\n');
    } else if (state.instruction_suspended) {
        debug_msg("Instruction suspended");
        setup_term_for_instruction_input(term);
    } else if (state.suspended) {
        term.set_prompt($.terminal.escape_formatting(state.trace_prompt + ' ? '));
    } else {
        term.pop();

        if (can_backtrack) {
            term.push(function (command, term) {
                backtrack_level(command, term);
            }, {
                name: 'backtrack',
                prompt: '  ? '
            });
        }
    }



}

function backtrack_level(command, term) {
    if (can_backtrack) {
        if (command === ';') {
            try_backtrack();
            if (state.suspended) {
                setup_term_for_input(term);
            }
        } else if (command === 'a') {
            try_backtrack_all();
        } else if (command === 'h') {
            stdout("Action (; for next solution, a for all solutions, RET to stop, h for this message)\n");
        } else if (command === '') {
            destroy_all_choicepoints();
            can_backtrack = false;
            term.pop();
        } else {
            stdout("Unrecognized command '" + command + "'.\n");
            stdout("Action (; for next solution, a for all solutions, RET to stop, h for this message)\n");
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

        term.echo($.terminal.escape_formatting(line));
    }

    buffer = (buffer ? buffer : '') + lines[lines.length - 1];
}

function solve_query(query, term) {
    stdout = function (msg) {
        buffered_write(msg, term);
    };

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
    try_running();
}

function debug(msg) {
 //   stdout('debug: ' + msg + '\n');
}

function try_running() {
    try {
        if (wam()===false) {
            stdout("false.\n");
        }
    } catch (anything) {
        stdout('wam error: ' + anything + '\n');
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
        if (state.suspended) {
            setup_term_for_input(term);
        }
    }

    stdout("no more solutions.\n");
    can_backtrack = false;
}
