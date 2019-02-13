load_state();
initialize();
var can_backtrack = false;
var x_history = [];

jQuery(function($, undefined) {
    $('#simpletest').terminal(function(command, term) {
        if (command !== '') {
            try {
                top_level(command, this);
                // if (result !== undefined) {
                //     this.echo(new String(result));
                // }
            } catch(e) {
                this.error(String(e));
            }
        } else {
            this.echo('');
        }
    }, {
        greetings: 'Proscript Interpreter',
        name: 'pl_interp',
        height: 200,
        prompt: '| ?- '
    });
});

var stdout;

function predicate_flush_stdout () {
    // no op
    return true;
}

function top_level(query, term) {
    solve_query(query, term);

    if(can_backtrack) {
        term.push(function(command, term) {
            backtrack_level(command, term);
        },{
            name: 'backtrack',
            prompt: '  ? '
        });
     }
}

function backtrack_level(command, term) {
    if(can_backtrack) {
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
    var lines = msg.split('\n');
    var line;
    for (var ofst = 0; ofst < lines.length - 1; ofst++) {
        if (ofst === 0 && buffer && buffer !== '') {
            line = buffer + lines[ofst];
            buffer = '';
        } else {
            line = lines[ofst];
        }

        term.echo(line);
    }

    buffer = lines[lines.length - 1];
}

function solve_query(query, term) {
    stdout = function(msg) {
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

    function try_running()
    {
        try
        {
            if (!wam())
            {
                stdout("false.\n");
            }
        }
        catch (anything)
        {
            stdout('wam error: ' + anything);
        }
        if (state.B !== 0)
        {
            debug_msg("Can backtrack");
            can_backtrack = true;
        }
        else
        {
            debug_msg("No more solutions after this");
            can_backtrack = false;
        }
    }

function try_backtrack() {
    if (backtrack())
    {
        try_running();
    }
    else
    {
        stdout("no more solutions.\n");
        can_backtrack = false;
    }
}

function try_backtrack_all() {
    while (backtrack())
    {
        try_running();
    }

    stdout("no more solutions.\n");
    can_backtrack = false;
}