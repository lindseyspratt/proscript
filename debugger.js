var input_buffer = [];

function predicate_get_terminal_char(c) {
    let char = input_buffer.shift();
    if(char) {
        return unify(c, lookup_atom(char));
    } else {
        return false;
    }

}

var trace_retry = 'false';

function predicate_trace_set_retry(value) {
    trace_retry = atable[VAL(value)];
    return true;
}

function predicate_trace_retry_value(value) {
    return unify(value, lookup_atom(trace_retry));
}