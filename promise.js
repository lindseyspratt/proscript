// Promise object in javascript encapsulates asynchronous
// processing. The functions in this file support the integration
// of some Promise features with Proscript.
//
// The basic integration with Proscript relies on foreign predicates
// that create Promise objects, Prolog terms of the form
// '$promise'(N) to map to these Javascript Promise objects,
// and two foreign predicates to request results from a
// Promise object and to handle the event (callback) when
// the requested results are made available to the Javascript
// runtime environment; request_promise_results/1 and
// handle_promise_results/2, respectively.
//
// There is a utility Proscript predicate promise_result/2
// that coordinates the use of request_promise_result/1 and
// handle_promise_result/2 with suspending the WAM and
// backtracking to restart the WAM:
//
// promise_result(Promise, _) :- request_result(Promise), halt.
// promise_result(Promise, Result) :- handle_result(Promise, Result).
//
// The halt/0 goal suspends the WAM (stops running it), leaving
// its evaluation state intact.
// When the callback (established by promise_request_results/1 goal) is invoked
// by the asynchronous request result it:
// - caches the result value,
// - uses the backtrack function
// to reset the WAM instruction pointer to the second clause, and
// - starts the WAM.
// This WAM restart then evaluates the promise_handle_results/2 goal
// which unifies the cached promise identifier and result value
// with the Promise and Result arguments, respectively.
//
// This mechanism can be extended to allow multiple promise_request_results/1
// goals to be evaluated prior to the halt/0 goal evaluation. In this case
// the WAM will not be resumed until all of the requested results have
// been returned or failed. Then promise_handle_results/2 may be used
// to get all of these results, e.g. setof(P-R, promise_handle_results(P,R), ResultPairs).

var idsToPromises = new Map();
var promisesToIDs = new Map();

function create_promise_structure(promiseJS) {
    var promiseMapID = lookup_promise(promiseJS);
    var lookupPromise = lookup_atom(promiseMapID);
    // '$promise'(lookupPromise)
    var ftor = lookup_functor('$promise', 1);
    var promisePL = alloc_structure(ftor);
    memory[state.H++] = lookupPromise;
    return promisePL;
}

function lookup_promise(promiseTerm) {
    var promiseMapID = promisesToIDs.get(promiseTerm);
    if (promiseMapID) {
        return promiseMapID;
    }

    promiseMapID = 'emi' + promisesToIDs.size;
    promisesToIDs.set(promiseTerm, promiseMapID);
    idsToPromises.set(promiseMapID, promiseTerm);
    return promiseMapID;
}

function get_promise_object(term, ref) {
    let promiseIDObject = {};
    if (!get_promise_id_object(term, promiseIDObject)) {
        return false;
    }
    ref.value = idsToPromises.get(promiseIDObject.value);
    return true;
}

function get_promise_id_object(term, s) {
    if (TAG(term) !== TAG_STR)
        return type_error("promise", term);
    var ftor = VAL(memory[VAL(term)]);
    if (atable[ftable[ftor][0]] === "$promise" && ftable_arity(ftor) === 1) {
        var arg = memory[VAL(term) + 1];
        if (TAG(arg) !== TAG_ATM)
            return type_error("promise_arg");
        s.value = atable[VAL(arg)];
        return true;
    }
    return type_error("promise", term);
}

function predicate_request_result(promise) {
    let promiseObject = {};
    if (!get_promise_object(promise, promiseObject)) {
        return representation_error('promise', promise);
    }
    let promises = [];
    let promiseJS = promiseObject.value;

    promise_requests.set(promise, '');
    request_result(promise, promiseJS);
    return true;
}

async function request_result(promise, promiseJS) {
    await promiseJS.then(
        (result) => {promise_callback(promise, result);}
    );

}

var promise_requests = new Map();
var promise_results = new Map();

function promise_callback(promise, result) {
    promise_results.set(promise, result);
    promise_requests.delete(promise);
    if(promise_requests.size === 0) {
        if(backtrack()){
            if(! wam()) {
                throw 'fail callback ' + promise + ' result ' + result;
            }
        } else {
            throw 'error callback ' + promise + ' result ' + result;
        }
    } else {
        // waiting on one or more requests.
    }
}

var promise_results_key_array = [];

function predicate_handle_result(promise, result) {
    if (TAG(promise) !== TAG_REF) {
        let text = promise_results.get(promise);
        if (text) {
            let memory_file = create_memory_file_structure(text);

            return unify(result, memory_file);
        } else {
            return false;
        }
    }

    let result_index = -1;
    if (state.foreign_retry) {
        result_index = state.foreign_value;

        debug_msg("Is retry! Setting result_index back to " + result_index);
    } else {
        create_choicepoint();
        let key_iterator = promise_results.keys();
        promise_results_key_array = [];
        for (let ofst = 0; ofst < promise_results.size; ofst++) {
            promise_results_key_array.push(key_iterator.next());
        }

    }

    result_index++;

    let text;
    let memory_file;
    while(! text && result_index < promise_results_key_array.length) {
        let promise_key = promise_results_key_array[result_index];
        text = promise_results.get(promise_key);
        if (text) {
            memory_file = create_memory_file_structure(text);
        } else {
            result_index++;
        }
    }

    if(result_index < promise_results_key_array.length) {
        update_choicepoint_data(result_index);
        return unify(promise, promise_key) && unify(result, memory_file);
     } else {
        destroy_choicepoint();
        return false;
    }
}

function predicate_fetch_promise(url, promise) {
    if(TAG(url) !== TAG_ATM) {
        return type_error('atom', url);
    }

    let promiseJS = fetch_promise(atable[VAL(url)]);
    let promisePL = create_promise_structure(promiseJS);
    return unify(promise, promisePL);
}

async function fetch_promise(urlJS) {
    if (!urlJS.includes(".")) {
        urlJS += ".pl";
    }
    const response = await fetch(urlJS);
    return response.text();
}

async function consult(urls, next_goal) {
    // fetch all the URLs in parallel
    const textPromises = urls.map(async url => {
        if(! url.includes(".")) {
            url += ".pl";
        }
        const response = await fetch(url);
        return response.text();
    });

    // compile them in sequence
    for (const textPromise of textPromises) {
        await textPromise.then(function(text){
            let index = text_to_memory_file(text);
            return "'$memory_file'(" + index + ")";
        }).then(function(memfile){
            proscript("compile_and_free_memory_file(" + memfile + ")");

        });
    }

    if(next_goal && next_goal !== '') {
        proscript(next_goal);
    }
}
