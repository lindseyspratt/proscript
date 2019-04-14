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
// There is a utility Proscript predicate promise_results/2
// that coordinates the use of request_promise_results/1 and
// handle_promise_results/2 with suspending the WAM and
// backtracking to restart the WAM:
//
// promise_results(Promise, _) :- promise_request_results(Promise), halt.
// promise_results(Promise, Results) :- promise_handle_results(Promise, Results).
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
// with the Promise and Results arguments, respectively.
//
// This mechanism can be extended to allow multiple promise_request_results/1
// goals to be evaluated prior to the halt/0 goal evaluation. In this case
// the WAM will not be resumed until all of the requested results have
// been returned or failed. Then promise_handle_results/2 may be used
// to get all of these results, e.g. setof(P-R, promise_handle_results(P,R), ResultPairs).

var idsToPromises = new Map();

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

    request_result(promise, promiseJS);
    return true;
}

async function request_result(promise, promiseJS) {
    await promiseJS.then(
        (result) => {promise_callback(promise, result);}
    );

}

function promise_callback(promise, result) {
    promise_results.push({promise:promise, result:result});
    promise_requests.remove(promise);
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
        let value = promise_results.get(promise);
        if (value) {
            return unify(value, result);
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

    if(result_index < promise_results_key_array.length) {
        update_choicepoint_data(result_index);

        let promise_key = promise_results_key_array[result_index];
        let value = promise_results.get(promise_key);
        return unify(promise, promise_key) && unify(result, value);
    } else {
        destroy_choicepoint();
        return false;
    }
}