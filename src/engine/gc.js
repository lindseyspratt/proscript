"use strict";

let gc_environment = 'browser'; // 'console'
let display_gc = false;
let display_minor_gc = false;

function predicate_gc()
{
    let msgIn = "Before GC, heap is " + state.H;
    //stdout("===" + msgIn + "\n" + '\n');
    gc_main_debug(msgIn);
    // WARNING: This assumes ONLY predicate_gc will mark things!
    total_marked = 0;

    let before;
    let after;

    if(display_gc) {
        // debugging only

        before = [];
        let e = state.E;
        let envsize = state.CP.code[state.CP.offset - 1];
        while (e !== HEAP_SIZE) {
            for (let i = 0; i < envsize; i++) {
                before.push(record_term(memory[e + 2 + i]));
            }
            let envcp = memory[e + 1];
            envsize = envcp.code[envcp.offset - 1];
            e = memory[e];
        }

//        gc_debug('===check stacks before mark');
        check_stacks(false);
    }
    mark();

//    gc_debug('===check stacks after mark');
//    check_stacks(true);

    push_registers();
    //   gc_debug('after push_registers, before sweep_trail');
    sweep_trail();
//    gc_debug('after sweep_trail, before sweep_stack');
    sweep_stack();


//    gc_debug('after sweep_stack, before compact');
    compact();
//    gc_debug('after compact, before pop_registers');
    pop_registers();
//    gc_debug('after pop_registers');
    state.H = total_marked;
    if(display_gc) {
//    gc_debug('===check stacks after compact/pop_registers');
//    check_stacks(false);

        after = [];
        let e = state.E;
        let envsize = state.CP.code[state.CP.offset - 1];
        while (e !== HEAP_SIZE) {
            for (let i = 0; i < envsize; i++) {
                after.push(record_term(memory[e + 2 + i]));
            }
            let envcp = memory[e + 1];
            envsize = envcp.code[envcp.offset - 1];
            e = memory[e];
        }
    }
    if (total_marked !== 0)
    {
    }
    if(display_gc) {
        while (before.length !== 0) {
            let a = before.pop();
            let b = after.pop();
            let at = recall_term(a, {});
            let bt = recall_term(b, {});
            if (!predicate_unify(at, bt)) {
                gc_main_debug("Error: Terms in environment changed during GC!");
                gc_main_debug("at = " + term_to_string(at));
                gc_main_debug("bt = " + term_to_string(bt));
                abort("false");
            }
        }


        // display_gc_log();
    }
    let msgOut = "After GC, heap is " + state.H;
    //stdout( msgOut + '\n');
    gc_main_debug(msgOut);

    return true;
}

function push_registers()
{
    for (var i = 0; i < state.num_of_args; i++)
    {
        memory[state.TR++] = register[i];
    }
}

function pop_registers()
{
    for (var i = state.num_of_args-1; i >= 0; i--)
    {
        register[i] = memory[--state.TR];
    }
}

function sweep_trail()
{
    for (var current = state.TR-1; current >= HEAP_SIZE + STACK_SIZE; current--)
    {
        if (IS_HEAP_PTR(memory[current]))
        {
            debug_msg("into_relocation_chain(" + VAL(memory[current]) + ", " + current + ")");
            into_relocation_chain(VAL(memory[current]), current);
        }
        else
        {
            debug_msg("Not a heap pointer!");
        }
    }
}

function sweep_stack()
{
    sweep_environments(state.E, state.CP.code[state.CP.offset - 1]);
    debug_msg("Environments swept... " + hex(memory[0]));
    sweep_choicepoints();
    debug_msg("Choicepoints swept");
}

function sweep_environments(e, envsize)
{
    while (e !== HEAP_SIZE)
    {
        // Traversing backwards to ensure we do not stop prematurely
        debug_msg("Environment is " + e + " and initially envcp is " + memory[e+1] + " environment has " + envsize + " slots");
        for (var y = envsize-1; y >= 0; y--)
        {
            if (IS_HEAP_PTR(memory[e+2 + y]))
            {
                if ((memory[e+2 + y] & M_BIT) === 0)
                {
                    // we have already swept this chain
                    debug_msg("Already swept this environment, since M_BIT is not set at " + (e+2+y) + " = " + hex(memory[e+2+y]));
                    return;
                }
                else 
                {
                    memory[e+2 + y] &= ~M_BIT;
                    debug_msg("Adding slot Y" + y + " (at " + (e+2+y) + ") to relocation chain. Present value is: " + hex(memory[e+2+y]));
                    into_relocation_chain(VAL(memory[e+2+y]), e+2+y);
                }
            }
        }
        var envcp = memory[e+1];
        debug_msg("envcp is at " + (e+1) +" and equals " + envcp);
        // work out the size of the previous environment, using the CP pointer saved in THIS environment.
        // This is why we had to pass size in to mark_environments()
        envsize = envcp.code[envcp.offset-1];
        e = memory[e];
    }
}

function sweep_choicepoints()
{
    var b = state.B;
    while (b !== 0)
    {
        var cpcp = memory[b + memory[b] + 2];        
        var envsize = cpcp.code[cpcp.offset-1];
        sweep_environments(memory[b + memory[b] + 1], envsize);
        for (var y = 0; y < memory[b]; y++)
        {
            if (IS_HEAP_PTR(memory[b+y+1]))
            {
                debug_msg("Adding choicepoint value into relocation chain");
                memory[b+y+1] &= ~M_BIT;
                into_relocation_chain(VAL(memory[b+y+1]), b+y+1);
            }
        }
        if ((memory[memory[b + memory[b] + 6]] & M_BIT) === 0)
        {
            // The choicepoint has a saved value for H (ie HB) which is not marked
            // Make a fake atom on the heap and change the HB to point to it
            memory[memory[b + memory[b] + 6]] = NIL ^ (M_BIT);
            total_marked++;
        }
        debug_msg("Adding HB into relocation chain... " + hex(memory[0]));
        into_relocation_chain(memory[b + memory[b] + 6], b + memory[b] + 6);
        b = memory[b + memory[b] + 3];
    }
}

function mark()
{
    mark_registers();
    debug_msg("Registers done: " + total_marked);
    mark_environments(state.E, state.CP.code[state.CP.offset - 1]);
    debug_msg("Env done"  + total_marked);
    mark_choicepoints();
    debug_msg("Choicepoints done " + total_marked);
}

function compact()
{
    var dest;
    var current;
    dest = total_marked - 1;
    gc_debug('before compact upward');
    // Upward
    for (current = state.H-1; current >= 0; current--)
    {
        if ((memory[current] & M_BIT) === M_BIT)
        {
            //gc_debug('compact - before update_relocation_chain('+current+', '+dest+')')
            update_relocation_chain(current, dest);
            if (IS_HEAP_PTR(memory[current]))
            {
                if (VAL(memory[current]) < current)
                {
                    //gc_debug('compact - after update_relocation_chain, before into_relocation_chain('+VAL(memory[current])+', '+current+')')
                    into_relocation_chain(VAL(memory[current]), current);
                }
                else if (VAL(memory[current]) === current)
                {
                    memory[current] = (memory[current] & NV_MASK) ^ dest;
                }
            }
            dest--;
        }
    }

    gc_debug('after compact upward, before downward');
    // Downward
    dest = 0;
    for (current = 0; current < state.H; current++)
    {
        if ((memory[current] & M_BIT) === M_BIT)
        {
            update_relocation_chain(current, dest);
            if (IS_HEAP_PTR(memory[current]) && VAL(memory[current]) > current)
            {
                into_relocation_chain(VAL(memory[current]), dest);

                memory[dest] = VAL(memory[dest]) ^ (TAG(memory[current]) << WORD_BITS);
            }
            else
            {
                memory[dest] = memory[current];
                // clear the GC flags
                memory[dest] &= ~F_BIT;
            }
            memory[dest] &= ~M_BIT;
            dest++;
        }
    }
    gc_debug('after compact downward');
}

function update_relocation_chain(current, dest)
{
    let localDebug = false; //(dest === 20);

    if(localDebug) {
        gc_debug('start update_relocation_chain(' + current + ', ' + dest + ')');
    }

    var j;
    while ((memory[current] & F_BIT) === F_BIT)
    {
        j = VAL(memory[current]);
        if(localDebug) {
            gc_debug('update_relocation_chain - memory[current]=' + memory[current] + ', j=' + j);
            gc_debug('memory[j]=' + memory[j] + ', VAL(memory[j])=' + VAL(memory[j]));
            gc_debug('(memory[current] & (NV_MASK ^ F_BIT))=' + (memory[current] & (NV_MASK ^ F_BIT))
                + ', (VAL(memory[j]) ^ (memory[current] & (NV_MASK ^ F_BIT)))=' + (VAL(memory[j]) ^ (memory[current] & (NV_MASK ^ F_BIT)))
                + ', (memory[j] & F_BIT)=' + (memory[j] & F_BIT));
        }
        memory[current] = (VAL(memory[j]) ^ (memory[current] & (NV_MASK ^ F_BIT))) | (memory[j] & F_BIT);
        if(localDebug) {
            gc_debug('new memory[current]=' + memory[current]);
            gc_debug('dest ^ (memory[j] & NV_MASK)='+(dest ^ (memory[j] & NV_MASK)));
        }
        memory[j] = dest ^ (memory[j] & NV_MASK);
        if(localDebug) {
            gc_debug('new memory[j]=' + memory[j]);
        }
        memory[j] &= ~F_BIT;
        if(localDebug) {
            gc_debug('new new memory[j]= (new memory[j] &= ~F_BIT)' + memory[j]);
        }
    }
}

function into_relocation_chain(j, current)
{
    memory[current] = VAL(memory[j]) ^ (memory[current] & (NV_MASK ^ F_BIT)) | (memory[j] & F_BIT);
    memory[j] = current ^ (memory[j] & NV_MASK);
    memory[j] |= F_BIT;        
}

/**
 * @return {boolean}
 */
function IS_HEAP_PTR(x)
{
    var tag = TAG(x);
    return (tag === TAG_STR || tag === TAG_LST || tag === TAG_REF) && (VAL(x) < HEAP_SIZE);
}

// Mark all the cells reachable from the registers as reachable (ie set their M bits)
function mark_registers()
{
    for (var i = 0; i < state.num_of_args; i++)
    {
        if (IS_HEAP_PTR(register[i]))
        {
            // register refers to the heap. We have to temporarily put this onto the heap since mark_variable
            // expects an address (ie an index into memory[]) and register[i]
            var tmp = state.H;
            if (state.H === HEAP_SIZE)
                abort("Out of heap during GC");
            memory[state.H++] = register[i];
            mark_variable(tmp);
            state.H--; // We can just clean it up now, since mark_variable is not allowed to write to memory[]
        }
    }
}

// Mark all the cells reachable from the environment 'initial'.
// Note that this takes into account LCO: Trimmed cells are ignored.
// If these are actually needed, mark_choicepoints() will find them
function mark_environments(initial, envsize)
{
    var e = initial;
    while (e !== HEAP_SIZE)
    {
        debug_msg("Marking environment " + e + " which has " + envsize + " slots");
        // Traversing backwards to ensure we do not stop prematurely
        for (var y = envsize-1; y >= 0; y--)
        {
            if ((memory[e+2 + y] & M_BIT) === M_BIT)
            {
                // we have already done this chain
                debug_msg("Slot is already marked. Stopping marking");
                return;
            }
            else if (IS_HEAP_PTR(memory[e+2 + y]))
            {
                // Y-register refers to the heap
                debug_msg("Marking environment slot " + y + " = " + hex(memory[e+2+y]) + " (" + term_to_string(memory[e+2+y]) + ")");
                mark_variable(e+2 + y);
                debug_msg("###memory[" + (e+2+y) + "] = " + hex(memory[e+2+y]));
            }
            else
            {
                debug_msg("Is not a heap ptr: " + hex(memory[e+2+y]));
            }
        }
        var envcp = memory[e+1];
        // work out the size of the previous environment, using the CP pointer saved in THIS environment.
        // This is why we had to pass size in to mark_environments()
        debug_msg("e->CE is " + memory[e]);
        debug_msg("e->CP is at " + (e+1) + " and is " + envcp);
        envsize = envcp.code[envcp.offset-1];
        e = memory[e];
    }
}

function mark_choicepoints()
{
    var b = state.B;
    while (b !== 0)
    {
        var cpcp = memory[b + memory[b] + 2];
        var envsize = cpcp.code[cpcp.offset-1];
        mark_environments(memory[b + memory[b] + 1], envsize);
        for (var y = 0; y < memory[b]; y++)
        {
            if (IS_HEAP_PTR(memory[b+y+1]))
            {
                // Y-register refers to the heap
                debug_msg("Marking B value " + (b+y+1));
                mark_variable(b + y + 1);
            }
        }
        b = memory[b + memory[b] + 3];
    }
}

var total_marked = 0;

// start is an address: That is, an index into memory[]. It is NOT a cell, so it does NOT have a tag!
// Also, it must be the address of something which is a pointer. That is, VAL(memory[start]) must be another index into memory[].
function mark_variable(start)
{
    debug_msg("\nMarking: " + start);
    let current = start;
    let next = VAL(memory[current]);
    memory[current] |= F_BIT;
    debug_msg("Set F on " + current);
    // mark_variable is always called with something which is either not on the heap
    // or not /really/ on the heap, in the case of register values. Therefore, when we count
    // the first thing, we should increment total_marked to 0, not 1.
    total_marked--;

    while(true) // unwrap goto into while loops
    {
        while (true) // forward
        {
            debug_msg("Forward. (" + current + ", " + next + ")");
            if ((memory[current] & M_BIT) === M_BIT)
                break; // goto backward
            debug_msg("Set M on " + current);
            memory[current] |= M_BIT;
            total_marked++;
            debug_msg("Total marked is now " + total_marked);
            switch(TAG(memory[current]))
            {
            case TAG_REF: // Transformation 1
                if ((memory[next] & F_BIT) === F_BIT)
                {
                    break; // goto backward
                }
                // REVERSE(current, next);
                debug_msg("REVERSE(" + current + ", " + next + ")");
            {
                let temp = VAL(memory[next]);

                memory[next] = (memory[next] & NV_MASK) ^ current;
                current = next;
                next = temp;
            }
                continue; // goto forward
            case TAG_STR: // Transform 2a
            case TAG_LST: // Transform 2b
                if ((memory[next+1] & F_BIT) === F_BIT)
                    break; // goto backward
                // Optimisation: We can skip the structure if we have already marked all its arguments
                // FIXME: Implement

                if (TAG(memory[current]) === TAG_STR)
                {
                    var i;
                    for (i = 0; i < ftable[VAL(memory[next])][1]; i++)
                    {
                        debug_msg("Set F on " + (next+1+i));
                        memory[next+1+i] |= F_BIT;
                    }
                    next = next+i;
                }
                else
                {
                    debug_msg("Set F on " + (next+1));
                    memory[next+1] |= F_BIT;
                    next = next+1;
                }
                debug_msg("REVERSE(" + current + ", " + next + ")");
                //REVERSE(current, next);
            {
                let temp = VAL(memory[next]);
                memory[next] = (memory[next] & NV_MASK) ^ current;
                current = next;
                next = temp;
            }
                continue; // goto forward
            default:
                // All other types: INT, ATM, FLT, etc
                // Transformation 3
                break; // goto backward
            }
            break; // if we get to the end of forward, we must be wanting to go to backward
        }
        while (true) // backward
        {
            debug_msg("Backward (" + current + ", " + next + ")");
            if ((memory[current] & F_BIT) !== F_BIT)
            {                
                // current is an internal cell
                // Transformation 4
                //UNDO(current, next);
                debug_msg("UNDO(" + current + ", " + next + ")");
                var temp = VAL(memory[current]);
                //var tag = TAG(memory[next]);
                memory[current] = (memory[current] & NV_MASK) ^ next;
                next = current;
                current = temp;
                continue; // goto backward
            }
            // current is the head of a chain
            debug_msg("Unset F on " + current);
            memory[current] &= ~F_BIT;
            if (current === start)
            {
                // current is the head of the chain we started with. Finished!
                return;
            }
            // Otherwise, current is the head of a subchain
            current--; // Transformation 5
            //ADVANCE(current, next);
            debug_msg("ADVANCE(" + current + ", " + next + ")");
            {
                let temp = VAL(memory[current + 1]);
                memory[current + 1] = (memory[current + 1] & NV_MASK) ^ next;
                next = VAL(memory[current]);
                memory[current] = (memory[current] & NV_MASK) ^ temp;
            }
            break; // goto forward
        }
    }
}



function gc_test(d)
{
    debugging = d;
    load_state();
    initialize();
    stdout("Loaded " + Object.keys(predicates).length + " predicates");
    stdout("Loaded " + atable.length + " atoms");
    stdout("Loaded " + ftable.length + " functors");
    stdout("Loaded " + code.length + " bytes of code");
    call_directives();

    memory[0] = 0x20000088;
    memory[1] = 0x20000071;
    memory[2] = 0x20000072;
    state.H = 3;
    state.CP.code[state.CP.offset - 1] = 1;
    memory[state.E + 2] = 0x8000000;
    debug_msg("Y0 = " + hex(memory[state.E+2]));
    debug_msg("    -> " + term_to_string(memory[state.E+2]));
    mark_variable(state.E+2);
    debug_msg("Marked " + total_marked);

    compact();
    debug_msg("Y0 = " + hex(memory[state.E+2]));
    debug_msg("    -> " + term_to_string(memory[state.E+2]));
}

function dump_heap()
{
    debug_msg("Heap:-----------------------");
    for (var i = 0; i < state.H; i++)
    {
        debug_msg(i + ": " + hex(memory[i]));
    }
    debug_msg("----------------------------");
}

function dump_registers()
{
    debug_msg("Registers:------------------");    
    for (var i = 0; i < state.num_of_args; i++)
    {
        debug_msg(i + ": " + hex(register[i]) + " => " + term_to_string(register[i]));
    }
    debug_msg("----------------------------");
}


function predicate_statistics()
{
    let aggregateDuration = statistics_wam_duration();
    let heapSize = statistics_heap_size();
    let maxHeap = statistics_max_heap();
    let maxStack = statistics_max_stack();
    let stackPortion = maxStack - HEAP_SIZE;

    stdout("WAM duration: " + aggregateDuration + "\n");
    stdout("Current heap: " + heapSize + "\n");
    stdout("Max heap: " + maxHeap + " (words of " + HEAP_SIZE + " limit)\n");
    stdout("Max stack: " + stackPortion + " (words of " + STACK_SIZE + " limit, " + maxStack + " absolute)\n");
    return true;
}

function predicate_wam_duration(duration) {
    var aggregateDuration = statistics_wam_duration();

    return unify_number(aggregateDuration, duration);
}

function statistics_wam_duration() {
    var activeWamDuration = 0;

    if(activeWamStartTime) {
        var now = Date.now();
        activeWamDuration = now - activeWamStartTime;
    }

    return activeWamDuration + wamDuration; // wamDuration may be float due to 0.1 fudge values being added.
}

function predicate_statistics_heap_size(size) {
    var heapSize = statistics_heap_size();

    return unify_number(heapSize, size);
}

function statistics_heap_size() {
    return state.H;
}

function predicate_statistics_max_stack(maxStackPL) {
    var maxStackJS = statistics_max_stack();

    return unify_number(maxStackJS, maxStackPL);
}

function statistics_max_stack() {
    return maxStackSize;
}

function predicate_statistics_max_heap(maxHeapPL) {
    var maxHeapJS = statistics_max_heap();

    return unify_number(maxHeapJS, maxHeapPL);
}

function statistics_max_heap() {
    return maxHeapSize;
}

function unify_number(numberJS, numberPL) {
    if(Number.isInteger(numberJS) && numberJS <= (2**(WORD_BITS-1)-1) && numberJS >= -1*(2**(WORD_BITS-1)-1)) {
        return PL_unify_integer(numberPL, numberJS);
    } else {
        return PL_unify_float(numberPL, numberJS);
    }
}

function check_stacks(m)
{
    gc_debug("Checking stacks " + m);
    check_environments(state.E, state.CP.code[state.CP.offset - 1], m);
    gc_debug("Stacks OK");
}

function check_environments(initial, envsize, m)
{
    var e = initial;
    while (e !== HEAP_SIZE)
    {
        // Traversing backwards to ensure we do not stop prematurely
        gc_debug("Checking environment " + e);
        for (var y = 0; y < envsize; y++)
        {            
            if (TAG(memory[e+2+y]) === TAG_STR ||
                TAG(memory[e+2+y]) === TAG_LST)
            {
                gc_debug("Checking Y" + y);
                check_term(memory[e+2+y], m);
            }
            else 
            {
                gc_debug("Y" + y + " is not a heap pointer");
            }
            // Otherwise we do not need to check it if it is in the environment
        }
        var envcp = memory[e+1];
        // work out the size of the previous environment, using the CP pointer saved in THIS environment.
        // This is why we had to pass size in to mark_environments()
        envsize = envcp.code[envcp.offset-1];
        e = memory[e];
    }
}

function check_term(t, m)
{
    gc_debug("Checking " + hex(t));
    if (!m)
    {
        gc_debug(" == " + term_to_string(t));
    }
    if ((t & M_BIT) === M_BIT)
    {
        if (!m)
            abort("Term " + hex(t) + " is marked but should not be");
    }
    else if (m)
    {
        abort("Term " + hex(t) + " is not marked but is reachable");
    }
    if ((t & F_BIT) === F_BIT)
    {
        if (!m)
            abort("Term " + hex(t) + " is F but should not be");
    }

    if (TAG(t) === TAG_LST)
    {
        if (VAL(t) > state.H)
            abort("Term " + hex(t) + " exceeds heap: " + state.H);
        check_term(memory[VAL(t)], m);
        check_term(memory[VAL(t)+1], m);
    }
    else if (TAG(t) === TAG_STR)
    {
        if (VAL(t) > state.H)
            abort("Term " + hex(t) + " exceeds heap: " + state.H);        
        if (ftable[VAL(memory[VAL(t)])] === undefined)
            abort("Illegal functor " + VAL(memory[VAL(t)]));
        var arity = ftable[VAL(memory[VAL(t)])][1];
        for (var i = 0; i < arity; i++)
            check_term(memory[VAL(t)+1+i], m);
    }
    // Everything else we assume is OK
}

let gc_log = [];

// gc_log is a circular log of that last 95 gc_debug(msg) calls.

function add_to_gc_log(msg) {
    if(gc_log.length === 96) {
        gc_log.shift();
    } else if(gc_log.length > 96) {
        let shift = gc_log.length - 95;
        gc_log = gc_log.slice(shift);
    }
    gc_log.push(msg);
}

function gc_main_debug(msg) {
    if(display_gc) {
        alert('gc: ' + msg);
//        display_backtrack = true;
    }
//    add_to_gc_log(msg);
}

function gc_debug(msg) {
    if(display_minor_gc) {
        alert('gc: ' + msg);
//        display_backtrack = true;
    }
//    add_to_gc_log(msg);
}

function display_gc_log() {
    // for(let msg of gc_log) {
    //     stdout('gc_debug: ' + msg + '\n');
    // }
}

function dump_environments(initial, envsize, cp)
{
    if(! initial){
        dump_environments(state.E, state.CP.code[state.CP.offset - 1], state.CP);
        return;
    }

    let e = initial;
    let envcp = cp;
    while (e !== HEAP_SIZE)
    {
        let predicate = functor_offset_to_predicate_string(envcp.predicate.key);

        gcWrite('environment at ' + e + ' has ' + envsize + ' slots. predicate=' + predicate + ', cp.predicate.key=' + envcp.predicate.key + ', cp.offset=' + envcp.offset);
        envcp = memory[e+1];
        // work out the size of the previous environment, using the CP pointer saved in THIS environment.
        // This is why we had to pass size in to mark_environments()
        envsize = envcp.code[envcp.offset-1];
        e = memory[e];
    }
}

function functor_offset_to_predicate_string(ftor) {
    let functorPair = ftable[ftor];
    let predicate;
    if(functorPair) {
        let predicateName = atable[functorPair[0]];
        let predicateArity = functorPair[1];
        predicate = predicateName + '/' + predicateArity;
    }
    return predicate;
}

function dump_choicepoints()
{
    let b = state.B;
    while (b !== 0)
    {
        b = dump_choicepoint(b);
    }
}

function dump_choicepoint(b) {
    let dynamicPortionSize = memory[b];

    let backtrack_cp = memory[b + dynamicPortionSize + CP_CP];
    var backtrack_envsize = backtrack_cp.code[backtrack_cp.offset-1];
    //dump_environments(memory[b + dynamicPortionSize + CP_E], envsize, backtrack_cp);
    let backtrack_predicate = functor_offset_to_predicate_string(backtrack_cp.predicate.key);
    let backtrack_offset = backtrack_cp.offset;

    let choice_cp = memory[b + dynamicPortionSize + CP_Next];
    var choice_envsize = choice_cp.code[choice_cp.offset-1];
    let choice_predicate = functor_offset_to_predicate_string(choice_cp.predicate.key);
    let choice_offset = choice_cp.offset;

    gcWrite('choicepoint at ' + b + ' has ' + dynamicPortionSize + ' dynamic slots. choice predicate=' + choice_predicate + ' (offset ' + choice_offset + '), backtrack predicate=' + backtrack_predicate + ' (offset ' + backtrack_offset + '), backtrack environment=' + memory[b + dynamicPortionSize + CP_E]);

    return memory[b + dynamicPortionSize + CP_B];
}

function gcWrite(msg) {
    if(gc_environment === 'console') {
        print(msg);
    } else if(gc_environment === 'browser') {
        console.log(msg);
    }
}