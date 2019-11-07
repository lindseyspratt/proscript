function dump(filter, mode) {
    if(mode && mode === 'load') {
        load_state();
    }

    let indexedCount = 0;
    let fullyIndexedCount = 0;
    let infos = [
        {ofst: 2, label: 'CA', bit: 1},
        {ofst: 3, label: 'CI', bit: 2},
        {ofst: 4, label: 'CF', bit: 4},
        {ofst: 5, label: 'L', bit: 8},
        {ofst: 6, label: 'S', bit: 16}
        ];
    let counts = {};

    for(let key of Object.keys(predicates)) {
        let predicate = predicates[key];
        if(typeof predicate.index !== 'undefined') {
            indexedCount++;

            let sequenceCount = 0;
            for(let clauseKey of predicate.clause_keys) {
                if(clauseKey !== predicate.index) {
                    let clause = predicate.clauses[predicate.clause_keys[clauseKey]];
                    let code = clause.code;
                    if (code[0] === 30 || code[0] === 254) {
                        // trust_me or nop2 instruction
                        // trust_me indicates a sequence of 2 or more clauses,
                        // nop2 indicates a sequence of one clause.
                        sequenceCount++;
                    }
                }
            }

            if(sequenceCount === 1) {
                fullyIndexedCount++;

                // inspect switch_on_term instruction.
                let clause = predicate.clauses[predicate.clause_keys[predicate.index]];
                let code = clause.code;

                // switch_on_term: [44, V, CA, CI, CF, L, S]

                let codePosition = 2; //first two slots are for nop2 or try_me_else.

                let V = code[codePosition + 1];
                let maskName = '';
                let maskBits = 0;

                for (let info of infos) {
                    info.value = decode_address(code[codePosition + info.ofst]);
                    if (info.value !== 'fail') {
                        maskName += ((maskName !== '') ? '/' : '') + info.label;
                        maskBits = maskBits | info.bit;
                    }
                }

                if (counts[maskBits]) {
                    counts[maskBits].counter++;
                } else {
                    counts[maskBits] = {name: maskName, counter: 1};
                }
            }
        }
    }

    dumpWrite("Loaded " + Object.keys(predicates).length + " predicates");
    dumpWrite("    " + indexedCount + " indexed, " + fullyIndexedCount + " single sequence");

    let pad = '        ';

    dumpWrite("    single sequence types:");
    for(let countKey of Object.keys(counts)) {
        let count = counts[countKey];
        dumpWrite(pad + count.name + ': ' + count.counter);
    }

    dumpWrite("Loaded " + atable.length + " atoms");
    dumpWrite("Loaded " + ftable.length + " functors");

    for(var ofst = 0;ofst < ftable.length;ofst++) {
        var functionPair = ftable[ofst];
        var predicateName = atable[functionPair[0]];
        let predicate = predicates[ofst];
        if (!filter ||
            (filter && ((filter === 'defined-predicate' && predicate)
                || (filter && filter === 'indexed-predicate' && predicate && predicate.index)
                || (filter === 'undefined-predicate' && !predicate)))) {
            dumpWrite(predicateName + '/' + functionPair[1] + ': ftor=' + ofst + ', atable ofst=' + functionPair[0] +
                ', ' + (predicate ?
                    ('has' + (predicate.index ? ' indexed' : ''))
                    : 'no') + ' predicate clauses');
        }
    }

}

function dumpPredicate(targetPredicateName, targetArity, mode) {
    if(mode && mode === 'load') {
        load_state();
    }

    for(var ofst = 0;ofst < ftable.length;ofst++){
        var functionPair = ftable[ofst];
        var predicateName = atable[functionPair[0]];
        if(predicateName === targetPredicateName && (! targetArity || functionPair[1] === targetArity)) {
            dumpWrite(predicateName + '/' + functionPair[1]);
            for(let clauseKey of predicates[ofst].clause_keys) {
                dumpWrite('---');
                dumpWrite('Clause ' + clauseKey);
                dumpWrite(' ');
                let clause = predicates[ofst].clauses[clauseKey];
                code = clause.code;
                let position = 0;
                while(position < code.length) {
                    let decoded = decode_instruction({key:ofst},position);
                    dumpWrite(decoded.string);
                    position += decoded.size;
                }
            }
            return;
        }
    }
}

function danglingPredicates(mode) {
    if(mode && mode === 'load') {
        load_state();
    }

//    dumpWrite('Dangling predicates:');

    let dangles = [];


    for(let predicateKey of Object.keys(predicates)) {
        let predicate = predicates[predicateKey];
        for(let clauseKey of predicate.clause_keys) {
            let clause = predicate.clauses[clauseKey];
            code = clause.code;
            let position = 0;
             while(position < code.length) {
                let decoded = decode_instruction({key:predicateKey},position);
                if(decoded.goalPredicate !== 'none' && ! predicates[decoded.goalPredicate.predicate]
                 && ! foreign_predicates[decoded.goalPredicate.predicate]) {
                    let references = dangles[decoded.goalPredicate.predicate];
                    if(typeof references === 'undefined') {
                        references = [];
                        dangles[decoded.goalPredicate.predicate] = references;
                    }
                    references.push({goalPredicate: decoded.goalPredicate, callingPredicate: predicateKey});
                }
                position += decoded.size;
            }

        }
    }

    for(let moduleID of Object.keys(module_exports)) {
        let moduleExports = module_exports[moduleID];
        let moduleName = atable[moduleID];
        let importedModules = module_imports[moduleID];

        for(let nameIDArity of moduleExports) {
            let nameID = nameIDArity[0];
            let arity = nameIDArity[1];
            let name = atable[nameID];
            let qualifiedName = moduleName + ':' + name;
            let predicateIDPL = lookup_functor(qualifiedName, arity);
            let predicateID = VAL(predicateIDPL);
            if(! predicates[predicateID]
                && ! foreign_predicates[predicateID]) {
                let found = false;
                for(let importedModule of importedModules) {
                    for( let pair of module_exports[importedModule]) {
                        if(pair[0] === nameID && pair[1] === arity) {
                            found = true;
                            break;
                        }
                    }
                }

                if(! found) {
                    let references = dangles[predicateID];
                    if (typeof references === 'undefined') {
                        references = [];
                        dangles[predicateID] = references;
                    }
                    references.push({export: moduleName});
                }
            }
        }
    }

    for(let predicateKey of Object.keys(dangles)){
        var functionPair = ftable[predicateKey];
        var predicateName = atable[functionPair[0]];
            dumpWrite('Undefined predicate: ' + predicateName + '/' + functionPair[1] + ' called by ');
            let references = dangles[predicateKey];
            for(let reference of references) {
                if(reference.callingPredicate) {
                    let caller = reference.callingPredicate;
                    let callerFunctionPair = ftable[caller];
                    let callerPredicateName = atable[callerFunctionPair[0]];
                    dumpWrite(' ' + callerPredicateName + '/' + callerFunctionPair[1]);
                } else {
                    dumpWrite(' exported by ' + reference.export);
                }
            }
            dumpWrite('\n');
     }
}

function unusedPredicates(mode) {
    if(mode && mode === 'load') {
        load_state();
    }

    let used = [];

    let retractPredicateID = VAL(lookup_functor('bootstrap_js:retract', 1));

    for(let predicateKey of Object.keys(predicates)) {
        let predicate = predicates[predicateKey];
        for(let clauseKey of predicate.clause_keys) {
            let clause = predicate.clauses[clauseKey];
            code = clause.code;
            let position = 0;
            let opCodePositions = [];
            while(position < code.length) {
                opCodePositions.push(position);
                let decoded = decode_instruction({key:predicateKey},position);
                if(decoded.goalPredicate !== 'none' && ! used.includes(decoded.goalPredicate.predicate) ) {

                    // let pair = ftable[decoded.goalPredicate.predicate];
                    // let qualifiedName = atable[pair[0]];
                    // let arity = pair[1];
                    // let moduleName = qualifiedName.substring(0, qualifiedName.indexOf(":"));

                    used.push(decoded.goalPredicate.predicate);
                }

                if(decoded.goalPredicate !== 'none' && decoded.goalPredicate.predicate === retractPredicateID ) {
                    let result = decode_retract_argument(position, opCodePositions);
                    if (result.ftor) {
                        let pair = ftable[result.ftor];
                        let baseName = atable[pair[0]];
                        let arity = pair[1];

                        let pushFtor;

                        if(baseName.includes(':')) {
                            pushFtor = result.ftor;
                        } else {
                            let qualifiedName = atable[result.module] + ':' + baseName;
                            pushFtor = VAL(lookup_functor(qualifiedName, arity));
                        }
                        used.push(pushFtor);
                    }
                }
                position += decoded.size;
            }
        }
    }

    for(let moduleID of Object.keys(module_exports)) {
        let moduleExports = module_exports[moduleID];
        let moduleName = atable[moduleID];
        for(let nameIDArity of moduleExports) {
            let nameID = nameIDArity[0];
            let arity = nameIDArity[1];
            let name = atable[nameID];
            let qualifiedName = moduleName + ':' + name;
            let predicateIDPL = lookup_functor(qualifiedName, arity);
            let predicateID = VAL(predicateIDPL);
            if(! used.includes(predicateID)) {
                used.push(predicateID);
            }
        }
    }

    for(let predicateID of system) {
        if(! used.includes(predicateID)) {
            used.push(predicateID);
        }
    }

    for(let predicateID of initialization) {
        if(! used.includes(predicateID)) {
            used.push(predicateID);
        }
    }

    let unused = [];
    for(let predicateKey of Object.keys(predicates)) {
        let predicateID = parseInt(predicateKey);
        if(! used.includes(predicateID)) {
            let pair = ftable[predicateID];
            let qualifiedName = atable[pair[0]];
            //let arity = pair[1];
            let moduleName = qualifiedName.substring(0, qualifiedName.indexOf(":"));
            if(moduleName !== 'user') {
                unused.push(predicateID);
            }
        }
    }

    if(unused.length > 0) {
        let plural = unused.length === 1 ? '' : 's';

        dumpWrite('Unused predicate' + plural + ':');

        for (let predicateID of unused) {
            let pair = ftable[predicateID];
            let predicateName = atable[pair[0]];
            dumpWrite(predicateID + ' <- ' + predicateName + '/' + pair[1]);
        }
    }
}

function dumpWrite(msg) {
    if(typeof console === 'object' && typeof console.log === 'function') {
        console.log(msg);
    } else if(typeof print === 'function') {
        print(msg);
    }
}