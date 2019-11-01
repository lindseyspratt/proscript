let environment =  'jsc'; // 'browser';

function dump(filter) {
    if(environment === 'console') {
        load_state();
    }
    for(var ofst = 0;ofst < ftable.length;ofst++) {
        var functionPair = ftable[ofst];
        var predicateName = atable[functionPair[0]];
        let predicate = predicates[ofst];
        if (!filter || (filter && ((filter === 'defined-predicate' && predicate)
            || (filter === 'undefined-predicate' && !predicate)))) {
            dumpWrite(predicateName + '/' + functionPair[1] + ': ftor=' + ofst + ', atable ofst=' + functionPair[0] + ', ' + (predicate ? 'has' : 'no') + ' predicate clauses');
        }
    }
}

function dumpPredicate(targetPredicateName, targetArity) {
    if(environment === 'console') {
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
    if((!mode && environment === 'console')
    || (mode && mode === 'load')) {
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
    if((!mode && environment === 'console')
        || (mode && mode === 'load')) {
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