function dump() {
    load_state();
    for(var ofst = 0;ofst < ftable.length;ofst++){
        var functionPair = ftable[ofst];
        var predicateName = atable[functionPair[0]];
        print(predicateName + '/' + functionPair[1] + ': ftor=' + ofst + ', atable ofst=' + functionPair[0]);
    }
}

function dumpPredicate(targetPredicateName, targetArity) {
    load_state();
    for(var ofst = 0;ofst < ftable.length;ofst++){
        var functionPair = ftable[ofst];
        var predicateName = atable[functionPair[0]];
        if(predicateName === targetPredicateName && (! targetArity || functionPair[1] === targetArity)) {
            print(predicateName + '/' + functionPair[1]);
            for(let clauseKey of predicates[ofst].clause_keys) {
                print('---');
                print('Clause ' + clauseKey);
                print(' ');
                let clause = predicates[ofst].clauses[clauseKey];
                code = clause.code;
                let position = 0;
                while(position < code.length) {
                    let decoded = decode_instruction({key:ofst},position);
                    print(decoded.string);
                    position += decoded.size;
                }
            }
            return;
        }
    }
}