function dump() {
    load_state();
    for(var ofst = 0;ofst < ftable.length;ofst++){
        var functionPair = ftable[ofst];
        var predicateName = atable[functionPair[0]];
        print(predicateName + '/' + functionPair[1]);
    }
}