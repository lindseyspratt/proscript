"use strict";

function node_goal(state, query) {
    let fs = require('fs');
    let path = require('path');

// append engine.js and state.js (args[0])
    // find proscriptls/dist/foo given workingdir containing proscriptls segment or not.
    // if workingdir contains proscriptls or proscriptls_sdk*, '.../proscriptls/...', then combine '.../proscriptls/' and 'foo'
    // if workingdir does not contain proscriptls or proscriptls_sdk* then assume 'foo' is in same dir.

    let engine = proscriptls_path('dist/proscriptls_engine.js');
    state = path.resolve(state);
    let standalone = proscriptls_path('node_tools/node_standalone.js');
    let exportsFile = proscriptls_path('node_tools/node_exports_init.js');

    let contents = [engine, standalone, exportsFile, state].map(function (file) {
        return fs.readFileSync(file, 'utf8');
    });

    let content = contents.join('\n');
    let file = path.resolve('temp_for_node_goal.js');
    fs.writeFileSync(file, content);

    const pls = require(file);

    pls.proscriptls_init(query + ".");

    fs.unlinkSync(file);
}

function node_toplevel(state) {
    let fs = require('fs');
    let path = require('path');

// append engine.js and state.js (args[0])

    let engine = proscriptls_path('dist/proscriptls_engine.js');
    state = path.resolve(state);
    let standalone = proscriptls_path('node_tools/node_standalone.js');
    let exportsFile = proscriptls_path('node_tools/node_exports_init.js');

    let contents = [engine, standalone, exportsFile, state].map(function (file) {
        return fs.readFileSync(file, 'utf8');
    });

    let content = contents.join('\n');
    let file = path.resolve('temp_for_node_goal.js');
    fs.writeFileSync(file, content);

    const pls = require(file);

    pls.proscriptls_toplevel();

    fs.unlinkSync(file);
}

function proscriptls_path(input) {
    let path = require('path');
    // find proscriptls/dist/foo given workingdir containing proscriptls segment or not.
    // if workingdir contains proscriptls or proscriptls_sdk*, e.g. '.../proscriptls/...', then combine '.../proscriptls/' and 'foo'
    // if workingdir does not contain proscriptls or proscriptls_sdk* then assume 'foo' is in same dir.
    let wd = __dirname; //process.cwd();

    let testSegment;
    if (wd.includes('/proscriptls/')) {
        testSegment = 'proscriptls';
    } else if (wd.includes('/proscriptls_sdk')) {
        testSegment = 'proscriptls_sdk';
    } else {
        return path.resolve(wd, input);
    }

    let x = wd;
    let y = '';
    while (! y.startsWith(testSegment)) {
        let next = path.dirname(x);
        y = path.basename(x);
        x = next;
    }
    x = path.join(x, y);
    return path.resolve(x, input);

}

module.exports = {node_goal: node_goal, node_toplevel: node_toplevel, proscriptls_path: proscriptls_path};
