"use strict";

const ng = require('./node_goal_module.js');

let args = process.argv.slice(2);

ng.node_goal(ng.proscriptls_path('dist/proscriptls_state.js'), "set_indexing_mode(basic), wam_compiler:compile_file(\'" + args[0] + "\'), wam_compiler:save_compiled_state(\'" + args[1] + "\').");
