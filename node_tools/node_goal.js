"use strict";

const ng = require('./node_goal_module.js');

let args = process.argv.slice(2);

ng.node_goal(args[0], args[1]);
