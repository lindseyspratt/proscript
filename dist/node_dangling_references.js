let args = process.argv.slice(2);

const pls = require(args[0]);

pls.proscriptls_init();

pls.danglingPredicates();

