const pls = require('./proscriptls_for_compile');
let args = process.argv.slice(2);
pls.proscriptls_init("compile_file(\'" + args[0] + "\'), save_compiled_state(\'" + args[1] + "\').");
