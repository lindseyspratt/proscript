const pls = require('./proscriptls_for_compile');
let args = process.argv.slice(2);
pls.proscriptls_init("wam_compiler:compile_file(\'" + args[0] + "\'), wam_compiler:save_compiled_state(\'" + args[1] + "\').");
