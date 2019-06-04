JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=true
SWIPL=/usr/local/bin/swipl --traditional


test:		../proscriptls_engine.js standalone.js proscriptls_state.js
		$(JSC) ../proscriptls_engine.js proscriptls_state.js standalone.js  -e "unit_tests($(DEBUG))"

proscriptls_state.js:	../debugger.pl ../bootstrap_js.pl ../wam_compiler.pl tests.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['tests.pl'], run_unit_tests), halt"

proscriptls_test.js:    ../proscriptls_engine.js proscriptls_state.js node_standalone.js node_exports.js
		cat ../proscriptls_engine.js proscriptls_state.js node_standalone.js node_exports.js > proscriptls_test.js

node_test:  proscriptls_test.js
		node node_test.js