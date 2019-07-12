JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=true
SWIPL=/usr/local/bin/swipl --traditional
ENGINE=../dist/proscriptls_engine.js
SRC_SYSTEM=../src/system


test:		$(ENGINE) standalone.js proscriptls_state.js
		$(JSC) $(ENGINE) proscriptls_state.js standalone.js  -e "proscriptls_toplevel($(DEBUG))"

proscriptls_state.js:	$(SRC_SYSTEM)/debugger.pl $(SRC_SYSTEM)/bootstrap_js.pl $(SRC_SYSTEM)/url.pl $(SRC_SYSTEM)/not.pl $(SRC_SYSTEM)/wam_compiler.pl ../src/tools/wam_bootstrap.pl ../src/tools/testing.pl tests.pl
		$(SWIPL) -q -f $(SRC_SYSTEM)/wam_compiler.pl -g "bootstrap('$(SRC_SYSTEM)/', ['tests.pl'], run_unit_tests), halt"

proscriptls_test.js:    $(ENGINE) proscriptls_state.js node_standalone.js node_exports.js
		cat $(ENGINE) proscriptls_state.js node_standalone.js node_exports.js > proscriptls_test.js

node_test:  proscriptls_test.js
		node node_test.js