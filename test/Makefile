JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/usr/local/bin/swipl --traditional
ENGINE=../dist/proscriptls_engine.js
SRC_ENGINE=../src/engine
SRC_SYSTEM=../src/system
SRC_TOOLS=../src/tools

all: node_test

jsc_test:		$(ENGINE) standalone.js proscriptls_state.js
		$(JSC) $(ENGINE) proscriptls_state.js standalone.js  -e "proscriptls_toplevel($(DEBUG))"

.PHONY: jsc_test node_test

proscriptls_state.js:	$(SRC_SYSTEM)/debugger.pl $(SRC_SYSTEM)/bootstrap_js.pl $(SRC_SYSTEM)/url.pl $(SRC_SYSTEM)/not.pl \
            $(SRC_SYSTEM)/wam_compiler.pl $(SRC_SYSTEM)/wam_assemble.pl $(SRC_SYSTEM)/wam_util.pl\
            $(SRC_TOOLS)/wam_bootstrap.pl $(SRC_TOOLS)/testing.pl tests.pl\
            $(SRC_TOOLS)/wam_bootstrap_master.pl $(SRC_TOOLS)/wam_bootstrap_util.pl
		$(SWIPL) -q -f $(SRC_TOOLS)/wam_bootstrap_master.pl -g "bootstrap('$(SRC_SYSTEM)/', ['tests.pl'], run_unit_tests), halt"

proscriptls_test.js:    $(ENGINE) proscriptls_state.js $(SRC_TOOLS)/node_standalone.js $(SRC_TOOLS)/node_exports_toplevel.js
		cat $(ENGINE) proscriptls_state.js $(SRC_TOOLS)/node_standalone.js $(SRC_TOOLS)/node_exports_toplevel.js > proscriptls_test.js

node_test: $(SRC_TOOLS)/node_toplevel.js proscriptls_test.js
		node $(SRC_TOOLS)/node_toplevel.js ../../test/proscriptls_test.js

dump-state: $(ENGINE) proscriptls_state.js $(SRC_ENGINE)/standalone.js $(SRC_TOOLS)/dump.js
		$(JSC) $(ENGINE) proscriptls_state.js $(SRC_ENGINE)/standalone.js $(SRC_TOOLS)/dump.js  -e "dump('defined-predicate')" #Predicate('compile_body_args')"

dump-state-test: proscriptls_test.js $(SRC_TOOLS)/dump.js
		$(JSC) proscriptls_test.js $(SRC_TOOLS)/dump.js  -e "dump('defined-predicate')" #Predicate('compile_body_args')"

clean_index:
		rm proscriptls_index_state.js;
		rm proscriptls_index_test.js;

proscriptls_index_state.js: index_test.pl $(SRC_SYSTEM)/debugger.pl $(SRC_SYSTEM)/bootstrap_js.pl $(SRC_SYSTEM)/url.pl $(SRC_SYSTEM)/not.pl \
            $(SRC_SYSTEM)/wam_compiler.pl $(SRC_SYSTEM)/wam_assemble.pl $(SRC_SYSTEM)/wam_util.pl\
            $(SRC_TOOLS)/wam_bootstrap.pl $(SRC_TOOLS)/testing.pl\
            $(SRC_TOOLS)/wam_bootstrap_master.pl $(SRC_TOOLS)/wam_bootstrap_util.pl
		$(SWIPL) -q -f $(SRC_TOOLS)/wam_bootstrap_master.pl -g "bootstrap('$(SRC_SYSTEM)/', [index(basic), '$<'], '$@', baz(f(c1))), halt"

proscriptls_index_test.js:    $(ENGINE) proscriptls_index_state.js $(SRC_TOOLS)/node_standalone.js $(SRC_TOOLS)/node_exports_toplevel.js
		cat $(ENGINE) proscriptls_index_state.js $(SRC_TOOLS)/node_standalone.js $(SRC_TOOLS)/node_exports_toplevel.js > proscriptls_index_test.js

node_index_test: $(SRC_TOOLS)/node_toplevel.js proscriptls_index_test.js
		node $(SRC_TOOLS)/node_toplevel.js ../../test/proscriptls_index_test.js
