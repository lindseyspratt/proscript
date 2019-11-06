JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/usr/local/bin/swipl --traditional
ENGINE=../dist/proscriptls_engine.js
SRC_ENGINE=../src/engine
SRC_SYSTEM=../src/system
SRC_TOOLS=../src/tools
NODE_TOOLS=../node_tools

all: node_test

clean: clean_index
		rm -f proscriptls_state.js
		rm -f proscriptls_test.js
		rm -f proscriptls_index_state.js

jsc_test:		$(ENGINE) standalone.js proscriptls_state.js
		$(JSC) $(ENGINE) proscriptls_state.js standalone.js  -e "proscriptls_toplevel($(DEBUG))"

.PHONY: jsc_test node_test

proscriptls_state.js:	$(SRC_SYSTEM)/debugger.pl $(SRC_SYSTEM)/bootstrap_js.pl $(SRC_SYSTEM)/url.pl $(SRC_SYSTEM)/not.pl \
            $(SRC_SYSTEM)/wam_compiler.pl $(SRC_SYSTEM)/wam_assemble.pl $(SRC_SYSTEM)/wam_util.pl\
            $(SRC_TOOLS)/wam_bootstrap.pl $(SRC_TOOLS)/testing.pl tests.pl\
            $(SRC_TOOLS)/wam_bootstrap_master.pl $(SRC_TOOLS)/wam_bootstrap_util.pl
		$(SWIPL) -q -f $(SRC_TOOLS)/wam_bootstrap_master.pl -g "bootstrap('$(SRC_SYSTEM)/', ['tests.pl'], run_unit_tests), halt"

node_test: $(NODE_TOOLS)/node_toplevel.js proscriptls_state.js
		node $(NODE_TOOLS)/node_toplevel.js ../test/proscriptls_state.js

dump-state: $(ENGINE) proscriptls_state.js $(SRC_ENGINE)/standalone.js
		$(JSC) $(ENGINE) proscriptls_state.js $(SRC_ENGINE)/standalone.js  -e "dump('defined-predicate')" #Predicate('compile_body_args')"

dump-state-test: proscriptls_state.js
		$(JSC) $(ENGINE) proscriptls_state.js  -e "dump('defined-predicate')" #Predicate('compile_body_args')"

clean_index:
		rm -f proscriptls_index_state.js;

proscriptls_index_state.js: index_test.pl $(SRC_SYSTEM)/debugger.pl $(SRC_SYSTEM)/bootstrap_js.pl $(SRC_SYSTEM)/url.pl $(SRC_SYSTEM)/not.pl \
            $(SRC_SYSTEM)/wam_compiler.pl $(SRC_SYSTEM)/wam_assemble.pl $(SRC_SYSTEM)/wam_util.pl\
            $(SRC_TOOLS)/wam_bootstrap.pl $(SRC_TOOLS)/testing.pl\
            $(SRC_TOOLS)/wam_bootstrap_master.pl $(SRC_TOOLS)/wam_bootstrap_util.pl
		$(SWIPL) -q -f $(SRC_TOOLS)/wam_bootstrap_master.pl -g "bootstrap('$(SRC_SYSTEM)/', [index(basic), '$<'], '$@', baz(f(c))), halt"

node_index_test: $(NODE_TOOLS)/node_toplevel.js proscriptls_index_test.js
		node $(NODE_TOOLS)/node_toplevel.js ../test/proscriptls_index_state.js
