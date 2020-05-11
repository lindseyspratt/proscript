JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/usr/local/bin/swipl --traditional
ENGINE=../dist/proscriptls_engine.js
SRC_ENGINE=../src/engine
SRC_SYSTEM=../src/system
SRC_TOOLS=../src/tools
NODE_TOOLS=../node_tools

all: node_test

clean: clean_index clean_factorial clean_struct
		rm -f proscriptls_state.js

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
		$(JSC) $(ENGINE) proscriptls_state.js $(SRC_ENGINE)/standalone.js  -e "dump('indexed-predicate', 'load')" #Predicate('compile_body_args')"

 # 'wam_compiler:compile_clause_2'
dump-state-pred: proscriptls_state.js
		$(JSC) $(ENGINE) proscriptls_state.js  -e "dumpPredicate('not:explicit_binding', 4, 'load')"

clean_index:
		rm -f proscriptls_index_state.js;

proscriptls_index_state.js: index_test.pl $(SRC_SYSTEM)/debugger.pl $(SRC_SYSTEM)/bootstrap_js.pl $(SRC_SYSTEM)/url.pl $(SRC_SYSTEM)/not.pl \
			$(SRC_SYSTEM)/wam_compiler.pl $(SRC_SYSTEM)/wam_assemble.pl $(SRC_SYSTEM)/wam_util.pl\
			$(SRC_TOOLS)/wam_bootstrap.pl $(SRC_TOOLS)/testing.pl\
			$(SRC_TOOLS)/wam_bootstrap_master.pl $(SRC_TOOLS)/wam_bootstrap_util.pl
		$(SWIPL) -q -f $(SRC_TOOLS)/wam_bootstrap_master.pl -g "bootstrap('$(SRC_SYSTEM)/', [index(basic), '$<'], '$@', baz(f(c))), halt"

node_index_test: $(NODE_TOOLS)/node_toplevel.js proscriptls_index_state.js
		node $(NODE_TOOLS)/node_toplevel.js ../test/proscriptls_index_state.js

clean_factorial:
		rm -f proscriptls_factorial_state.js;

xproscriptls_factorial_state.js: factorial.pl $(SRC_SYSTEM)/debugger.pl $(SRC_SYSTEM)/bootstrap_js.pl $(SRC_SYSTEM)/url.pl $(SRC_SYSTEM)/not.pl \
			$(SRC_SYSTEM)/wam_compiler.pl $(SRC_SYSTEM)/wam_assemble.pl $(SRC_SYSTEM)/wam_util.pl\
			$(SRC_TOOLS)/wam_bootstrap.pl $(SRC_TOOLS)/testing.pl\
			$(SRC_TOOLS)/wam_bootstrap_master.pl $(SRC_TOOLS)/wam_bootstrap_util.pl
		$(SWIPL) -q -f $(SRC_TOOLS)/wam_bootstrap_master.pl -g "bootstrap('$(SRC_SYSTEM)/', ['$<'], '$@', (factorial(5, N),writeln(N))), halt"

proscriptls_factorial_state.js:  ../dist/proscriptls_engine_for_node.js factorial.pl\
				$(NODE_TOOLS)/node_compile.js $(NODE_TOOLS)/node_goal.js
		node $(NODE_TOOLS)/node_compile.js `pwd`/factorial.pl `pwd`/proscriptls_factorial_state.js
		node $(NODE_TOOLS)/node_goal.js `pwd`/proscriptls_factorial_state.js true

node_factorial_test: $(NODE_TOOLS)/node_toplevel.js proscriptls_factorial_state.js
		node $(NODE_TOOLS)/node_goal.js `pwd`/proscriptls_factorial_state.js '(factorial(5, N),writeln(N)))'

dump_factorial: proscriptls_factorial_state.js
		$(JSC) $(ENGINE) proscriptls_factorial_state.js  -e "dumpPredicate('user:factorial', 2, 'load')"

clean_struct:
		rm -f proscriptls_struct_state.js;

xproscriptls_struct_state.js: struct.pl $(SRC_SYSTEM)/debugger.pl $(SRC_SYSTEM)/bootstrap_js.pl $(SRC_SYSTEM)/url.pl $(SRC_SYSTEM)/not.pl \
			$(SRC_SYSTEM)/wam_compiler.pl $(SRC_SYSTEM)/wam_assemble.pl $(SRC_SYSTEM)/wam_util.pl\
			$(SRC_TOOLS)/wam_bootstrap.pl $(SRC_TOOLS)/testing.pl\
			$(SRC_TOOLS)/wam_bootstrap_master.pl $(SRC_TOOLS)/wam_bootstrap_util.pl
		$(SWIPL) -q -f $(SRC_TOOLS)/wam_bootstrap_master.pl -g "bootstrap('$(SRC_SYSTEM)/', ['$<'], '$@', (struct(5, N),writeln(N))), halt"

proscriptls_struct_state.js:  ../dist/proscriptls_engine_for_node.js struct.pl\
				$(NODE_TOOLS)/node_compile.js $(NODE_TOOLS)/node_goal.js
		node $(NODE_TOOLS)/node_compile.js `pwd`/struct.pl `pwd`/proscriptls_struct_state.js
		node $(NODE_TOOLS)/node_goal.js `pwd`/proscriptls_struct_state.js true

node_struct_test: $(NODE_TOOLS)/node_toplevel.js proscriptls_struct_state.js
		node $(NODE_TOOLS)/node_goal.js `pwd`/proscriptls_struct_state.js 'struct(a : [b]))'

dump_struct: proscriptls_struct_state.js
		$(JSC) $(ENGINE) proscriptls_struct_state.js  -e "dumpPredicate('user:struct', 1, 'load')"
