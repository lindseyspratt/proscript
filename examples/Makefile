JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=true
SWIPL=/usr/local/bin/swipl --traditional
ENGINE=../dist/proscriptls_engine.js
SRC_SYSTEM=../src/system
SRC_ENGINE=../src/engine
SRC_TOOLS=../src/tools

all:	demo

clean:
		rm -f proscriptls_state_demo.js

demo:	$(ENGINE) ../test/standalone.js proscriptls_state_demo.js
		$(JSC) $(ENGINE) proscriptls_state_demo.js ../test/standalone.js  -e "proscriptls_toplevel($(DEBUG))"

proscriptls_state_demo.js: demo.pl $(SRC_SYSTEM)/debugger.pl $(SRC_SYSTEM)/bootstrap_js.pl $(SRC_SYSTEM)/wam_compiler.pl
		$(SWIPL) -q -f $(SRC_SYSTEM)/wam_compiler.pl -g "bootstrap('$(SRC_SYSTEM)/', ['$<'], '$@', 'foo'), halt"

dump-state: $(ENGINE) proscriptls_state_demo.js ../test/standalone.js $(SRC_TOOLS)/dump.js
		$(JSC) $(ENGINE) proscriptls_state_demo.js ../test/standalone.js $(SRC_TOOLS)/dump.js  -e "dump()"

proscriptls_state_tiles.js:	tiles.pl $(SRC_SYSTEM)/debugger.pl $(SRC_SYSTEM)/bootstrap_js.pl $(SRC_SYSTEM)/wam_compiler.pl
		$(SWIPL) -q -f $(SRC_SYSTEM)/wam_compiler.pl -g "bootstrap('$(SRC_SYSTEM)/', ['$<'], '$@', 'true'), halt"
		$(JSC) $(ENGINE) ../dist/proscriptls_state.js ../test/standalone.js  -e "proscriptls_init('compile_file(\'tiles.pl\')')"

proscriptls_node_compile.js:    ../dist/proscriptls.js node_standalone.js node_exports.js
		cat ../dist/proscriptls.js node_standalone.js node_exports.js > proscriptls_node_compile.js

node_compile_tiles:  proscriptls_node_compile.js
		node node_compile_tiles.js