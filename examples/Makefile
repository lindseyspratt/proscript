JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=true
SWIPL=/usr/local/bin/swipl --traditional
ENGINE=../dist/proscriptls_engine.js
SRC_SYSTEM=../src/system
SRC_ENGINE=../src/engine
SRC_TOOLS=../src/tools

all:	proscriptls_state_tiles.js proscriptls_state_demo.js

.PHONY: all clean demo dump-state

clean:
		rm -f proscriptls_state_tiles.js proscriptls_state_demo.js proscriptls_for_compile.js

demo:	$(ENGINE) ../test/standalone.js proscriptls_state_demo.js
		$(JSC) $(ENGINE) proscriptls_state_demo.js ../test/standalone.js  -e "proscriptls_toplevel($(DEBUG))"

proscriptls_state_demo.js: demo.pl $(SRC_SYSTEM)/debugger.pl $(SRC_SYSTEM)/bootstrap_js.pl $(SRC_SYSTEM)/wam_compiler.pl
		$(SWIPL) -q -f $(SRC_SYSTEM)/wam_compiler.pl -g "bootstrap('$(SRC_SYSTEM)/', ['$<'], '$@', 'foo'), halt"

dump-state: $(ENGINE) proscriptls_state_demo.js ../test/standalone.js $(SRC_TOOLS)/dump.js
		$(JSC) $(ENGINE) proscriptls_state_demo.js ../test/standalone.js $(SRC_TOOLS)/dump.js  -e "dump()"

proscriptls_for_compile.js:    ../dist/proscriptls.js $(SRC_TOOLS)/node_standalone.js $(SRC_TOOLS)/node_exports_init.js
		cat ../dist/proscriptls.js $(SRC_TOOLS)/node_standalone.js $(SRC_TOOLS)/node_exports_init.js > proscriptls_for_compile.js

proscriptls_state_tiles.js:  proscriptls_for_compile.js tiles.pl node_compile.js
		node node_compile.js tiles.pl proscriptls_state_tiles.js