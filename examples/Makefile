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
		rm -f proscriptls_state_tiles.js proscriptls_state_demo.js

demo:	$(ENGINE) ../test/standalone.js proscriptls_state_demo.js
		$(JSC) $(ENGINE) proscriptls_state_demo.js ../test/standalone.js  -e "proscriptls_toplevel($(DEBUG))"

proscriptls_state_demo.js: demo.pl $(SRC_TOOLS)/wam_bootstrap_master.pl $(SRC_SYSTEM)/debugger.pl $(SRC_SYSTEM)/bootstrap_js.pl $(SRC_SYSTEM)/wam_compiler.pl
		$(SWIPL) -q -f $(SRC_TOOLS)/wam_bootstrap_master.pl -g "bootstrap('$(SRC_SYSTEM)/', ['$<'], '$@', 'foo'), halt"

dump-state: $(ENGINE) proscriptls_state_demo.js ../test/standalone.js $(SRC_TOOLS)/dump.js
		$(JSC) $(ENGINE) proscriptls_state_demo.js ../test/standalone.js $(SRC_TOOLS)/dump.js  -e "dump()"

proscriptls_state_tiles.js:  ../dist/proscriptls_for_compile.js tiles.pl ../library/data_predicates.pl ../library/object.pl ../dist/node_compile.js
		node ../dist/node_compile.js `pwd`/tiles.pl `pwd`/proscriptls_state_tiles.js