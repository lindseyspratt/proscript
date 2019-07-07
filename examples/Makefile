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

proscriptls_state_demo.js:	$(SRC_SYSTEM)/debugger.pl $(SRC_SYSTEM)/bootstrap_js.pl $(SRC_SYSTEM)/wam_compiler.pl demo.pl
		$(SWIPL) -q -f $(SRC_SYSTEM)/wam_compiler.pl -g "bootstrap('$(SRC_SYSTEM)/', ['demo.pl'], 'proscriptls_state_demo.js', 'foo'), halt"

dump-state: $(ENGINE) proscriptls_state_demo.js ../test/standalone.js $(SRC_TOOLS)/dump.js
		$(JSC) $(ENGINE) proscriptls_state_demo.js ../test/standalone.js $(SRC_TOOLS)/dump.js  -e "dump()"
