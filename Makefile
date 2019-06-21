JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/usr/local/bin/swipl --traditional


all:		dist/proscriptls.js doc
clean:		
		cd src/engine && make clean
		cd src/system && make clean
		cd src/docs && make clean
		rm -f dist/proscriptls.js

dist/proscriptls_state.js: src/system/* src/tools/wam_bootstrap.pl
		cd src/system && make

dist/proscriptls_engine.js: src/engine/* src/tools/js_preprocess.pl
		cd src/engine && make

dist/proscriptls.js:	dist/proscriptls_engine.js dist/proscriptls_state.js
		cat dist/proscriptls_engine.js dist/proscriptls_state.js > dist/proscriptls.js

doc:
		cd src/docs && make

gc:		dist/proscriptls.js standalone.js
		$(JSC) dist/proscriptls.js standalone.js  -e "gc_test($(DEBUG))"

dump-state: dist/proscriptls.js standalone.js dump.js
		$(JSC) dist/proscriptls.js standalone.js dump.js  -e "dumpPredicate('compile_body_args')"

test_proscript:		dist/proscriptls.js standalone.js
		$(JSC) dist/proscriptls.js standalone.js  -e "proscript(\"trace, mem(X,[a,b]), mem(X,[c,b]),writeln(X),notrace)\")"
