VERSION=0.2.0
JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/usr/local/bin/swipl --traditional
SDK:=proscriptls_sdk_$(VERSION)


all:		dist/proscriptls.js dist/proscriptls_for_compile.js dist/node_compile.js doc examples

.PHONY: all clean doc examples gc dump-state test_proscript sdk

clean:		
		cd src/engine && make clean
		cd src/system && make clean
		cd src/docs && make clean
		cd examples && make clean
		rm -f dist/proscriptls.js dist/proscriptls_state.js dist/proscriptls_engine.js dist/proscriptls_for_compile.js dist/node_compile.js
		rm -rf $(SDK)

dist/proscriptls_state.js: src/system/* src/tools/wam_bootstrap.pl
		cd src/system && make

dist/proscriptls_engine.js: src/engine/* src/tools/js_preprocess.pl
		cd src/engine && make

dist/proscriptls.js:	dist/proscriptls_engine.js dist/proscriptls_state.js
		cat dist/proscriptls_engine.js dist/proscriptls_state.js > dist/proscriptls.js

dist/proscriptls_for_compile.js:    dist/proscriptls.js src/tools/node_standalone.js src/tools/node_exports_init.js
		cat dist/proscriptls.js src/tools/node_standalone.js src/tools/node_exports_init.js > dist/proscriptls_for_compile.js

dist/node_compile.js: src/tools/node_compile.js
		cp src/tools/node_compile.js dist/node_compile.js

doc:
		cd src/docs && make

examples:
		cd examples && make

gc:		dist/proscriptls.js standalone.js
		$(JSC) dist/proscriptls.js standalone.js  -e "gc_test($(DEBUG))"

dump-state: dist/proscriptls.js standalone.js dump.js
		$(JSC) dist/proscriptls.js standalone.js dump.js  -e "dumpPredicate('compile_body_args')"

test_proscript:		dist/proscriptls.js standalone.js
		$(JSC) dist/proscriptls.js standalone.js  -e "proscriptls(\"trace, mem(X,[a,b]), mem(X,[c,b]),writeln(X),notrace)\")"

sdk:
		rm -rf $(SDK)
		mkdir $(SDK)
		cp index.html $(SDK)
		cp README.SDK.md $(SDK)
		cp src/tools/simple_server.js $(SDK)
		cp -r dist $(SDK)/dist
		cp -r examples $(SDK)/examples
		cp -r docs $(SDK)/docs
		cp -r library $(SDK)/library
		cp -r terminal $(SDK)/terminal
		tar -zcvf $(SDK).tar.gz $(SDK)
		rm -rf $(SDK)