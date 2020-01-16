VERSION:=$(shell cat version.txt)
JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/usr/local/bin/swipl --traditional
SDK:=proscriptls_sdk_$(VERSION)



all:	dist/proscriptls.js dist/proscriptls_engine_for_node.js doc examples

.PHONY: all clean doc examples gc dump-state test_proscript sdk test dump-dangling

clean:		
		cd src/engine && make clean
		cd src/system && make clean
		cd src/docs && make clean
		cd examples && make clean
		rm -f dist/proscriptls.js dist/proscriptls_state.js dist/proscriptls_engine.js dist/proscriptls_for_compile.js
		rm -rf $(SDK)

dist/proscriptls_state.js: src/system/* src/tools/wam_bootstrap.pl
		cd src/system && make

dist/proscriptls_engine.js: src/engine/* src/engine/wam/*.template src/tools/js_preprocess.pl
		cd src/engine && make

# invoke node_goal.js to check that there are no dangling references in state recorded in dist/proscriptls_state.js.
dist/proscriptls.js:	dist/proscriptls_engine.js dist/proscriptls_state.js dist/proscriptls_engine_for_node.js node_tools/node_goal.js\
				node_tools/node_goal_module.js
		cat dist/proscriptls_engine.js dist/proscriptls_state.js > dist/proscriptls.js
		node node_tools/node_goal.js dist/proscriptls_state.js true

dist/proscriptls_engine_for_node.js:    dist/proscriptls_engine.js node_tools/node_standalone.js node_tools/node_exports_init.js
		cat dist/proscriptls_engine.js node_tools/node_standalone.js node_tools/node_exports_init.js > dist/proscriptls_engine_for_node.js

doc:
		cd src/docs && make

examples:
		cd examples && make

gc:		dist/proscriptls.js src/engine/standalone.js
		$(JSC) dist/proscriptls.js src/engine/standalone.js  -e "gc_test($(DEBUG))"

dump-state: dist/proscriptls.js src/engine/standalone.js
		$(JSC) dist/proscriptls.js src/engine/standalone.js  -e "dumpPredicate('wam_compiler:pop_current_compilation_module', 2, 'load')"

test_proscript:		dist/proscriptls.js src/engine/standalone.js
		$(JSC) dist/proscriptls.js src/engine/standalone.js  -e "proscriptls(\"trace, mem(X,[a,b]), mem(X,[c,b]),writeln(X),notrace)\")"

sdk:
		rm -rf $(SDK)
		mkdir $(SDK)
		cp index.html $(SDK)
		cp README.SDK.md $(SDK)
		cp -r node_tools $(SDK)
		cp -r dist $(SDK)/dist
		cp -r examples $(SDK)/examples
		cp -r docs $(SDK)/docs
		cp -r library $(SDK)/library
		cp -r terminal $(SDK)/terminal
		tar -zcvf $(SDK).tar.gz $(SDK)
		rm -rf $(SDK)
