JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/usr/local/bin/swipl --traditional


all:		bootstrap.js wam-pp.js
clean:		
		rm -f wam-pp.js bootstrap.js

bootstrap.js:	wam_compiler.pl wam_bootstrap.pl bootstrap_js.pl not.pl debugger.pl promise.pl
		$(SWIPL) -q -f wam_compiler.pl -g "build_saved_state(['debugger.pl', 'wam_compiler.pl', 'bootstrap_js.pl', 'not.pl', 'promise.pl'], 'foo'), halt"

tests_bootstrap.js:	wam_compiler.pl wam_bootstrap.pl bootstrap_js.pl not.pl debugger.pl test/web_tests.pl
		$(SWIPL) -q -f wam_compiler.pl -g "build_saved_state(['debugger.pl', 'wam_compiler.pl', 'bootstrap_js.pl', 'not.pl', 'test/web_tests.pl'], 'foo'), halt"

nrev_bootstrap.js:	wam_compiler.pl wam_bootstrap.pl bootstrap_js.pl not.pl debugger.pl bench/nrev.pl bench/common.pl bench/hook.pl
		$(SWIPL) -q -f wam_compiler.pl -g "build_saved_state(['bench/nrev.pl', 'bench/common.pl', 'bench/hook.pl', 'debugger.pl', 'wam_compiler.pl', 'bootstrap_js.pl', 'not.pl'], 'foo'), halt"

wam-pp.js:	foreign.js memory_files.js wam.js read.js record.js fli.js stream.js gc.js dom.js\
            debugger.js decode_instruction.js promise.js object.js web_interfaces.js object_property.js object_method.js
		$(SWIPL) -q -f js_preprocess.pl\
		    -g "preprocess(['foreign.js', 'memory_files.js', 'wam.js', 'read.js', 'record.js', 'fli.js', 'stream.js',\
		                    'gc.js', 'dom.js', 'debugger.js',\
		                    'decode_instruction.js', 'promise.js', 'object.js', 'web_interfaces.js', 'object_property.js', 'object_method.js'],\
		                   'wam-pp.js', [debug=$(DEBUG)]),\
		        halt"

gc:		wam-pp.js bootstrap.js standalone.js
		$(JSC) wam-pp.js bootstrap.js standalone.js  -e "gc_test($(DEBUG))"

dump-state: wam-pp.js bootstrap.js standalone.js dump.js
		$(JSC) wam-pp.js bootstrap.js standalone.js dump.js  -e "dumpPredicate('compile_body_args')"

test_proscript:		wam-pp.js bootstrap.js standalone.js
		$(JSC) wam-pp.js bootstrap.js standalone.js  -e "proscript(\"trace, mem(X,[a,b]), mem(X,[c,b]),writeln(X),notrace)\")"
