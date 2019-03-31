JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/opt/local/bin/swipl


all:		bootstrap.js wam-pp.js
clean:		
		rm -f wam-pp.js bootstrap.js

bootstrap.js:	wam_compiler.pl wam_bootstrap.pl bootstrap_js.pl debugger.pl
		$(SWIPL) -q -f wam_compiler.pl -g "build_saved_state(['debugger.pl', 'wam_compiler.pl', 'bootstrap_js.pl'], 'foo'), halt"

tests_bootstrap.js:	wam_compiler.pl wam_bootstrap.pl bootstrap_js.pl debugger.pl test/tests.pl
		$(SWIPL) -q -f wam_compiler.pl -g "build_saved_state(['debugger.pl', 'wam_compiler.pl', 'bootstrap_js.pl', 'test/tests.pl'], 'foo'), halt"

zebra_bootstrap.js:	wam_compiler.pl wam_bootstrap.pl bootstrap_js.pl debugger.pl bench/zebra.pl bench/common.pl bench/hook.pl
		$(SWIPL) -q -f wam_compiler.pl -g "build_saved_state(['bench/zebra.pl', 'bench/common.pl', 'bench/hook.pl', 'debugger.pl', 'wam_compiler.pl', 'bootstrap_js.pl'], 'foo'), halt"

wam-pp.js:	foreign.js memory_files.js wam.js read.js record.js fli.js stream.js gc.js dom.js dom_element_property.js dom_element_method.js debugger.js decode_instruction.js
		$(SWIPL) -q -f js_preprocess.pl -g "preprocess(['foreign.js', 'memory_files.js', 'wam.js', 'read.js', 'record.js', 'fli.js', 'stream.js', 'gc.js', 'dom.js', 'dom_element_property.js', 'dom_element_method.js', 'debugger.js', 'decode_instruction.js'], 'wam-pp.js', [debug=$(DEBUG)]), halt"

gc:		wam-pp.js bootstrap.js standalone.js
		$(JSC) wam-pp.js bootstrap.js standalone.js  -e "gc_test($(DEBUG))"

dump-state: wam-pp.js bootstrap.js standalone.js dump.js
		$(JSC) wam-pp.js bootstrap.js standalone.js dump.js  -e "dump()"

test_proscript:		wam-pp.js bootstrap.js standalone.js
		$(JSC) wam-pp.js bootstrap.js standalone.js  -e "proscript(\"trace, mem(X,[a,b]), mem(X,[c,b]),writeln(X),notrace)\")"
