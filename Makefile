JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/opt/local/bin/swipl


all:		bootstrap.js wam-pp.js
clean:		
		rm -f wam-pp.js bootstrap.js

bootstrap.js:	wam_compiler.pl testing.pl wam_bootstrap.pl bootstrap_js.pl tests.pl simple_test.pl console_button_test.pl debugger.pl
		$(SWIPL) -q -f wam_compiler.pl -g "build_saved_state(['simple_test.pl', 'console_button_test.pl', 'debugger.pl', 'wam_compiler.pl', 'bootstrap_js.pl', 'demo.pl'], 'foo'), halt"

wam-pp.js:	foreign.js wam.js read.js record.js fli.js stream.js gc.js dom.js dom_element_property.js dom_element_method.js
		$(SWIPL) -q -f js_preprocess.pl -g "preprocess(['foreign.js', 'wam.js', 'read.js', 'record.js', 'fli.js', 'stream.js', 'gc.js', 'dom.js', 'dom_element_property.js', 'dom_element_method.js'], 'wam-pp.js', [debug=$(DEBUG)]), halt"

test:		wam-pp.js bootstrap.js standalone.js wam_compiler.pl tests.pl
		$(SWIPL) -q -f wam_compiler.pl -g "bootstrap('tests.pl', run_unit_tests), halt" 
		$(JSC) wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

demo:		wam-pp.js bootstrap.js standalone.js
		$(JSC) wam-pp.js bootstrap.js standalone.js  -e "demo($(DEBUG))"

gc:		wam-pp.js bootstrap.js standalone.js
		$(JSC) wam-pp.js bootstrap.js standalone.js  -e "gc_test($(DEBUG))"

dump-state: wam-pp.js bootstrap.js standalone.js dump.js
		$(JSC) wam-pp.js bootstrap.js standalone.js dump.js  -e "dump()"
