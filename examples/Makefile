JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/opt/local/bin/swipl

clean:
		rm -f bootstrap.js

# create bootstrap.js for use with console_button_test.html and console_button_test.pl.
console.bootstrap.js:   console_button_test.pl ../wam_compiler.pl ../wam_bootstrap.pl ../bootstrap_js.pl ../debugger.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "build_saved_state(['console_button_test.pl', '../debugger.pl', '../wam_compiler.pl', '../bootstrap_js.pl'], 'foo'), halt"

# create bootstrap.js for use with simple_test.html and simple_test.pl.
simple.bootstrap.js:   simple_test.pl ../wam_compiler.pl ../wam_bootstrap.pl ../bootstrap_js.pl ../debugger.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "build_saved_state(['simple_test.pl', '../debugger.pl', '../wam_compiler.pl', '../bootstrap_js.pl'], 'foo'), halt"

# create bootstrap.js for use with calculator.html and calculator.pl.
calculator.bootstrap.js:   calculator.pl ../wam_compiler.pl ../wam_bootstrap.pl ../bootstrap_js.pl ../debugger.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "build_saved_state(['calculator.pl', '../debugger.pl', '../wam_compiler.pl', '../bootstrap_js.pl'], 'foo'), halt"

dump-state: ../wam-pp.js ../bootstrap.js standalone.js ../dump.js
		$(JSC) ../wam-pp.js bootstrap.js ../standalone.js ../dump.js  -e "dump()"
