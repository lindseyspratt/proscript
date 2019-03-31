JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/opt/local/bin/swipl

clean:
		rm -f bootstrap.js

demo:	../wam_compiler.pl ../wam_bootstrap.pl ../bootstrap_js.pl ../debugger.pl demo.pl ../wam-pp.js ../standalone.js
		$(SWIPL) -q -f ../wam_compiler.pl -g "build_saved_state(['demo.pl', '../debugger.pl', '../wam_compiler.pl', '../bootstrap_js.pl'], 'foo'), halt"
		$(JSC) ../wam-pp.js bootstrap.js ../standalone.js  -e "demo($(DEBUG))"

dump-state: ../wam-pp.js ../bootstrap.js ../standalone.js ../dump.js
		$(JSC) ../wam-pp.js bootstrap.js ../standalone.js ../dump.js  -e "dump()"
