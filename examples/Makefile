JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/opt/local/bin/swipl

clean:
		rm -f proscriptls_state.js

demo:	../wam_compiler.pl ../wam_bootstrap.pl ../bootstrap_js.pl ../debugger.pl demo.pl ../proscriptls_engine.js ../standalone.js
		$(SWIPL) -q -f ../wam_compiler.pl -g "build_saved_state(['demo.pl', '../debugger.pl', '../wam_compiler.pl', '../bootstrap_js.pl'], 'foo'), halt"
		$(JSC) ../proscriptls_engine.js proscriptls_state.js ../standalone.js  -e "demo($(DEBUG))"

dump-state: ../proscriptls_engine.js ../proscriptls_state.js ../standalone.js ../dump.js
		$(JSC) ../proscriptls_engine.js proscriptls_state.js ../standalone.js ../dump.js  -e "dump()"
