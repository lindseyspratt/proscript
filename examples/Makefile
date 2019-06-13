JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/opt/local/bin/swipl
ENGINE=../dist/proscriptls_engine.js

clean:
		rm -f proscriptls_state.js

demo:	../wam_compiler.pl ../wam_bootstrap.pl ../bootstrap_js.pl ../debugger.pl demo.pl $(ENGINE) ../standalone.js
		$(SWIPL) -q -f ../wam_compiler.pl -g "build_saved_state(['demo.pl', '../debugger.pl', '../wam_compiler.pl', '../bootstrap_js.pl'], 'foo'), halt"
		$(JSC) $(ENGINE) proscriptls_state.js ../standalone.js  -e "demo($(DEBUG))"

dump-state: $(ENGINE) proscriptls_state.js ../standalone.js ../dump.js
		$(JSC) $(ENGINE) proscriptls_state.js ../standalone.js ../dump.js  -e "dump()"
