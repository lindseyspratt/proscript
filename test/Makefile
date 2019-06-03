JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=true
SWIPL=/usr/local/bin/swipl --traditional


test:		../proscriptls_engine.js ../proscriptls_state.js standalone.js ../wam_compiler.pl tests.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['tests.pl'], run_unit_tests), halt"
		$(JSC) ../proscriptls_engine.js proscriptls_state.js standalone.js  -e "unit_tests($(DEBUG))"
