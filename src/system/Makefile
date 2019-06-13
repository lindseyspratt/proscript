JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/usr/local/bin/swipl --traditional


all:		proscriptls_state.js
clean:		
		rm -f ../../dist/proscriptls_state.js

proscriptls_state.js:	wam_compiler.pl ../tools/wam_bootstrap.pl url.pl bootstrap_js.pl not.pl debugger.pl promise.pl
		$(SWIPL) -q -f wam_compiler.pl -g "build_saved_state(['debugger.pl', 'wam_compiler.pl', 'url.pl', 'bootstrap_js.pl', 'not.pl', 'promise.pl'], '../../dist/proscriptls_state.js', 'foo'), halt"
