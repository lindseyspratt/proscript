JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
SWIPL=/usr/local/bin/swipl --traditional


all:		proscriptls_state.js
clean:		
		rm -f ../../dist/proscriptls_state.js

proscriptls_state.js:	../tools/wam_bootstrap_master.pl wam_compiler.pl wam_assemble.pl wam_util.pl \
            ../tools/wam_bootstrap.pl ../tools/wam_bootstrap_util.pl system.pl url.pl bootstrap_js.pl not.pl debugger.pl promise.pl
		$(SWIPL) -q -f ../tools/wam_bootstrap_master.pl -g "build_saved_state([\
			index(basic), 'system.pl', 'wam_compiler.pl'],\
			'../../dist/proscriptls_state.js', 'foo'), halt"
# debugger, system_util, not, bootstrap_js, promise, url,