JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/usr/local/bin/swipl --traditional


all:		wam_core.js
clean:		
		rm -f wam_core.js

wam_core.js: wam.template *.wam.template
		$(SWIPL) -q -f ../../../library/template.pl\
		    -g "substitute_template('$<', '$@'), halt"
