JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/opt/local/bin/swipl


bench:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl chat_parser.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'chat_parser.pl'], main_test(1000)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"
