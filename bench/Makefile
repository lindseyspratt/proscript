JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/opt/local/bin/swipl

all: cal.test chat_parser.test crypt.test ham.test meta_qsort.test nand.test

cal.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl cal.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'cal.pl'], main_test(10)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

chat_parser.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl chat_parser.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'chat_parser.pl'], main_test(20)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

crypt.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl crypt.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'crypt.pl'], main_test(500)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

ham.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl ham.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'ham.pl'], main_test(4)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

meta_qsort.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl meta_qsort.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'meta_qsort.pl'], main_test(20)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

nand.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl nand.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'nand.pl'], main_test(6)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"
