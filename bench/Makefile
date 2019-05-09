JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/usr/local/bin/swipl --traditional

all: cal.test chat_parser.test crypt.test ham.test meta_qsort.test nand.test nrev.test poly_10.test qsort.test \
    queens.test queensn.test query.test reducer.test sendmore.test zebra.test browse.test

# boyer.test fails due to a heap overflow. It builds huge terms.
boyer.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl boyer.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'boyer.pl'], main_test(1)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

# browse.test is slow - one iteration takes more than 6 seconds.
browse.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl browse.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'browse.pl'], main_test(1)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

cal.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl cal.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'cal.pl'], main_test(2)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

chat_parser.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl chat_parser.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'chat_parser.pl'], main_test(2)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

crypt.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl crypt.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'crypt.pl'], main_test(50)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

ham.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl ham.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'ham.pl'], main_test(2)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

meta_qsort.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl meta_qsort.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'meta_qsort.pl'], main_test(10)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

nand.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl nand.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'nand.pl'], main_test(6)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

nrev.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl nrev.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'nrev.pl'], main_test(2)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

poly_10.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl poly_10.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'poly_10.pl'], main_test(10)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

qsort.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl qsort.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'qsort.pl'], main_test(100)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

queens.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl queens.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'queens.pl'], main_test(2)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

queensn.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl queensn.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'queensn.pl'], main_test(1)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

# query test fails if "D is (P * 100) // A" is not parenthesized: "D is P * 100 // A". Bug in yfx processing for * and //.
# large iteration count (e.g. 100,000) causes test to fail: appears to be a memory leak when iterating.
query.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl query.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'query.pl'], main_test(20000)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

reducer.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl reducer.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'reducer.pl'], main_test(4)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

sendmore.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl sendmore.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'sendmore.pl'], main_test(3)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

# tak.test fails: Top of frame exceeds bounds in allocate: 0x30006
tak.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl tak.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'tak.pl'], main_test(1)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"

zebra.test:	../wam-pp.js ../bootstrap.js standalone.js ../wam_compiler.pl zebra.pl
		$(SWIPL) -q -f ../wam_compiler.pl -g "bootstrap('../', ['common.pl', 'hook.pl', 'zebra.pl'], main_test(10)), halt"
		$(JSC) ../wam-pp.js bootstrap.js standalone.js  -e "unit_tests($(DEBUG))"
