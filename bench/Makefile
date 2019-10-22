JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/usr/local/bin/swipl --traditional
ENGINE=../dist/proscriptls_engine.js
SRC_TOOLS=../src/tools
SRC_SYSTEM=../src/system

all: cal.test chat_parser.test crypt.test ham.test meta_qsort.test nand.test nrev.test poly_10.test qsort.test \
    queens.test queensn.test query.test reducer.test sendmore.test tak.test zebra.test boyer.test browse.test

%.test: %.pl $(ENGINE) standalone.js $(SRC_SYSTEM)/wam_compiler.pl
		$(SWIPL) -q -f $(SRC_TOOLS)/wam_bootstrap_master.pl -g "bootstrap('$(SRC_SYSTEM)/', [index(basic), 'common.pl', 'hook.pl', '$<'], main_test(1)), halt"
		$(JSC) $(ENGINE) proscriptls_state.js standalone.js  -e "proscriptls_toplevel($(DEBUG))"


# browse.test is slow - one iteration takes more than 6 seconds.

# query test fails if "D is (P * 100) // A" is not parenthesized: "D is P * 100 // A". Bug in yfx processing for * and //.
# large iteration count (e.g. 100,000) causes test to fail: appears to be a memory leak when iterating.
