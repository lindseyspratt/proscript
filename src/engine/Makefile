JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/usr/local/bin/swipl --traditional


all:		proscriptls_engine.js
clean:		
		rm -f ../../dist/proscriptls_engine.js

proscriptls_engine.js:	foreign.js memory_files.js wam.js read.js record.js fli.js stream.js gc.js dom.js\
            debugger.js decode_instruction.js promise.js object.js web_interfaces.js object_property.js object_method.js\
            ../tools/js_preprocess.pl
		$(SWIPL) -q -f ../tools/js_preprocess.pl\
		    -g "preprocess(['foreign.js', 'memory_files.js', 'wam.js', 'read.js', 'record.js', 'fli.js', 'stream.js',\
		                    'gc.js', 'dom.js', 'debugger.js',\
		                    'decode_instruction.js', 'promise.js', 'object.js', 'web_interfaces.js', 'object_property.js', 'object_method.js'],\
		                   '../../dist/proscriptls_engine.js', [debug=$(DEBUG)]),\
		        halt"
