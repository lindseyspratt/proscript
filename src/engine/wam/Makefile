JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/usr/local/bin/swipl --traditional


all:		wam_core.js
clean:		
		rm -f wam_core.js

wam_core.js: wam.template trace_call_or_execute.wam.template advance_next_trace_conditionally.wam.template\
				complete_call_or_execute.wam.template setup_and_call_foreign.wam.template deref.wam.template\
				tag.wam.template val.wam.template alloc_var.wam.template bind.wam.template\
				alloc_structure.wam.template
		$(SWIPL) -q -f ../../../library/template.pl\
		    -g "substitute_template('$<', '$@'), halt"
