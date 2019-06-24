JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/usr/local/bin/swipl --traditional
DIST_DOC=../../docs
HTML := $(addprefix $(DIST_DOC)/, index.html index_about.html index_download.html index_doc.html)
DIST_LIB := ../../dist/proscriptls.js
DOCS_LIB := $(DIST_DOC)/proscriptls.js
DOCS_OBJECT_PL := $(DIST_DOC)/object.pl
LIB_OBJECT_PL := ../../library/object.pl
ROOT_ENGINE=dist/proscriptls_engine.js
ENGINE:=../../$(ROOT_ENGINE)
SRC_SYSTEM=../system


all:	$(HTML) $(DOCS_LIB) $(DOCS_OBJECT_PL)

clean:		
		rm -f $(HTML) $(DOCS_LIB) $(DOCS_OBJECT_PL)

$(DOCS_LIB): $(DIST_LIB)
		cp $(DIST_LIB) $(DOCS_LIB)

$(DOCS_OBJECT_PL): $(LIB_OBJECT_PL)
		cp $(LIB_OBJECT_PL) $(DOCS_OBJECT_PL)

$(DIST_DOC)/index_doc.html: type_references.template library.doc.template objects.doc.template debugger.doc.template

$(DIST_DOC)/%.html : %.template content.%.template sidenav.index.template framework.index.template
		$(SWIPL) -q -f ../../library/template.pl\
		    -g "substitute_template('$<', '$@'), halt"

$(ENGINE):
		../../make $(ROOT_ENGINE)

proscriptls_state_doc.js:	$(SRC_SYSTEM)/debugger.pl $(SRC_SYSTEM)/bootstrap_js.pl $(SRC_SYSTEM)/wam_compiler.pl\
				$(SRC_SYSTEM)/url.pl $(SRC_SYSTEM)/not.pl $(SRC_SYSTEM)/promise.pl ../tools/type_references.pl
		$(SWIPL) -q -f $(SRC_SYSTEM)/wam_compiler.pl -g \
			"bootstrap('$(SRC_SYSTEM)/', ['../tools/type_references.pl'], 'proscriptls_state_doc.js', generate_type_references('type_references.template')), halt"

dump-state: $(ENGINE) proscriptls_state_doc.js ../../test/standalone.js ../tools/dump.js
		$(JSC) $(ENGINE) proscriptls_state_doc.js ../../test/standalone.js ../tools/dump.js  -e "dump()"

proscriptls_doc_node.js:    $(ENGINE) proscriptls_state_doc.js ../tools/node_standalone.js ../tools/node_exports.js
		cat $(ENGINE) proscriptls_state_doc.js ../tools/node_standalone.js ../tools/node_exports.js > proscriptls_doc_node.js

type_references.template:  proscriptls_doc_node.js
		node ../tools/node_test.js