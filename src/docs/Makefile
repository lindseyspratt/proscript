JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/usr/local/bin/swipl --traditional
DIST_DOC=../../docs
HTML := $(addprefix $(DIST_DOC)/, index.html index_about.html index_download.html index_doc.html calculator_source.html\
    console_source.html simple_source.html likes_source.html consult_console_source.html tiles_source.html)
TOP_HTML := ../../index.html
DIST_LIB := ../../dist/proscriptls.js
DOCS_LIB := $(DIST_DOC)/proscriptls.js
DOCS_OBJECT_PL := $(DIST_DOC)/object.pl
LIB_OBJECT_PL := ../../library/object.pl
ROOT_ENGINE=dist/proscriptls_engine.js
ENGINE:=../../$(ROOT_ENGINE)
SRC_SYSTEM=../system
SRC_TOOLS=../tools


all:	$(HTML) $(TOP_HTML) ../../README.SDK.md

clean:		
		rm -f $(HTML) $(DOCS_LIB) $(DOCS_OBJECT_PL) proscriptls_state_doc.js proscriptls_doc_node.js type_references.template

.PHONY: all clean dump-state

# create /index.html with '<!--suppress ALL -->' removed from $(DIST_DOC)/index.html.
# The 'suppress ALL' directive is used by webstorm inspection to suppress quality checks
# of $(DIST_DOC)/index.html. This version of index.html is in the wrong place (hence the copy
# to /index.html) so the inspection's href src checks spuriously indicate an problem.
# The 'sed' command both removes the 'suppress ALL' directive and creates a copy
# in the root of the project.

$(TOP_HTML): $(DIST_DOC)/index.html
		sed '1d' $(DIST_DOC)/index.html > $(TOP_HTML)

$(DIST_DOC)/index_doc.html: type_references.template library.doc.template objects.doc.template debugger.doc.template \
				compilation.doc.template syntax.doc.template items.predicates.doc.template predicates.doc.template

$(DIST_DOC)/%.html : %.template content.%.template sidenav.index.template api.sidenav.template anchor.sidenav.template \
				predicates.sidenav.template framework.index.template \
				../../version.txt ../../version_date.txt ../../library/template.pl
		$(SWIPL) -q -f ../../library/template.pl\
		    -g "substitute_template('$<', '$@'), halt"

$(ENGINE):
		../../make $(ROOT_ENGINE)

proscriptls_state_doc.js:	$(SRC_SYSTEM)/debugger.pl $(SRC_SYSTEM)/bootstrap_js.pl $(SRC_SYSTEM)/wam_compiler.pl\
                $(SRC_SYSTEM)/wam_assemble.pl $(SRC_SYSTEM)/wam_util.pl\
                $(SRC_TOOLS)/wam_bootstrap_master.pl $(SRC_TOOLS)/wam_bootstrap.pl $(SRC_TOOLS)/wam_bootstrap_util.pl\
				$(SRC_SYSTEM)/url.pl $(SRC_SYSTEM)/not.pl $(SRC_SYSTEM)/promise.pl\
				$(SRC_TOOLS)/type_references.pl
		$(SWIPL) -q -f $(SRC_TOOLS)/wam_bootstrap_master.pl -g \
			"bootstrap('$(SRC_SYSTEM)/', [index(basic), '$(SRC_TOOLS)/type_references.pl'], 'proscriptls_state_doc.js', true), halt"

dump-state: $(ENGINE) proscriptls_state_doc.js ../../test/standalone.js ../tools/dump.js
		$(JSC) $(ENGINE) proscriptls_state_doc.js ../../test/standalone.js ../tools/dump.js  -e "dump()"

proscriptls_doc_node.js:    $(ENGINE) proscriptls_state_doc.js ../tools/node_standalone.js ../tools/node_exports_init.js
		cat $(ENGINE) proscriptls_state_doc.js ../tools/node_standalone.js ../tools/node_exports_init.js > proscriptls_doc_node.js

items.predicates.doc.template:  proscriptls_doc_node.js ../tools/node_goal.js ../tools/generate_predicate_doc.pl predicate_doc.pl
		$(SWIPL) -q -f ../tools/generate_predicate_doc.pl -g "generate_predicate_doc('$@'), halt"

predicates.sidenav.template:  proscriptls_doc_node.js ../tools/node_goal.js ../tools/generate_predicate_doc.pl predicate_doc.pl
		$(SWIPL) -q -f ../tools/generate_predicate_doc.pl -g "generate_pred_cat_sidenav('$@'), halt"

type_references.template:  proscriptls_doc_node.js ../tools/node_goal.js
		node ../tools/node_goal.js ../docs/proscriptls_doc_node "generate_type_references('../docs/type_references.template')"

api.sidenav.template:  proscriptls_doc_node.js ../tools/node_goal.js
		node ../tools/node_goal.js ../docs/proscriptls_doc_node "generate_type_sidenav('../docs/api.sidenav.template')"

../../README.SDK.md: README.SDK.md.template ../../version.txt ../../version_date.txt
		$(SWIPL) -q -f ../../library/template.pl -g "substitute_template('$<', '$@'), halt"
