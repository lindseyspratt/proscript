JSC=/System/Library/Frameworks/JavaScriptCore.framework/Versions/A/Resources/jsc
DEBUG=false
SWIPL=/usr/local/bin/swipl --traditional
DIST_DOC=../../dist/doc
HTML := $(addprefix $(DIST_DOC)/, index.html index_about.html index_download.html index_doc.html)


all:	$(HTML)

clean:		
		rm -f $(HTML)

$(DIST_DOC)/%.html : %.template content.%.template sidenav.index.template framework.index.template
		$(SWIPL) -q -f ../../library/template.pl\
		    -g "substitute_template('$<', '$@'), halt"
