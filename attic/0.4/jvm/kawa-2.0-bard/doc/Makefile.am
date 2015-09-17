SUFFIXES = .pdf

kawa.txt:  $(srcdir)/kawa.texi version.texi
	$(MAKEINFO) -I$(srcdir) \
	    --no-split --no-headers --paragraph-indent=asis $< -o - \
          | sed -e '/^Concept Index/,$$d' \
             -e '/Making info file /,/END-INFO-DIR-ENTRY/d' \
          >$@

KAWA_HTMLDIR = kawa-html
KAWA_HTMLS = $(KAWA_HTMLDIR)/index.html
$(KAWA_HTMLS):  $(srcdir)/kawa.texi version.texi
	$(MAKEINFO) -I$(srcdir) --html --no-node-files $< -o $(KAWA_HTMLDIR)
HTMLS = $(KAWA_HTMLS)

html: $(KAWA_HTMLS)

TEXI2PDF = texi2pdf

.texi.pdf:
	TEXINPUTS=$(top_srcdir)/../texinfo/texinfo.tex:$$TEXINPUTS \
	  MAKEINFO='$(MAKEINFO) -I $(srcdir)' $(TEXI2PDF) -E $<

man_MANS = kawa.1 qexo.1
kawa.1: $(srcdir)/kawa.man
	nroff -man $(srcdir)/kawa.man > tpm-kawa.1
	mv tpm-kawa.1 kawa.1

qexo.1: $(srcdir)/qexo.man
	nroff -man $(srcdir)/qexo.man > tpm-qexo1
	mv tpm-qexo1 qexo.1

../kawa-doc-$(VERSION).tar.gz: kawa.info $(KAWA_HTMLDIR)/index.html kawa.pdf
	tar cf - $(KAWA_HTMLDIR)/*.html kawa.pdf|gzip -c --best >$@

clean-local:
	-rm -rf $(KAWA_HTMLDIR) tpm-qexo1 tpm-qexo1 kawa.1 qexo.1

EXTRA_DIST = scm2java.html kawa.man qexo.man mdate-sh
info_TEXINFOS = kawa.texi
MAINTAINERCLEANFILES = kawa.txt *.info 

# Redundant if configured with  --enable-maintainer-mode.
# Work around a questionable autoconf feature.
version.texi: stamp-vti

HTML_HOME = $$HOME/public_html

install-html:
	-mkdir $(HTML_HOME)/kawa
	cp $(KAWA_HTML_PARTS) $(HTML_HOME)/kawa/
	cp kawa_toc.html $(HTML_HOME)/kawa/kawa_toc.html
	cp kawa_toc.html $(HTML_HOME)/kawa/index.html
	sed -e 's|HREF="kawa_|HREF="kawa/kawa_|g' -e 's|HREF="../|HREF="|g' \
		<kawa_toc.html >$(HTML_HOME)/kawa.html
