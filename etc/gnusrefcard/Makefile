# -*- mode: makefile; tab-width: 4 -*-

###### variables
sources	= README Makefile makelogo gnuslogo.ps gnusref.tex \
	quickref.tex refcard.tex booklet.tex bk-a4.tex bk-lt.tex
targets	= quickref.ps refcard.ps $(bk-lt) $(bk-a4)
bk-lt	= bk-lt-d.ps bk-lt-s1.ps bk-lt-s2.ps
bk-a4	= bk-a4-d.ps bk-a4-s1.ps bk-a4-s2.ps
 x-lt = 10.8in
 y-lt =  5.5in
x2-lt = -2.3in
y2-lt = 11.0in
 x-a4 = 27.4cm
 y-a4 = 14.85cm
x2-a4 = -6.3cm
y2-a4 = 29.7cm
  first = -3L($(x-$*),0)+0L($(x-$*),$(y-$*))
 second = 1R($(x2-$*),$(y2-$*))+-2R($(x2-$*),$(y-$*))
reverse = -1L($(x-$*),0)+2L($(x-$*),$(y-$*))

###### user targets
all: $(targets)
quickref: quickref.ps
refcard: refcard.ps
booklet: bk-lt bk-a4
bk-lt: bk-lt-d bk-lt-s
bk-a4: bk-a4-d bk-a4-s
bk-lt-d: bk-lt-d.ps
bk-lt-s: bk-lt-s1.ps bk-lt-s2.ps
bk-a4-d: bk-a4-d.ps
bk-a4-s: bk-a4-s1.ps bk-a4-s2.ps
clean:
	-rm -f *.dvi *.aux *.log *.toc gnuslogo.???* bk-??.ps $(targets)

###### internal targets
quickref.ps: quickref.tex gnuslogo.quickref
	latex quickref.tex
	dvips quickref.dvi
refcard.ps: refcard.tex gnusref.tex gnuslogo.refcard
	latex refcard.tex
	dvips refcard.dvi
bk-lt.ps bk-a4.ps: bk-%.ps: bk-%.tex booklet.tex gnusref.tex gnuslogo.booklet
	latex bk-$*.tex; latex bk-$*.tex  # twice to make the TOC
	dvips bk-$*.dvi
gnuslogo.quickref gnuslogo.refcard gnuslogo.booklet: gnuslogo.%: gnuslogo.ps
	./makelogo $*
bk-lt-d.ps bk-a4-d.ps: bk-%-d.ps: bk-%.ps
	pstops '4:$(first),$(second)' bk-$*.ps bk-$*-d.ps
bk-lt-s1.ps bk-a4-s1.ps: bk-%-s1.ps: bk-%.ps
	pstops '4:$(first)' bk-$*.ps bk-$*-s1.ps
bk-lt-s2.ps bk-a4-s2.ps: bk-%-s2.ps: bk-%.ps
	pstops '4:$(reverse)' bk-$*.ps bk-$*-s2.ps

dist = /usr/menaik/ftp/pub/oolog/gnus
dist: $(sources) $(targets)
	-mkdir $(dist); 
	-rm -f $(dist)/*
	tar cf - $(sources) | gzip > $(dist)/gnusref.tar.gz
	for F in $(targets); do gzip -c $$F > $(dist)/$$F.gz; done
	cp README $(dist)/README
	chmod ogu+r $(dist) $(dist)/*
	ls -l $(dist)
