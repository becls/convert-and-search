pdf :=\
  searcher.pdf\


.SUFFIXES:

.PHONY: all
all: $(pdf)

%.pdf: %.tex sagian.sty
	./run-latex $*

%.pdf: %.dot
	dot -Tpdf -o $@ $<

.PHONY: clean
clean:
	rm -f $(pdf) *.aux *.bbl *.blg *.lof *.log *.lot *.out *.idx *.ind *.ilg *.toc *.thm
	find . -iname "*.dot" | sed "s/\.dot$$/\.pdf/" | xargs rm -f
