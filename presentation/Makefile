ROOT_DOC = slides
# BIB_FILE = ...

MODE = batchmode
VERBOSE_MODE = errorstopmode
LATEX = pdflatex -interaction=$(MODE) -synctex=1
LATEX_VERBOSE = pdflatex -interaction=$(VERBOSE_MODE) -synctex=1
BIB = bibtex

.PHONY: all verbose view clean full

all: $(ROOT_DOC).pdf

verbose:
	$(LATEX_VERBOSE) $(ROOT_DOC)

view:
	@xdg-open $(ROOT_DOC).pdf &

clean:
	@find . -name "*.aux" -type f -delete
	@find . -name "*.log" -type f -delete
	@find . -name "*.bbl" -type f -delete
	@find . -name "*.blg" -type f -delete
	@find . -name "*.bcf" -type f -delete
	@find . -name "*.run.xml" -type f -delete
	@find . -name "*.out" -type f -delete
	@find . -name "*.toc" -type f -delete
	@find . -name "*.synctex.gz" -type f -delete
	@echo "Cleanup complete."

.PHONY: full
full: $(ROOT_DOC).tex
	$(LATEX) $(ROOT_DOC) ;true
	# $(BIB)   $(ROOT_DOC) ;true
	$(LATEX) $(ROOT_DOC) ;true
	$(LATEX) $(ROOT_DOC) ;true

# Add further dependencies here
$(ROOT_DOC).pdf: $(ROOT_DOC).tex
	$(LATEX) $(ROOT_DOC)
