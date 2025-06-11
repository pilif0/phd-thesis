# Note: whole thesis does not reuse outputs of chapter building because of numberings and references

cwd = $(shell pwd)
latex_cmd = pdflatex --output-directory=build -shell-escape
bib_cmd = bibtex

# Set up the canned recipe for building a file
define build_command
$(latex_cmd) $(1).tex
if grep -qe "\\\\citation" build/*.aux; then $(bib_cmd) build/$(1); else echo No citations; fi
$(latex_cmd) $(1).tex
$(latex_cmd) $(1).tex
$(latex_cmd) $(1).tex
cp build/$(1).pdf output
cat build/$(1).log | { grep "Package smolathesis Warning" || echo "No custom warnings"; }
cat build/$(1).log | { grep "LaTeX Warning: Citation" || echo "No missing citations"; }
cat build/$(1).log | { grep "LaTeX Warning: Reference" || echo "No missing references"; }
endef

# Alias targets
thesis: output/main.pdf
appendix: output/appendix.pdf
ch-intro: output/ch-intro.pdf
ch-resources: output/ch-resources.pdf
ch-compositions: output/ch-compositions.pdf
ch-linearity: output/ch-linearity.pdf
ch-probabilistic: output/ch-probabilistic.pdf
ch-port_graphs: output/ch-port_graphs.pdf
ch-case_studies: output/ch-case_studies.pdf
ch-conc: output/ch-conc.pdf
prepdirs: build/.dirstamp output/.dirstamp
clean: clean_intermediate clean_build_pdf clean_output clean_figures

# None of those are actual files
.PHONY: thesis appendix ch-intro ch-resources ch-compositions ch-linearity ch-probabilistic ch-port_graphs ch-case_studies ch-conc prepdirs clean

# Build LaTeX files using a rule pattern
latex_files = main appendix ch-intro ch-resources ch-compositions ch-linearity ch-probabilistic ch-port_graphs ch-case_studies ch-conc
latex_targets = $(foreach target,$(latex_files),output/$(target).pdf)
$(latex_targets): output/%.pdf: clean_intermediate output/.dirstamp %.tex
	$(call build_command,$(basename $(notdir $@)))

# Generate figures from PhD repository
.PHONY: figures
figures: clean_figures
	cd figuremaker ; stack run ../img-gen

# Clean intermediate files from build directory
.PHONY: clean_intermediate
clean_intermediate: build/.dirstamp
	rm -f build/*.aux build/*.bbl build/*.log build/*.toc build/*.blg

# Clean PDFs from build directory
.PHONY: clean_build_pdf
clean_build_pdf: build/.dirstamp
	rm -f build/*.pdf

# Clean generated figures
.PHONY: clean_figures
clean_figures: img-gen/.dirstamp
	rm -f img-gen/*.svg

# Prepare build directory
build/.dirstamp:
	mkdir -p build && touch $@

# Prepare figure directory
img-gen/.dirstamp:
	mkdir -p img-gen && touch $@

# Clean output
.PHONY: clean_output
clean_output: output/.dirstamp
	rm -f output/*.pdf

# Prepare output directory
output/.dirstamp:
	mkdir -p output && touch $@
