# A Formalised Approach to the Composition of Processes over Linear Resources

This repository contains the code used to build my PhD thesis, submitted at the School of Informatics, University of Edinburgh.
I make the code available to share the different tricks I had to find while devloping and typesetting this large document.
The archival version of the thesis is available on the Edinburgh Research Archive (pending).

## Document Class

The initial class is the [`infthesis.cls`](infthesis.cls) file available from the School of Informatics.

I extend this class in [`smolathesis.cls`](smolathesis.cls) with additional packages and macros to suit my work and my style.
These revolve mainly around presenting Isabelle code and noting work in progress.

### Isabelle Style

The styling of Isabelle code is done mainly by style files in my [isastyle](https://github.com/pilif0/isastyle) repository.

The basis for the formatting of Isabelle code are the `isabelle.sty` and `isabellesym.sty` files included in Isabelle document preparation.
To these I add the `isabelleman.sty` file with some macros used in its manuals.

To highlight the syntax I use commands defined in `isabellecolor.sty`, applied manually.
(Note that this was developed before Isabelle 2025, which adds similar commands to document generation, allowing part of the colouring to be automatic.)
The colours are taken from the default configuration of Isabelle/jEdit.

Finally, I define two groups of Isabelle-specific helper commands.
One group are abbreviations for common surrounds (list brackets, parentheses, ...).
The other are for indentation, stretching the space that some text occupies to be as big as some other text (with two options for alignment of the shown text in that space - left or right).

### Numbered Isabelle Environments

To facilitate cross-referencing, `smolathesis.cls` defines theorem environments for lemmas (`isalemma`) and definitions (`isadef`).
These allow for (optional) names and (mandatory) labels, with the body switching directly to the `isabelle` environment.
Additionally, the definition environment has an alternative "raw" version that does not immediately enter the `isabelle` environment, allowin the contents to be first e.g. put into a `minipage`.

### Noting Work in Progress

The class file brings in two packages for this purpose: `todonotes` and `changebar`.
It includes the boolean option `feedback`, which will disable their rendering if false, and sets the margin options for the todo notes to fit.

For changebars it only defines one macro: `\cbar{text}` surrounds `text` with `\cbstart` and `\cbend` in a way that works well inline.

For todonotes, there are macros for various special cases:
- `\tocite` adds a small inline mark to denote a missing citation,
- `\toxref` adds a similar small inline mark to denote a missing cross-reference,
- `\todopoint` adds a small inine mark with just the text "ToDo",
- `\tonote{text}` adds a larger inline box with a grey background (which I used for remarks that are not tasks),
- `\tolist{\item ...}` adds a larger inline box with a list of items to do,
- `\stodo{text}` adds a blue note with an author tag (which I used for supervisor feedback).

Additionally, the `\tocite` and `\toxref` macros are set up to produce a warning in the build log, including the file name and line number in the message.
This allows for quickly checking whether any such missing citations/references are left in the built file.

### Lay Summary

The class file defines an environment for the lay summary to place it on its own page.
This is done in the same way that the abstract is set up in `infthesis.cls`.

## File Structure

I use the `standalone` package to split the thesis into a file for each chapter (plus files for the abstract, lay summary, acknowledgements and appendix).
Automatically generated figures are defined in a Haskell Stack project in [`figuremaker`](figuremaker), which uses my [library](https://github.com/pilif0/process-diagrams) for generating process diagrams.
The building is handled by `make`.

### Main File

The main file is [`main.tex`](main.tex), which sets up the overall document properties (title, author, year), abstract, lay summary and acknowledgements.
It then includes all the chapter files.

### Chapter Files

Each chapter file (the `.tex` files prefixed with `ch-`) uses the `standalone` document class to allow it to be both built on its own and included in other files.

### Building with Make

The `make` configuration is in the [`Makefile`](Makefile).

The general overview is that the individual files are built with `pdflatex` and `bibtex` in the `build` directory, and the resulting PDF is then copied to the `output` directory.
This keeps the latest build of each chapter (or the whole thesis) on hand while all the auxiliary build files are left separate.
In particular, the build command searches the log file for common warnings and repeats them at the end (or notes that there were none).

The building of an arbitrary LaTeX file is set up at the start as a canned recipe.
This is used in a rule pattern applied to all of the chapter files, the main file and the appendix file.
To avoid the need to specify targets as `output/file.pdf`, there are aliases for them with just the file name with no extension (and `thesis` for the main file).
The default target is `thesis`.

Make is also configured to generate the figures defined in `figuremaker`.
It simply navigates into the directory and uses Stack to run the executable there, targeting the `img-gen` directory.
