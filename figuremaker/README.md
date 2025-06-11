# Thesis Figures

This directory handles generating of figures using the public version of the theory and associated code.
It relies on the [repository](https://github.com/pilif0/process-diagrams) with process diagram implementation.

All the automatically generated figures for the thesis are made by the `thesis` executable, the only executable in this project.
It takes as argument the destination directory for the figures, which will in most cases be [`img-gen`](../img-gen) in the root of the repository (but can be something else e.g. for testing purposes).
See the `figures` target in the thesis [Makefile](../Makefile).

## Building

This project requires [Stack](https://docs.haskellstack.org/en/stable/) to build.
Simply run `stack build` in the root to gather all dependencies, build them and build this project.

## Running

This project includes a single executable called `thesis`, which generates the thesis figures.
This can be run with `stack run thesis -- DIR` where `DIR` is the destination directory.
