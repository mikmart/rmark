# rmark

_**NB** This project is experimental and subject to change at any time._

R bindings for the Commonmark C library [cmark](https://github.com/commonmark/cmark).

## Development

Rebuild documentation, compile and install the package.

``` console
./tools/build
```

## Testing

Run unit tests:

``` console
./tools/test
```

Run memory tests:

``` console
./tools/test valgrind finalization
./tools/test valgrind iteration
./tools/test valgrind read
```

## Prior Art

- [commonmark](https://docs.ropensci.org/commonmark/) implements a narrower interface to the [GitHub fork of cmark](https://github.com/github/cmark-gfm/).
- [markdown](https://github.com/rstudio/markdown) builds on commonmark for more rendering features.
- [tinkr](https://docs.ropensci.org/tinkr/) builds on commonmark to expose editing Markdown documents via an XML representation.
