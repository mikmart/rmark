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
