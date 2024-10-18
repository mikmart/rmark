# rmark

R bindings for the Commonmark C library [cmark](https://github.com/commonmark/cmark).

## Development

``` console
./tools/document
R CMD INSTALL .
```

## Testing

``` console
R --vanilla -d valgrind < tests/test-rmark.R
```
