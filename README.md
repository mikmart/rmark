# rmark

_**NB** This project is experimental and subject to change at any time._

R bindings for the Commonmark C library [cmark](https://github.com/commonmark/cmark).

## Installation

Install from GitHub:

``` r
pak::pak("mikmart/rmark")
```

Note that the package links dynamically to libcmark, so you'll need to have that available.

## Example

A very crude approximation of the R Markdown rendering process:

``` r
library(rmark)

root <- parse_md(
  c(
    "# Hello",
    "Here comes a code block:",
    "``` r",
    "1 + 1",
    "```",
    "And some inline code: `pi`."
  )
)

md_iterate(
  root,
  function(node, event) {
    if (md_type(node) %in% c("code", "code_block")) {
      text <- md_literal(node)
      code <- parse(text = text)
      if (md_type(node) == "code") {
        result <- try(eval(code))
        new_node <- md_new_node("text")
        md_literal(new_node) <- format(result)
        md_replace(node, new_node)
      } else {
        result <- capture.output(try(eval(code)))
        new_node <- md_new_node("code_block")
        md_literal(new_node) <- format(result)
        md_insert_after(node, new_node)
      }
    }
  }
)

cat(render_md(root))
```

```
#> # Hello
#>
#> Here comes a code block:
#>
#> ``` r
#> 1 + 1
#> ```
#>
#> ```
#> [1] 2
#> ```
#>
#> And some inline code: 3.141593.
```

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
