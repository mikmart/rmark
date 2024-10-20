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

# A very crude approximation of the R Markdown rendering process.

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
gc()

cat(render_md(root))
