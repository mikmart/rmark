library(rmark)

root <- parse_md(readLines("LICENSE.md"))
cat(render_md(root, width = 69))
cat(render_md(root, format = "xml"))
gc()
