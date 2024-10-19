library(rmark)

root <- parse_md(readLines("LICENSE.md"))
cat(render_md(root, 69))
gc()
