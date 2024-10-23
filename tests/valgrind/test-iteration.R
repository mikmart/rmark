library(rmark)

root <- parse_md("# Hello")
md_iterate(root, function(node, event) {
  cat(event, "ing ", format(node), "\n", sep = "")
})
gc()

try({
  md_iterate(root, function(node, event) stop("boom"))
})
gc()
