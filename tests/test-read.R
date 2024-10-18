library(rmark)

read_md("LICENSE.md")
read_md(file("LICENSE.md"))
read_md(url("file://LICENSE.md"))
gc()
