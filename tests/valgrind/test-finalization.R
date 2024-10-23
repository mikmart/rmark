library(rmark)

# Root does not leak when the reference is lost.
root <- parse_md("# Hello")
root <- parse_md("# Hello")
gc()

# Root does not deallocate while children are around.
child <- md_first_child(root)
rm(root)
gc()
child

# Root does deallocate once child is dropped.
rm(child)
gc()

# Losing a child reference doesn't deallocate.
root <- parse_md("# Hello")
child <- md_first_child(root)
rm(child)
gc()
if (is.null(md_first_child(root))) {
  stop("Child was dropped.")
}

# Dropping a reference to root acquired via md_parent() doesn't deallocate.
root <- parse_md("# Hello")
child <- md_first_child(root)
parent <- md_parent(root)
rm(parent, child)
gc()
root

# Unlinked nodes get cleaned up.
root <- read_md("README.md")
md_unlink(md_first_child(root))
gc()
