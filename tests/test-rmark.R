library(rmark)

# Root does not leak when the reference is lost.
root <- parse_md("# Hello")
root <- parse_md("# Hello")
gc()

# Root does not deallocate while children are around.
child <- md_first_child(root)
root <- NULL
child
gc()

# Child of childless node is NULL.
text_node <- md_first_child(child)
md_first_child(text_node)

# Classifiers.
md_is_block(text_node)
md_is_inline(text_node)
md_is_leaf(text_node)
