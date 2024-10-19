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

# Accessing literal values.
heading <- md_first_child(parse_md("# Hello"))
text_node <- md_first_child(heading)
md_literal(text_node)
md_literal(text_node) <- "World"
md_literal(text_node)

md_literal(heading)
try(md_literal(heading) <- "There")

# Accessing heading levels.
heading <- md_first_child(parse_md("# Hello"))
md_heading_level(heading)
md_heading_level(heading) <- 2
md_heading_level(heading)

md_heading_level(text_node)
try(md_heading_level(text_node) <- 2)

# Acecssing list types.
list_node <- md_first_child(parse_md("* Hello"))
md_list_type(list_node)
md_list_type(list_node) <- "ordered"
md_list_type(list_node)
render_md(list_node)

try(md_list_type(list_node) <- "foo")

md_list_type(text_node)
try(md_list_type(text_node) <- "bullet")
