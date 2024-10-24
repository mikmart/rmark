#' Read a Markdown file
#' @param x A file path or [connection][base::connections].
#' @return A markdown node.
#' @export
read_md <- function(x) {
  if (is.character(x))
    x <- file(x)
  if (!isOpen(x)) {
    open(x, "r")
    on.exit(close(x))
  }
  .Call(rmark_read_md, x)
}

#' Parse Markdown text
#' @param x A character vector of lines of Markdown.
#' @return A markdown node.
#' @examples
#' parse_md("# Hello")
#' @export
parse_md <- function(x) {
  if (!is.character(x))
    x <- as.character(x)
  if (length(x) > 1)
    x <- paste(x, collapse = "\n")
  .Call(rmark_parse_md, x)
}

#' Render Markdown
#' @param x A markdown node.
#' @param ... Arguments reserved for future use.
#' @param format A string.
#' @param width An integer.
#' @export
render_md <- function(x, ..., format = "commonmark", width = getOption("width")) {
  if (...length() > 0)
    stop("`...` must be empty. Did you misspell or forget to name an argument?")
  format <- match.arg(format, CMARK_OUTPUT_FORMATS)
  .Call(rmark_render, x, match(format, CMARK_OUTPUT_FORMATS), as.integer(width))
}

CMARK_OUTPUT_FORMATS <- c("commonmark", "html", "latex", "man", "xml")

#' Classification
#' @param x A markdown node.
#' @name md_is
NULL

#' @export
#' @rdname md_is
is_md <- function(x) {
  inherits(x, "rmark_node")
}

#' @rdname md_is
#' @export
md_is_block <- function(x) {
  .Call(rmark_node_is_block, x)
}

#' @rdname md_is
#' @export
md_is_inline <- function(x) {
  .Call(rmark_node_is_inline, x)
}

#' @rdname md_is
#' @export
md_is_leaf <- function(x) {
  .Call(rmark_node_is_leaf, x)
}

#' Creating Nodes
#' @param type A string.
#' @return A markdown node.
#' @export
md_new_node <- function(type = "document") {
  type <- match.arg(type, CMARK_NODE_TYPES)
  .Call(rmark_node_new, match(type, CMARK_NODE_TYPES))
}

CMARK_NODE_TYPES <- c(
  # Block
  "document",
  "block_quote",
  "list",
  "item",
  "code_block",
  "html_block",
  "custom_block",
  "paragraph",
  "heading",
  "thematic_break",

  # Inline
  "text",
  "softbreak",
  "linebreak",
  "code",
  "html_inline",
  "custom_inline",
  "emph",
  "strong",
  "link",
  "image"
)

#' Tree Traversal
#' @param x A markdown node.
#' @name md_traversal
NULL

#' @rdname md_traversal
#' @export
md_next <- function(x) {
  .Call(rmark_node_next, x)
}

#' @rdname md_traversal
#' @export
md_previous <- function(x) {
  .Call(rmark_node_previous, x)
}

#' @rdname md_traversal
#' @export
md_parent <- function(x) {
  .Call(rmark_node_parent, x)
}

#' @rdname md_traversal
#' @export
md_first_child <- function(x) {
  .Call(rmark_node_first_child, x)
}

#' @rdname md_traversal
#' @export
md_last_child <- function(x) {
  .Call(rmark_node_last_child, x)
}


#' Iteration
#' @param x A markdown node.
#' @param callback A function.
#' @return `NULL`, invisibly.
#' @examples
#' root <- parse_md("# Hello")
#' md_iterate(root, function(node, event) {
#'   cat(event, "ing ", format(node), sep = "")
#' })
#' @export
md_iterate <- function(x, callback) {
  invisible(.Call(rmark_iterate, x, callback, parent.frame()))
}


#' Accessors
#' @param x A markdown node.
#' @name md_node
NULL

#' @rdname md_node
#' @export
md_type <- function(x) {
  .Call(rmark_node_get_type_string, x)
}

#' @rdname md_node
#' @export
md_literal <- function(x) {
  .Call(rmark_node_get_literal, x)
}

#' @param value A string.
#' @rdname md_node
#' @export
`md_literal<-` <- function(x, value) {
  if (!md_is_leaf(x))
    stop("`x` must be a leaf node, not a node of type <", md_type(x), ">.")
  stopifnot(length(value) == 1 && !is.na(value))
  .Call(rmark_node_set_literal, x, as.character(value))
}

#' @rdname md_node
#' @export
md_heading_level <- function(x) {
  .Call(rmark_node_get_heading_level, x)
}

#' @rdname md_node
#' @export
`md_heading_level<-` <- function(x, value) {
  if (md_type(x) != "heading")
    stop("`x` must be a heading node, not a node of type <", md_type(x), ">.")
  stopifnot(length(value) == 1 && !is.na(value))
  .Call(rmark_node_set_heading_level, x, as.integer(value))
}

#' @rdname md_node
#' @export
md_list_type <- function(x) {
  MD_LIST_TYPES[.Call(rmark_node_get_list_type, x)]
}

#' @rdname md_node
#' @export
`md_list_type<-` <- function(x, value) {
  if (md_type(x) != "list")
    stop("`x` must be a list node, not a node of type <", md_type(x), ">.")
  stopifnot(length(value) == 1)
  value <- match.arg(value, MD_LIST_TYPES)
  .Call(rmark_node_set_list_type, x, match(value, MD_LIST_TYPES))
}

MD_LIST_TYPES <- c("bullet", "ordered")

#' @rdname md_node
#' @export
md_list_delim <- function(x) {
  MD_LIST_DELIMS[.Call(rmark_node_get_list_delim, x)]
}

#' @rdname md_node
#' @export
`md_list_delim<-` <- function(x, value) {
  if (md_type(x) != "list")
    stop("`x` must be a list node, not a node of type <", md_type(x), ">.")
  stopifnot(length(value) == 1)
  value <- match.arg(value, MD_LIST_DELIMS)
  .Call(rmark_node_set_list_delim, x, match(value, MD_LIST_DELIMS))
}

MD_LIST_DELIMS <- c("period", "paren")

#' @rdname md_node
#' @export
md_list_start <- function(x) {
  .Call(rmark_node_get_list_start, x)
}

#' @rdname md_node
#' @export
`md_list_start<-` <- function(x, value) {
  if (md_type(x) != "list")
    stop("`x` must be a list node, not a node of type <", md_type(x), ">.")
  stopifnot(length(value) == 1 && !is.na(value))
  .Call(rmark_node_set_list_start, x, as.integer(value))
}

#' @rdname md_node
#' @export
md_list_tight <- function(x) {
  if (md_type(x) != "list")
    return(NA) # C API does not communicate "inapplicable".
  .Call(rmark_node_get_list_tight, x)
}

#' @rdname md_node
#' @export
`md_list_tight<-` <- function(x, value) {
  if (md_type(x) != "list")
    stop("`x` must be a list node, not a node of type <", md_type(x), ">.")
  stopifnot(length(value) == 1 && !is.na(value))
  .Call(rmark_node_set_list_tight, x, as.logical(value))
}

# TODO: Add node type validation to rest of the accessors below.

#' @rdname md_node
#' @export
md_fence_info <- function(x) {
  .Call(rmark_node_get_fence_info, x)
}

#' @rdname md_node
#' @export
`md_fence_info<-` <- function(x, value) {
  .Call(rmark_node_set_fence_info, x, as.character(value))
}

#' @rdname md_node
#' @export
md_url <- function(x) {
  .Call(rmark_node_get_url, x)
}

#' @rdname md_node
#' @export
`md_url<-` <- function(x, value) {
  .Call(rmark_node_set_url, x, as.character(value))
}

#' @rdname md_node
#' @export
md_title <- function(x) {
  .Call(rmark_node_get_title, x)
}

#' @rdname md_node
#' @export
`md_title<-` <- function(x, value) {
  .Call(rmark_node_set_title, x, as.character(value))
}

#' Location in Source Document
#' @param x A markdown node.
#' @return An integer.
#' @name md_location
NULL

#' @rdname md_location
#' @export
md_start_line <- function(x) {
  .Call(rmark_node_get_start_line, x)
}

#' @rdname md_location
#' @export
md_start_column <- function(x) {
  .Call(rmark_node_get_start_column, x)
}

#' @rdname md_location
#' @export
md_end_line <- function(x) {
  .Call(rmark_node_get_end_line, x)
}

#' @rdname md_location
#' @export
md_end_column <- function(x) {
  .Call(rmark_node_get_end_column, x)
}


#' Tree Manipulation
#' @param x A markdown node.
#' @param new A markdown node.
#' @return `NULL`, invisibly.
#' @name md_insert
NULL

#' @rdname md_insert
#' @export
md_unlink <- function(x) {
  invisible(.Call(rmark_node_unlink, x))
}

#' @rdname md_insert
#' @export
md_insert_before <- function(x, new) {
  ok <- .Call(rmark_node_insert_before, x, new)
  if (!ok) {
    stop(sprintf("Can't insert %s into %s.", format(new), format(md_parent(x))))
  }
  invisible(NULL)
}

#' @rdname md_insert
#' @export
md_insert_after <- function(x, new) {
  ok <- .Call(rmark_node_insert_after, x, new)
  if (!ok) {
    stop(sprintf("Can't insert %s into %s.", format(new), format(md_parent(x))))
  }
  invisible(NULL)
}

#' @rdname md_insert
#' @export
md_replace <- function(x, new) {
  ok <- .Call(rmark_node_replace, x, new)
  if (!ok) {
    stop(sprintf("Can't replace %s with %s.", format(x), format(new)))
  }
  invisible(NULL)
}

#' @rdname md_insert
#' @export
md_prepend_child <- function(x, new) {
  ok <- .Call(rmark_node_prepend_child, x, new)
  if (!ok) {
    stop(sprintf("Can't insert %s into %s.", format(new), format(x)))
  }
  invisible(NULL)
}

#' @rdname md_insert
#' @export
md_append_child <- function(x, new) {
  ok <- .Call(rmark_node_append_child, x, new)
  if (!ok) {
    stop(sprintf("Can't insert %s into %s.", format(new), format(x)))
  }
  invisible(NULL)
}


#' @export
print.rmark_node <- function(x, ...) {
  cat(format(x, ...), "\n")
  invisible(x)
}

#' @export
format.rmark_node <- function(x, ...) {
  paste("<md_node<", md_type(x), ">>", sep = "")
}
