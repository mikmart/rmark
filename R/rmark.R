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
  .Call("rmark_read_md", x)
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
    paste(x, collapse = "\n")
  .Call("rmark_parse_md", x)
}

#' Render Markdown
#' @param x A markdown node.
#' @param width An integer specifying the maximum line width of the output text.
#' @export
render_md <- function(x, width = getOption("width")) {
  .Call("rmark_render_md", x, as.integer(width))
}

#' Markdown nodes
#' @param x A markdown node.
#' @name md_node
NULL

#' @rdname md_node
#' @export
md_type <- function(x) {
  .Call("rmark_node_type", x)
}

#' @rdname md_node
#' @export
md_first_child <- function(x) {
  .Call("rmark_node_first_child", x)
}

#' @rdname md_node
#' @export
md_literal <- function(x) {
  .Call("rmark_node_get_literal", x)
}

#' @export
print.rmark_node <- function(x, ...) {
  cat("<md_node<", md_type(x), ">>\n", sep = "")
  invisible(x)
}

#' @rdname md_node
#' @export
md_is_block <- function(x) {
  .Call("rmark_node_is_block", x)
}

#' @rdname md_node
#' @export
md_is_inline <- function(x) {
  .Call("rmark_node_is_inline", x)
}

#' @rdname md_node
#' @export
md_is_leaf <- function(x) {
  .Call("rmark_node_is_leaf", x)
}
