#' Parse Markdown
#' @param x A string of text containing Markdown.
#' @rdname rmark
#' @export
parse_md <- function(x) {
  .Call("rmark_parse_md", as.character(x))
}

#' Markdown nodes
#' @param x A markdown node.
#' @export
md_node_type <- function(x) {
  .Call("rmark_md_node_type", x)
}

#' @export
print.rmark_node <- function(x, ...) {
  cat(sprintf("<rmark_node<%s>>\n", md_node_type(x)))
  invisible(x)
}
