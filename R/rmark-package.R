#' @useDynLib rmark, .registration = TRUE
NULL

#' Version of the Underlying cmark Library
#' @return A [numeric_version][base::numeric_version] object.
#' @export
cmark_version <- function() {
  numeric_version(.Call(rmark_cmark_version_string))
}
