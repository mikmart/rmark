% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rmark.R
\name{md_insert}
\alias{md_insert}
\alias{md_unlink}
\alias{md_insert_before}
\alias{md_insert_after}
\alias{md_replace}
\alias{md_prepend_child}
\alias{md_append_child}
\title{Tree Manipulation}
\usage{
md_unlink(x)

md_insert_before(x, new)

md_insert_after(x, new)

md_replace(x, new)

md_prepend_child(x, new)

md_append_child(x, new)
}
\arguments{
\item{x}{A markdown node.}

\item{new}{A markdown node.}
}
\value{
\code{NULL}, invisibly.
}
\description{
Tree Manipulation
}
