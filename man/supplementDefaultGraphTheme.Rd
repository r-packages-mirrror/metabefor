% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/supplementDefaultGraphTheme.R
\name{supplementDefaultGraphTheme}
\alias{supplementDefaultGraphTheme}
\title{Supplement / edit the default graph theme}
\usage{
supplementDefaultGraphTheme(
  graphTheme,
  defaultGraphTheme = metabefor::defaultGraphTheme()
)
}
\arguments{
\item{graphTheme}{Changes, as a list with vectors, where each vector has
three elements corresponding to field name, field value, and what to set
that setting for, e.g. \code{c("fontname", "Arial", "graph")},
\code{c("shape", "box", "node")}, or \code{c("dir", "none", "edge")}.}

\item{defaultGraphTheme}{The default graph theme to use / supplement / edit.}
}
\value{
The supplemented / edited graph theme.
}
\description{
Supplement / edit the default graph theme
}
\examples{
supplementDefaultGraphTheme(
  list(
    c("dir", "both", "edge")
  )
);
}
