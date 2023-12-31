% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/generate_qurid.R
\name{generate_qurids}
\alias{generate_qurids}
\alias{qurid}
\title{Generate quasi-unique record identifier}
\usage{
generate_qurids(x, origin = Sys.time())

qurid()
}
\arguments{
\item{x}{The number of QURIDs to produce.}

\item{origin}{The start date to use.}
}
\value{
The QURIDs as a character vector.
}
\description{
This function generates a quasi-unique record identifier.
}
\examples{
metabefor::qurid();
metabefor::generate_qurids(4);
}
