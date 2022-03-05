% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/import_search_results.R
\name{import_search_results}
\alias{import_search_results}
\title{Import search results}
\usage{
import_search_results(
  path,
  dirRegex = ".*",
  fileRegex = "\\\\.ris$",
  recursive = TRUE,
  search_metadataRegex = metabefor::opts$get("search_metadataRegex"),
  silent = metabefor::opts$get("silent")
)
}
\arguments{
\item{path}{The path to the files with the search results.}

\item{dirRegex}{The regular expression to match subdirectories against.}

\item{fileRegex}{The regular expression to match filenames against.}

\item{recursive}{Whether to recursively read subdirectories.}

\item{search_metadataRegex}{A regular expression to match against the
filenames. If it matches, metadata will be extracted in three capturing
groups, in the order date (using ISO standard 8601 format, i.e. 2022-03-05),
interface, and database.}

\item{silent}{Whether to be silent or chatty.}
}
\value{
An object with all the imported information, including, most
importantly, the data frame \code{bibHitDf} with all results.
}
\description{
This is a wrapper around the \code{synthesisr} function \code{read_refs} that
processes the hits in a set of subdirectories in a directory, specifying
the original file, directory, and, if the files were names using a
convention, date, database, and interface (see the \code{search_metadataRegex}
argument).
}