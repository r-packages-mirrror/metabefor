% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/flattenStudies_to_wideDf.R
\name{flattenRxsProject_to_wideDf}
\alias{flattenRxsProject_to_wideDf}
\title{Flatten all sources' Rxs trees to a wide data frame (somewhat brutally)}
\usage{
flattenRxsProject_to_wideDf(x, silent = metabefor::opts$get("silent"))
}
\arguments{
\item{x}{The full Rxs Project object (i.e. the parsed Rxs files)}

\item{silent}{Whether to be silent or chatty.}
}
\value{
A data frame
}
\description{
Flatten all sources' Rxs trees to a wide data frame (somewhat brutally)
}
