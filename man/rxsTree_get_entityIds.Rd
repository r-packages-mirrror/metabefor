% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rxsProject_get_entityIds.R,
%   R/rxsTree_get_entityIds.R
\name{rxsProject_get_entityIds}
\alias{rxsProject_get_entityIds}
\alias{rxsTree_get_entityIds}
\title{Get all identifiers of all entities in an Rxs tree or project}
\usage{
rxsProject_get_entityIds(x, includeClusteredEntities = TRUE)

rxsTree_get_entityIds(x, includeClusteredEntities = TRUE)
}
\arguments{
\item{x}{The rxs tree object or project object.}

\item{includeClusteredEntities}{Whether to also include clustered
entities (entities in a \code{list()} value).}
}
\value{
A vector with entity names.
}
\description{
This is a wrapper for the \code{data.tree} method \verb{$Get} to get the names of
all nodes in the tree object or project.
}
\examples{
### Load an example Rxs project
example_rxsProject <-
  metabefor::example_rxsProject;

metabefor::rxsProject_get_entityIds(
  example_rxsProject
);
}
