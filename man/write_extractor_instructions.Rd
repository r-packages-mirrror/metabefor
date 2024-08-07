% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/write_extractor_instructions.R
\name{write_extractor_instructions}
\alias{write_extractor_instructions}
\title{Show and/or write the extractor instructions}
\usage{
write_extractor_instructions(
  x,
  output = "asis",
  outputFile = NULL,
  title = "Extractor Instructions",
  author = "",
  silent = TRUE
)
}
\arguments{
\item{x}{An rxsStructure object, as imported
by metabefor::rxs_fromSpecifications().}

\item{output}{The format of the object to return: \code{"asis"} to return content
ready to be included in a knitted file; \code{"raw"} for the raw results; or
\code{"none"} to return the raw result invisibly.}

\item{outputFile}{Optionally a file to write the extractor instructions to.}

\item{title, author}{The title and author to use when exporting.}

\item{silent}{Whether to be silent or chatty.}
}
\value{
A character vector with the extractor instructions in Markdown
}
\description{
Show and/or write the extractor instructions
}
\examples{
### Load an example rxs specification
data("rxs_minimal_example_2", package="metabefor");

### "Write" the extractor instructions but store
### them in a character vector instead of writing
### them to a file
extractorInstructions <-
  metabefor::write_extractor_instructions(
    rxs_minimal_example_2
  );

### Show the beginning
cat(substr(extractorInstructions, 1, 265));

### Show a fragment with instructions about one entity
cat(substr(extractorInstructions, 2070, 2790));
}
