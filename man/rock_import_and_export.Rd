% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/rxs_export_to_rock.R, R/rxs_import_from_rock.R
\name{rxs_export_to_rock}
\alias{rxs_export_to_rock}
\alias{rxs_import_from_rock}
\title{Export an entity to ROCK format and import it back into the Rxs object}
\usage{
rxs_export_to_rock(
  x,
  entityId = NULL,
  outputFile = NULL,
  rxsEntityId = metabefor::opts$get("rockInterfacing_rxsEntityId"),
  rxsSourceId = metabefor::opts$get("rockInterfacing_rxsSourceId")
)

rxs_import_from_rock(
  x,
  input = NULL,
  newEntityIds = "\%s_\%s",
  newEntityIdCodeFirst = FALSE,
  filenameRegex = NULL,
  recursive = TRUE,
  silent = metabefor::opts$get("silent"),
  rxsSourceId = metabefor::opts$get("rockInterfacing_rxsSourceId"),
  rxsEntityId = metabefor::opts$get("rockInterfacing_rxsEntityId")
)
}
\arguments{
\item{x}{The Rxs object, either an Rxs project (as produced
by \code{\link[=rxs_parseExtractionScripts]{rxs_parseExtractionScripts()}}) or a single Rxs
extraction tree, to export from or merge into.}

\item{entityId}{The identifier of the entity to create a source for.}

\item{outputFile}{Optionally, a file to write the source to. If no
file is specified, the resulting character vector will be returned
visibly (if an output file \emph{is} specified, it will be returned invisibly).}

\item{rxsEntityId, rxsSourceId}{The class instance identifiers to use in
the ROCK source to identify instances of the 'entity' and 'source' classes.
Note that source here refers to a source in the systematic review, i.e. an
Rxs tree; not to be confused with a ROCK source (i.e. a plain text file
with one or more data fragments to code).}

\item{input}{The filename or path with files from which to import the ROCK
sources, or a character vector with the source's text.}

\item{newEntityIds, newEntityIdCodeFirst}{To determine how the new entity
identifiers are composed, \code{newEntityIds} s used. This has to be a
\link[base:sprintf]{base::sprintf} \code{fmt} string containing exactly two occurrences of "\verb{\%s}".
If \code{newEntityIdCodeFirst} is \code{FALSE}, the first \verb{\%s} is replaced by the
the original entity identifier (stored using the \code{rxsEntityId} class
instance identifier in the ROCK format), and the second by the code. If it is
\code{TRUE}, this order is reversed.}

\item{filenameRegex}{Optionally, a regular expression: if not \code{NULL}, only
files matching this regualr expression will be imported.}

\item{recursive}{Whether to read directories recursively}

\item{silent}{Whether to be chatty or silent.}
}
\value{
The ROCK source as a character vector, invisibly, unless
an \code{outputFile} is specified for \code{metabefor::rxs_export_to_rock()}. However,
note that \code{metabefor::rxs_import_from_rock()} also modifies the Rxs object
in place (based on the "pass by reference" logic used by \code{data.tree}).
}
\description{
The Reproducible Open Coding Kit (ROCK) format is a format for
open and transparent storage and analysis of qualitative data. In
systematic reviews, you often want to extract an entity as raw text,
so that you can apply a structured coding procedure to the raw text
fragments without exposing the coders to other extracted information (to
minimize bias) while basing the coding on the full overview of all extracted
information (to facilitate identification of sensible codes/categories).
By exporting these entities to the ROCK format
with \code{\link[=rxs_export_to_rock]{rxs_export_to_rock()}}, the
regular workflows used for qualitative research can be used, after which
the coded sources can be imported back into \code{metabefor} and merged with
the Rxs project using \code{\link[=rxs_import_from_rock]{rxs_import_from_rock()}}.
}
\examples{
### Load an example Rxs project
data('example_rxsProject_1', package="metabefor");

### Export the titles to a ROCK source
ROCK_source <-
  metabefor::rxs_export_to_rock(
    example_rxsProject_1,
    entityId = "sourceTitle"
  );

### Show the result
cat(ROCK_source, sep="\n");

### Apply ROCK codes
ROCK_source[5] <-
  paste(
    ROCK_source[5],
    "[[pan]]"
  );
ROCK_source[11] <-
  paste(
    ROCK_source[11],
    "[[boot]]"
  );

### Show coded result
cat(ROCK_source, sep="\n");

### Import the result again
metabefor::rxs_import_from_rock(
  example_rxsProject_1,
  ROCK_source
);

### Check both sources to see that the codes were imported
example_rxsProject_1$rxsTrees$qurid_7h50rzpq$general;
example_rxsProject_1$rxsTrees$qurid_7h50rzmz$general;

### Look at the values (1s and 0s indicating whether
### the code was applied for that source)
metabefor::rxsTree_to_valueDf(
  example_rxsProject_1$rxsTrees$qurid_7h50rzpq
)[3:5, ];
}
