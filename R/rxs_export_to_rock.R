#' Export an entity to ROCK format and import it back into the Rxs object
#' 
#' The Reproducible Open Coding Kit (ROCK) format is a format for
#' open and transparent storage and analysis of qualitative data. In
#' systematic reviews, you often want to extract an entity as raw text,
#' so that you can apply a structured coding procedure to the raw text
#' fragments without exposing the coders to other extracted information (to
#' minimize bias). By exporting these entities to the ROCK format, the
#' regular workflows used for qualitative research can be used, after which
#' the coded sources can be imported back into `metabefor` and merged with
#' the Rxs project.
#'
#' @param x The Rxs object, either an Rxs project (as produced
#' by [metabefor::rxs_parseExtractionScripts()]) or a single Rxs
#' extraction tree, to export from or merge into.
#' @param entityId The identifier of the entity to create a source for.
#' @param rxsEntityId,rxsSourceId The class instance identifiers to use in
#' the ROCK source to identify instances of the 'entity' and 'source' classes.
#' Note that source here refers to a source in the systematic review, i.e. an
#' Rxs tree; not to be confused with a ROCK source (i.e. a plain text file
#' with one or more data fragments to code).
#' @param outputFile Optionally, a file to write the source to. If no
#' file is specified, the resulting character vector will be returned
#' visibly (if an output file _is_ specified, it will be returned invisibly).
#'
#' @return The ROCK source as a character vector, invisibly, unless
#' an `outputFile` is specified for `metabefor::rxs_export_to_rock()`. However,
#' note that `metabefor::rxs_import_from_rock()` also modifies the Rxs object
#' in place (based on the "pass by reference" logic used by `data.tree`).
#' 
#' @export
#' @rdname rock_import_and_export
#'
#' @examples
rxs_export_to_rock <- function(x,
                               entityId = NULL,
                               outputFile = NULL,
                               rxsEntityId = metabefor::opts$get("rockInterfacing_rxsEntityId"),
                               rxsSourceId = metabefor::opts$get("rockInterfacing_rxsSourceId")) {
  
  if (!(requireNamespace("rock", quietly = TRUE))) {
    stop(wrap_error(
      "To export to ROCK format, you need the `rock` R package. ",
      "To install it, can use:\n\n  ",
      "install.packages('rock');\n"
    ));
  }

  if ((!inherits(x, "rxs_parsedExtractionScripts")) &&
      (!inherits(x, "rxsObject")) &&
      !(inherits(x, "rxs") && inherits(x, "Node"))) {
    stop(wrap_error(
      "As `x`, you have to pass a full Rxs project (i.e. as ",
      "obtained when parsing a set of Rxs files ",
      "with `metabefor::rxs_parseExtractionScripts()`), but instead, ",
      "you passed an object with class(es) ", vecTxtQ(class(x)), "."
    ));
  }

  df <- metabefor::get_singleValue(
    x = x,
    entityId = entityId,
    returnDf = TRUE,
    returnLongDf = TRUE
  );
  
  rxsSourceId <- metabefor::opts$get("rockInterfacing_rxsSourceId");
  rxsEntityId <- metabefor::opts$get("rockInterfacing_rxsEntityId");
  
  cols_to_iids <- c("sourceId", "entityId");
  names(cols_to_iids) <- c(rxsSourceId, rxsEntityId);
  
  return(
    rock::convert_df_to_source(
      data = df,
      output = outputFile,
      cols_to_utterances = "value",
      cols_to_ciids = cols_to_iids
    )
  );

}
