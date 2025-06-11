#' Export an entity to ROCK format and import it back into the Rxs object
#' 
#' The Reproducible Open Coding Kit (ROCK) format is a format for
#' open and transparent storage and analysis of qualitative data. In
#' systematic reviews, you often want to extract an entity as raw text,
#' so that you can apply a structured coding procedure to the raw text
#' fragments without exposing the coders to other extracted information (to
#' minimize bias) while basing the coding on the full overview of all extracted
#' information (to facilitate identification of sensible codes/categories).
#' By exporting these entities to the ROCK format
#' with [metabefor::rxs_export_to_rock()], the
#' regular workflows used for qualitative research can be used, after which
#' the coded sources can be imported back into `metabefor` and merged with
#' the Rxs project using [metabefor::rxs_import_from_rock()].
#'
#' @param x The Rxs object, either an Rxs project (as produced
#' by [metabefor::rxs_parseExtractionScripts()]) or a single Rxs
#' extraction tree, to export from or merge into.
#' @param input The filename or path with files from which to import the ROCK
#' sources, or a character vector with the source's text.
#' @param filenameRegex Optionally, a regular expression: if not `NULL`, only
#' files matching this regualr expression will be imported.
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
#' @examples ### Load an example Rxs project
#' data('example_rxsProject_1', package="metabefor");
#' 
#' ### Export the titles to a ROCK source
#' ROCK_source <-
#'   metabefor::rxs_export_to_rock(
#'     example_rxsProject_1,
#'     entityId = "sourceTitle"
#'   );
#' 
#' ### Show the result
#' cat(ROCK_source, sep="\n");
#' 
#' ### Apply ROCK codes
#' ROCK_source[5] <-
#'   paste(
#'     ROCK_source[5],
#'     "[[pan]]"
#'   );
#' ROCK_source[11] <-
#'   paste(
#'     ROCK_source[11],
#'     "[[boot]]"
#'   );
#' 
#' ### Show coded result
#' cat(ROCK_source, sep="\n");
#' 
#' ### Import the result again
#' metabefor::rxs_import_from_rock(
#'   example_rxsProject_1,
#'   ROCK_source
#' );
#' 
#' ### Check both sources to see that the codes were imported
#' example_rxsProject_1$rxsTrees$qurid_7h50rzpq$general;
#' example_rxsProject_1$rxsTrees$qurid_7h50rzmz$general;
#' 
#' ### Look at the values (1s and 0s indicating whether
#' ### the code was applied for that source)
#' metabefor::rxsTree_to_valueDf(
#'   example_rxsProject_1$rxsTrees$qurid_7h50rzpq
#' )[3:5, ];
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
  
  rxsSourceId <- metabefor::opts$get("rockInterfacing_rxsSourceId");
  rxsEntityId <- metabefor::opts$get("rockInterfacing_rxsEntityId");
  
  if (length(entityId) == 1) {
    
    df <- metabefor::get_singleValue(
      x = x,
      entityId = entityId,
      returnDf = TRUE,
      returnLongDf = TRUE
    );
    
    cols_to_iids <- c("sourceId", "entityId");
    names(cols_to_iids) <- c(rxsSourceId, rxsEntityId);
    
    res <-
      rock::convert_df_to_source(
        data = df,
        output = outputFile,
        cols_to_utterances = "value",
        cols_to_ciids = cols_to_iids
      );
    
  } else {
    
    df <- metabefor::get_multipleValues(
      x = x,
      entityIds = entityId
    );
    
    cols_to_iids <- c("sourceId");
    names(cols_to_iids) <- c(rxsSourceId);
    
    includedEntities <-
      entityId[entityId %in% names(df)];

    res <-
      rock::convert_df_to_source(
        data = df,
        output = outputFile,
        cols_to_utterances = includedEntities,
        utterance_classId = rxsEntityId,
        cols_to_ciids = cols_to_iids
      );
    
  }
  
  return(
    invisible(
      res
    )
  );

}
