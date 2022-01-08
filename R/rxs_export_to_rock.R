rxs_export_to_rock <- function(x,
                               entityId = NULL,
                               entityRegex = NULL,
                               outputFile = NULL) {
  
  if (!(requireNamespace("rock", quietly = TRUE))) {
    stop(wrap_error(
      "To export to ROCK format, you need the `rock` R package. ",
      "To install it, can use:\n\n  ",
      "install.packages('rock');\n"
    ));
  }
  
  if (is.null(entityId) && is.null(entityRegex)) {
    stop(wrap_error(
      "You have to pass either `entityId` or `entityRegex`, but ",
      "you passed neither."
    ));
  } else if (is.null(entityRegex)) {
    entityRegex <- paste0("^", entityId, "$");
  }
  
  if (inherits(x, "rxs_parsedExtractionScripts")) {
    
    
    
  } else if (inherits(x, "rxsObject")) {
    
    
    
  } else {
    stop(wrap_error(
      "As `x`, you have to pass a full Rxs project (i.e. as ",
      "obtained when parsing a set of Rxs files ",
      "with `metabefor::rxs_parseExtractionScripts()`), but instead, ",
      "you passed an object with class(es) ", vecTxtQ(class(x)), "."
    ));
  }

}