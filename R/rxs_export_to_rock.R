rxs_export_to_rock <- function(x,
                               entity,
                               outputFile = NULL) {
  
  if (!inherits(x, "rxs_parsedExtractionScripts")) {
    stop(wrap_error(
      "As `x`, you have to pass a full Rxs project (i.e. as ",
      "obtained when parsing a set of Rxs files ",
      "with `metabefor::rxs_parseExtractionScripts()`), but instead, ",
      "you passed an object with class(es) ", vecTxtQ(class(x)), "."
    ));
  }
  
  if (!(requireNamespace("rock", quietly = TRUE))) {
    stop();
  }
  
}