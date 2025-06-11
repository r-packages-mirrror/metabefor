#' Printing a project with parsed Rxs file
#'
#' @param x The Rxs project file (as produced
#' by [rxs_parseExtractionScripts()])
#' @param ... Any additional arguments (ignored)
#'
#' @return `x`, invisibly.
#' @export
print.rxs_parsedExtractionScripts <- function(x, ...) {
  
  cat("\nThis Rxs project contains ", length(x$rxsTrees),
      " Rxs trees (suggesting it contains data extracted from ",
      length(x$rxsTrees), " sources).\n\n",
      "These trees contain the following entities:\n\n",
      metabefor::vecTxtQ(rxsTree_get_entityIds(x)), ".",
      sep = "");
  
  return(invisible(x));
  
}

