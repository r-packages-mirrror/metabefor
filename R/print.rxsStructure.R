#' Printing a parsed Rxs specification
#'
#' @param x The Rxs specification (as produced
#' by [rxs_fromSpecifications()])
#' @param ... Any additional arguments (ignored)
#'
#' @return `x`, invisibly.
#' @export
print.rxsStructure <- function(x, ...) {
  
  cat("\nThis Rxs specification contains ",
      nrow(x$rxsSpecification$entities),
      " entities, with the following identifiers:\n\n",
      metabefor::vecTxtQ(x$rxsSpecification$entities$identifier),
      ".",
      sep = "");
  
  return(invisible(x));
  
}

