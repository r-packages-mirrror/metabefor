#' Sanitize for DiagrammeR
#'
#' Basically a wrapper for [gsub()] to sanitize a string for
#' DiagrammeR
#'
#' @param x The string or vector
#' @param regExReplacements A list of two-element character vectors; first element
#' should be the element to search, and the second element, the replacement.
#'
#' @return The sanitized character vector
#' @export
#'
#' @examples metabefor::sanitize_for_DiagrammeR("This is or isn't problematic");
sanitize_for_DiagrammeR <-
  function(x,
           regExReplacements = metabefor::opts$get("diagrammerSanitization")) {

  for (i in seq_along(regExReplacements)) {
    x <- gsub(regExReplacements[[i]][1],
              regExReplacements[[i]][2],
              x);
  }
  return(x);
}

