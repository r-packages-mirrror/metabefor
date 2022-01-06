#' Liberally checking whether a value is TRUE
#'
#' This function returns `FALSE` if `x` is `NULL`, `NA`, or `0`; `TRUE` if `x`
#' is a number other than `0` or the text `TRUE` (case insensitively).
#'
#' @param x The value to check.
#'
#' @return `TRUE` or `FALSE`
#' @export
#'
#' @examples
is_TRUE <- function(x) {
  if (is.null(x)) {
    return(FALSE);
  } else if (is.na(x)) {
    return(FALSE);
  } else if (isTRUE(x)) {
    return(TRUE);
  } else if (x == 0) {
    return(FALSE);
  } else if (is.numeric(x)) {
    return(TRUE);
  } else if (toupper(trimws(x))=="TRUE") {
    return(TRUE);
  } else {
    return(FALSE);
  }
}
