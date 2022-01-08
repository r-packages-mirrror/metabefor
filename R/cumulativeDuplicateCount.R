#' Get the cumulative duplicate count
#'
#' @param x A vector with (potential) duplicates
#'
#' @return A vector of length(x) with the cumulative duplicate count
#' @export
#'
#' @examples metabefor::cumulativeDuplicateCount(
#'   c(1, 1, 2, 3, 4, 2, 5, 6, 2, 7, 8, 1, 2)
#' );
cumulativeDuplicateCount <- function(x) {
  res <- c();
  for (i in seq_along(x)) {
    res[i] <- sum(x[1:i] == x[i]);
  }
  return(res);
}
