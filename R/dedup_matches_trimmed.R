#' Return for needles whether they occur in the haystack, trimmed if desired
#'
#' @param needles The entries to look for
#' @param haystack The set of entries in which to look
#' @param start The number of characters to use from the beginning (truncating
#' after this number of characters, e.g. only looking at the first 20 characters)
#' @param end The number of characters to use from the end (trimming everything
#' before this number of characters, e.g. only looking at the last 10 characters)
#'
#' @return A logical vector
#' @export
#'
#' @examples dedup_matches_trimmed(
#'   c('a', 'b', 'c'),
#'   c('c', 'd');
#' );
dedup_matches_trimmed <-  function(needles, haystack, start=0, end=0) {

  if (start > 0) {
    needles <- substr(needles, 1, start);
    haystack <- substr(haystack, 1, start);
  }
  
  if (end > 0) {
    needles <- substr(needles, nchar(needles) - end + 1, nchar(needles));
    haystack <- substr(haystack, nchar(haystack) - end + 1, nchar(haystack));
  }

  return(
    ifelse(
      is.na(needles),
      NA,
      needles %in% haystack
    )
  );

}

