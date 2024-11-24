#' @rdname short_dois
#' @examples \dontrun{
#' ### Get a short DOI, just the short DOI returned
#' metabefor::get_short_dois(
#'   c("10.1371/journal.pone.0042793",
#'     "10.1890/10-0340.1")
#' );
#' }
#' @export
get_short_dois <- function(x = NULL, strip10 = TRUE, throttle = TRUE,
                           throttleTime = .1,
                           silent = metabefor::opts$get('silent')) {
  
  if (!silent) {
    cat("Fetching ", length(x), " ShortDOIs; throttling is turned on and the ",
        "throttle time is set to ", throttle, " seconds, so if the requests ",
        "require throttling this could take ", x * throttleTime, " seconds.");
  }
  
  progress
  
  return(
    unlist(
      lapply(
        x,
        get_short_doi,
        strip10 = strip10,
        throttle = throttle,
        throttleTime = throttleTime
      )
    )
  );
}
