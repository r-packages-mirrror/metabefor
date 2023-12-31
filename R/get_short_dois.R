#' @rdname short_dois
#' @examples \dontrun{
#' ### Get a short DOI, just the short DOI returned
#' short_doi(doi = "10.1371/journal.pone.0042793")
#' short_doi(doi = "10.1890/10-0340.1")
#' }
#' @export
get_short_dois <- function(x = NULL, strip10 = TRUE) {
  return(
    unlist(
      lapply(
        x,
        get_short_doi,
        strip10 = strip10
      )
    )
  );
}
