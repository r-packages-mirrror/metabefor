#' Retrieve the short DOI for one or more DOIs
#'
#' @param x A DOI or a vector of DOIs.
#' @param strip10 Whether to strip the leading `10/`.
#' @param throttle Whether to wait for `throttleTime` if no valid JSON containing
#' the short doi is returned (`throttle=TRUE`) or throw an
#' error (`throttle=FALSE`).
#' @param throttleTime How long to wait when throttling.
#' @return The short DOI or DOIs.
#' @rdname short_dois
#' @examples \dontrun{
#' ### Get a short DOI, just the short DOI returned
#' metabefor::get_short_doi("10.1371/journal.pone.0042793");
#' metabefor::get_short_doi("10.1890/10-0340.1");
#' }
#' @export
get_short_doi <- function(x = NULL, strip10 = TRUE,
                          throttle = TRUE, throttleTime = .1) {
  
  x <- sub("https?://doi.org/", "", x);

  urlToGet <-
    paste0(
      "https://shortdoi.org/",
      x,
      "?format=json&mailto=gjalt-jorn@behaviorchange.eu"
    );
  
  urlConnection <-
    url(
      urlToGet,
      headers = c(
        HTTPUserAgent = 
          paste0(
            options("HTTPUserAgent"),
            "; metabefor/0.3 ",
            "(https://r-packages.gitlab.io/metabefor; ",
            "mailto:gjalt-jorn@behaviorchange.eu)"
          )
        )
    );

  if (!(any(grepl("\"ShortDOI\":", res))) && throttle) {

    ### Wait
    Sys.sleep(throttleTime);
    
    ### Try again
    res <- readLines(urlConnection);

  }
  
  if (!(any(grepl("\"ShortDOI\":", res)))) {
      stop("No valid JSON containing a short DOI returned! Instead, ",
           "I received this:\n\n",
           paste0(res, collapse="\n"));
  }
  
  close(urlConnection);
  
  res <- grep("\"ShortDOI\":", res, value=TRUE);

  res <- gsub(".*(10/[a-zA-Z0-9]+).*", "\\1", res);
  
  if (strip10) {
    res <- gsub("^10/", "", res);
  }
  
  return(res);

}
