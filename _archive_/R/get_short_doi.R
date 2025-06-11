#' Retrieve the short DOI for one or more DOIs
#'
#' @param x A DOI or a vector of DOIs.
#' @param strip10 Whether to strip the leading `10/`.
#' @param throttle Whether to wait for `throttleTime` if no valid JSON containing
#' the short doi is returned (`throttle=TRUE`) or throw an
#' error (`throttle=FALSE`).
#' @param throttleTime How long to wait when throttling.
#' @param progress For `get_short_dois`, whether to show a progress bar; for
#' `get_short_doi`, optionally, a progress bar to tick after the file
#' has been parsed  (must be a [progress::progress_bar()] object.
#' @return The short DOI or DOIs.
#' @rdname short_dois
#' @examples \dontrun{
#' ### Get a short DOI, just the short DOI returned
#' metabefor::get_short_doi("10.1371/journal.pone.0042793");
#' metabefor::get_short_doi("10.1890/10-0340.1");
#' }
#' @export
get_short_doi <- function(x = NULL, strip10 = TRUE,
                          throttle = TRUE, throttleTime = .1,
                          progress = NULL,
                          silent = metabefor::opts$get('silent')) {
  
  if (is.null(x) || is.na(x)) {
    if (!silent) {
      cat("You passed NULL or NA, so returning NA (invisibly).");
    }
    return(invisible(NA));
  }
  
  x <- sub("https?://doi.org/", "", x);

  urlToGet <-
    paste0(
      "https://shortdoi.org/",
      x,
      "?format=json&mailto=gjalt-jorn@behaviorchange.eu"
    );
  
  tryCatch(
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
      ),
    warning = function(w) {
      message("Warning when creating the URL connection!");
      browser();
    },
    error = function(e) {
      message("Error when creating the URL connection!");
      browser();
    }
  );

  ### Try to get the ShortDOI
  res <-
    tryCatch(
      readLines(urlConnection),
      warning = function(w) {
        if (grepl("HTTP status was '400 Bad Request'", w$message)) {
          warning("DOI ", x, " seems invalid; the ShortDOI service returned ",
                  "a 400 error (bad request). You can check it by visiting\n\n",
                  "https://shortdoi.org/", x,
                  "\n\nin your browser.");
          invisible(NA);
          tryInvokeRestart("muffleWarning");
        } else {
          message("Warning when reading from the URL connection!");
          browser();
          invisible(NA);
        }
      },
      error = function(e) {
        message("Error when reading from the URL connection!");
        browser();
        invisible(NA);
      }
    );
  
  if (all(is.na(res))) {
    return(NA);
  }

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
  
  if (!is.null(progress)) {
    if (inherits(progress, "progress_bar")) {
      progress$tick();
    }
  }
  
  return(res);

}
