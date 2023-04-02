#' Return the currently knitted file, without `.Rxs.Rmd` extension
#'
#' @param x Optionally, a filename. If `NULL`, [knitr::current_input()] is
#' read instead.
#' @return The currently knitted file, without `.Rxs.Rmd` extension
#' @export
#'
#' @examples metabefor::knittedFileSansExt("filename.Rxs.Rmd");
knittedFileSansExt <- function(x = NULL) {
  if (is.null(x)) {
    x <- knitr::current_input();
  }
  return(
    sub(
      "\\.Rxs\\.Rmd$",
      "",
      x,
      ignore.case = TRUE
    )
  );
}
