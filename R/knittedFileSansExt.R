#' Return the currently knitted file, without `.Rxs.Rmd` extension
#'
#' @return The currently knitted file, without `.Rxs.Rmd` extension
#' @export
#'
#' @examples metabefor::knittedFileSansExt("filename.Rxs.Rmd");
knittedFileSansExt <- function() {
  return(
    sub(
      "\\.Rxs\\.Rmd$",
      "",
      knitr::current_input()
    )
  );
}
