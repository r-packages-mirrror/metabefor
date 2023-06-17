#' Knit the Rxs Template
#'
#' @param x The object with the Rxs Specification as produced by a call to
#' [metabefor::rxs_fromSpecifications()].
#'
#' @return
#' @export
#'
#' @examples
knit_rxsTemplate <- function(x) {
  
  if (!inherits(x, "rxsStructure")) {
    stop("As `x`, pass an object of class `rxsStructure`, as produced by ",
         "a call to metabefor::rxs_fromSpecifications().");
  }
  
  return(
    knitr::asis_output(
      paste0(
        "\n\n<pre><textarea rows='40' cols='124' style='font-family:monospace;font-size:11px;white-space:pre;'>",
        paste0(
          unlist(x$rxsTemplate),
          collapse = "\n"
        ),
        "</textarea></pre>\n\n",
        sep = "\n"
      )
    )
  );
  
}