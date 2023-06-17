#' Show the number of search hits with DOIs
#'
#' @param x The object with search results, as produced by a call to
#' [metabefor::import_search_results()].
#' @param output Output format: `"kable"` or `"table"`.
#'
#' @return
#' @export
#'
#' @examples
show_search_hits_with_dois <- function(x,
                                       output = "kable") {
  
  if (!inherits(x, "mbfSearch")) {
    stop("As `x`, pass an object of class `mbfSearch`, as produced by ",
         "a call to metabefor::import_search_results().");
  }
  
  if (output == "kable") {
    return(
      knitr::kable(
        table(searchResults$bibHitDf$doi),
        col.names = c("DOI present?", "Number of records")
      )
    );
  } else {
    return(
      table(searchResults$bibHitDf$doi)
    )
  }
  
}