#' Show the search hits by database
#'
#' @param x The object with search results, as produced by a call to
#' [metabefor::import_search_results()].
#' @param output Output format: `"kable"` or `"table"`.
#'
#' @return
#' @export
#'
#' @examples
show_search_hits_by_database <- function(x,
                                         output = "kable") {
  
  if (!inherits(x, "mbfSearch")) {
    stop("As `x`, pass an object of class `mbfSearch`, as produced by ",
         "a call to metabefor::import_search_results().");
  }
  
  if (output == "kable") {
    return(
      knitr::kable(
        table(searchResults$bibHitDf$originDatabase),
        col.names = c("Database", "Number of records")
      )
    );
  } else {
    return(
      table(searchResults$bibHitDf$originDatabase)
    )
  }
  
}