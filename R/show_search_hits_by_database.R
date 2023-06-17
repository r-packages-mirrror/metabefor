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
  
  tbl <- table(searchResults$bibHitDf$originDatabase);
  
  if (output == "kable") {
    return(
      knitr::kable(
        tbl,
        col.names = c("Database", "Number of records")
      )
    );
  } else {
    return(
      tbl
    )
  }
  
}