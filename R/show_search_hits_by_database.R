#' Show the search hits by database
#'
#' @param x The object with search results, as produced by a call to
#' [metabefor::import_search_results()].
#' @param output Output format: `"kable"` or `"table"`.
#'
#' @return A character vector with the table with search results in Markdown
#' @export
#'
#' @examples ### Path to extra files in {metabefor} package
#' metabefor_files_path <-
#'   system.file(
#'     "extdata",
#'     package = "metabefor"
#'   ); 
#' 
#' ### Example path with search hits
#' EbscoExport_path <-
#'   file.path(
#'     metabefor_files_path,
#'     "ebsco-exports"
#'   ); 
#'   
#' bibHits_Ebsco <-
#'   metabefor::import_search_results(
#'     EbscoExport_path
#'   );
#'   
#' ### Show the databases
#' metabefor::show_search_hits_by_database(
#'   bibHits_Ebsco
#' );
show_search_hits_by_database <- function(x,
                                         output = "kable") {
  
  if (!inherits(x, "mbfSearch")) {
    stop("As `x`, pass an object of class `mbfSearch`, as produced by ",
         "a call to metabefor::import_search_results().");
  }
  
  tbl <- table(x$bibHitDf$originDatabase);
  
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