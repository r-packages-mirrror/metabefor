#' Show the number of search hits with DOIs
#'
#' @param x The object with search results, as produced by a call to
#' [metabefor::import_search_results()].
#' @param output Output format: `"kable"` or `"table"`.
#'
#' @return A character vector with the table in Markdown
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
#' metabefor::show_search_hits_with_dois(
#'   bibHits_Ebsco
#' );
show_search_hits_with_dois <- function(x,
                                       output = "kable") {
  
  if (!inherits(x, "mbfSearch")) {
    stop("As `x`, pass an object of class `mbfSearch`, as produced by ",
         "a call to metabefor::import_search_results().");
  }
  
  if ((!("doi" %in% names(x$bibHitDf))) || (all(is.na(x$bibHitDf$doi)))) {
    tbl <- as.table(c(nrow(x$bibHitDf), 0, nrow(x$bibHitDf)));
  } else if (all(!is.na(x$bibHitDf$doi))) {
    tbl <- as.table(c(0, nrow(x$bibHitDf), nrow(x$bibHitDf)));
  } else {
    tbl <- cbind(table(!is.na(x$bibHitDf$doi)), nrow(x$bibHitDf));
  }
  names(tbl) <- c("No DOI", "DOI", "Number of records");

  if (output == "kable") {
    return(
      knitr::kable(
        t(tbl)
      )
    );
  } else {
    return(
      tbl
    )
  }
  
}