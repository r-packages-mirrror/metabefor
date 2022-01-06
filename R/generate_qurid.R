#' Generate quasi-unique record identifier
#' 
#' This function generates a quasi-unique record identifier.
#'
#' @param x The number of QURIDs to produce.
#' @param origin The start date to use.
#'
#' @return The QURIDs as a character vector.
#' @rdname qurid
#' @export
#'
#' @examples metabefor::qurid();
#' metabefor::generate_qurids(4);
generate_qurids <- function(x, origin = Sys.time()) {
  quridPrefix <- metabefor::opts$get('quridPrefix');
  quridSuffix <- metabefor::opts$get('quridSuffix');
  timeNrString <- as.character(round(as.numeric(origin) * 100, 
                                     0));
  timeNrs <- as.numeric(timeNrString) + (0:(x - 1));
  res <- unlist(lapply(timeNrs, metabefor::numericToBase30));
  return(paste0(quridPrefix, res, quridSuffix));
}

#' @export
#' @rdname qurid
qurid <- function() {
  return(generate_qurids(1));
}
