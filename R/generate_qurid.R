#' @export
generate_qurids <- function(x, origin = Sys.time()) {
  quridPrefix <- metabefor::opts$get('quridPrefix');
  quridSuffix <- metabefor::opts$get('quridSuffix');
  timeNrString <- as.character(round(as.numeric(origin) * 100, 
                                     0));
  timeNrs <- as.numeric(timeNrString) + (0:(x - 1));
  res <- unlist(lapply(timeNrs, metabefor::numericToBase30));
  return(paste0(quridPrefix, res, quridSuffix));
}
