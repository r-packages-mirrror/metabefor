#' @rdname maximizeMatrices
#' @export
maximizeMatrices <- function(x,
                             fillValue = 0) {

  resRows <- lapply(x, rownames);
  resCols <- lapply(x, colnames);
  
  uniqueRowIds <- unique(resRows);
  uniqueColIds <- unique(resCols);
  
  allRowIds <- unique(do.call(c, uniqueRowIds));
  allColIds <- unique(do.call(c, uniqueColIds));
  
  allRowIds[is.na(allRowIds)] <- "NA";
  allColIds[is.na(allColIds)] <- "NA";
  
  res <-
    lapply(
      x,
      maximizeMatrix,
      rowNames = allRowIds,
      colNames = allColIds,
      fillValue = fillValue
    );
  
  return(res);
  
}
                               