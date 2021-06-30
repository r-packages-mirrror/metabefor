#' @export
flattenNodeValues <- function(x) {
  
  if (!is.list(x)) {
    x <- list(x);
  }
  
  res <- lapply(x, function(singleValue) {

    res <- flattenNodeValue(singleValue);
    names(res) <- names(singleValue);
    return(res);

  });
  
  res <- unlist(res);
  names(res) <- names(x);

  return(res);
}
