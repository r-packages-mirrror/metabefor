#' @export
flattenNodeValues <- function(x) {
  
  if (is.null(x) || all(is.na(x))) {
    return(x);
  }
  
  if (!is.list(x)) {
    x <- list(x);
  }
  
  res <- lapply(x, function(singleValue) {

    res <- flattenNodeValue(singleValue);
    names(res) <- names(singleValue);
    return(res);

  });
  
  res <- unlist(res);
  if (!is.null(res)) {
    newNames <- names(x);
    if ((length(res) != length(names(x))) &&
        (all(names(res) %in% names(x)))) {
      newNames <- names(res);
    }
    names(res) <- newNames;
  }

  return(res);
}
