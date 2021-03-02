flattenNodeValues <- function(x) {
  if (!is.list(x)) x <- list(x);
  res <- lapply(x, function(singleValue) {
    if (is.null(singleValue)) {
      return(NULL);
    } else if (all(is.na(singleValue))) {
      return(NA);
    } else if (length(singleValue) == 1) {
      return(singleValue);
    } else if (is.vector(singleValue)) {
      return(ufs::vecTxtQ(singleValue));
    } else if (is.matrix(singleValue)) {
      return(paste(apply(singleValue, 1, vecTxtQ), collapse="\n"));
    } else {
      return(utils::capture.output(str(singleValue)));
    }
  });
  return(unlist(res));
}
