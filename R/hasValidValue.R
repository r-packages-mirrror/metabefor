hasValidValue <- function(x) {
  return(
    (!is.null(x)) &&
    (!(any(is.na(x)))) &&
    (length(x) > 0)
  );
}

