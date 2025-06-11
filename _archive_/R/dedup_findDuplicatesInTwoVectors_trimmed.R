dedup_findDuplicatesInTwoVectors_trimmed <- function(x, y, start=0, end=0) {
  
  if (start > 0) {
    x <- substr(x, 1, start);
    y <- substr(y, 1, start);
  }
  
  if (end > 0) {
    x <- substr(x, nchar(x) - end + 1, nchar(x));
    y <- substr(y, nchar(y) - end + 1, nchar(y));
  }
  
  y <- y[!is.na(y)];
  
  return(
    (!is.na(x)) & (x %in% y)
  );
  
}
