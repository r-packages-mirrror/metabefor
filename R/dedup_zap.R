dedup_zap <- function(zap, x) {
  return(
    gsub(zap, "", trimws(tolower(x)))
  );
}