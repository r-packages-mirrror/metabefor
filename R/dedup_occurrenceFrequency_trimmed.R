dedup_occurrenceFrequency_trimmed <- function(x, start=0, end=0) {
  
  if (start > 0) {
    x <- substr(x, 1, start);
  }
  
  if (end > 0) {
    x <- substr(x, nchar(x) - end + 1, nchar(x));
  }
  
  freqTab <- table(x);
  
  return(
    freqTab[x]
  );
}
