dedup_matches_trimmed <-  function(needles, haystack, start=0, end=0) {

  if (start > 0) {
    needles <- substr(needles, 1, start);
    haystack <- substr(haystack, 1, start);
  }
  
  if (end > 0) {
    needles <- substr(needles, nchar(needles) - end + 1, nchar(needles));
    haystack <- substr(haystack, nchar(haystack) - end + 1, nchar(haystack));
  }
  
  return(
    ifelse(
      is.na(needles) | is.na(haystack),
      NA,
      needles %in% haystack
    )
  );

}

