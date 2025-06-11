sanitizeVector <- function(pattern, replacement, x, label) {
  ### Sanitize whitespace and unpermitted characters
  sanitizedVector <- gsub(pattern, replacement, x);
  sanitizationComparison <-
    x != sanitizedVector;
  
  sanitizationComparisonNonMissings <-
    sanitizationComparison[!is.na(sanitizationComparison)]
  
  if (length(sanitizationComparisonNonMissings) == 0) {
    return(sanitizedVector);
  } else if (all(sanitizationComparisonNonMissings)) {
    sanitizedValues <- !sanitizationComparison;
    warning("I checked and sanitized the ", label, ". ",
            "In this process, I made some changes, specifically, I changed ",
            vecTxtQ(x[sanitizedValues]),
            " to, respectively, ",
            vecTxtQ(sanitizedVector[sanitizedValues]), ".");
  }
  return(sanitizedVector);
  
}