#' Converting ORCIDs to ShORCIDs and vice versa
#'
#' These functions produce ShOCRIDs (Short ORCIDs) from ORCIDs and vice versa.
#'
#' Conversion ORCID to ShORCID occurs by detaching the last character (the
#' checksum) and storing it. Then in the first string of characters, all
#' non-numbers are removed and the resulting number is converted to a base 30
#' system with [numericToBase30()]. The checksum is then re-attached. This is
#' done separately because the checksum can be X (i.e. the only character in an
#' ORCID that's not necessarily numeric). Then, an 'i' is prepended to ensure
#' that the ShORCID starts with a letter. Conversion the other way around just
#' inverts the process (and so uses [base30toNumeric()]).
#'
#' @param x The ORCID(s) or ShORCID(s).
#'
#' @return The ShORCID(s) or ORCID(s), as a character vector.
#' @rdname ShORCIDs
#' @export
#'
#' @examples orcid_to_shorcid("0000-0002-9540-5371");
orcid_to_shorcid <- function(x) {
  
  if (length(x) > 1) {
    return(unlist(lapply(x, orcid_to_shorcid)));
  }
  
  x <- trimws(x);
  
  checksum <- substring(x, nchar(x));
  
  idbit <- substring(x, 1, nchar(x) - 1);
  
  idbit <- gsub("[^0-9]", "", idbit);
  
  if (nchar(idbit) != 15) {
    stop("I can only convert ORCIDs. An ORCID is exactly 16 characters (not ",
         "counting the dashes added between every 4 characters for readability), ",
         "but the input you provided ('", x, "') is ", nchar(idbit) + 1,
         " characters, and so, not an ORCID.");
  }
  
  res <- numericToBase30(as.numeric(idbit));
  
  res <- paste0("i", res, checksum);
  
  return(res);
  
}
