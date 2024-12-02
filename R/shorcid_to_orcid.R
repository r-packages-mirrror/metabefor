#' @rdname ShORCIDs
#' @export
#'
#' @examples shorcid_to_orcid("i16g2sk1");
shorcid_to_orcid <- function(x) {
  
  if (length(x) > 1) {
    return(unlist(lapply(x, shorcid_to_orcid)));
  }
  
  x <- trimws(x);
  
  if (nchar(x) > 12) {
    stop("I can only convert ShORCIDs. A ShORCID is at most 12 characters, ",
         "but the input you provided ('", x, "') is ", nchar(x),
         " characters, and so, not an ShORCID.");
  }
  
  checksum <- substring(x, nchar(x));
  
  idbit <- substring(x, 2, nchar(x) - 1);
  
  res <- base30toNumeric(idbit);
  
  res <- paste0(res, checksum);
  
  padding <- repStr(16 - nchar(res), "0");
  
  res <- paste0(padding, res);
  
  res <- paste0(substr(res, 1, 4),
                "-",
                substr(res, 5, 8),
                "-",
                substr(res, 9, 12),
                "-",
                substr(res, 13, 16));
  
  return(res);
  
}
