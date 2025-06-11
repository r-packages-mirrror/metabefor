#' Extract DOI(s) from a character vector
#' 
#' This function extracts the DOI or DOIs from a character vector. It always
#' returns a single DOI for each element in the input vector (if one is found),
#' but returns additional DOIs in the attributes.
#'
#' @param x The character vector.
#'
#' @return A character vector.
#'
#' @examples exampleVector <-
#'   c("eni040 [pii] and 10.1093/deafed/eni040 [doi]",
#'     "S0022437503000938 [pii] and 10.1016/j.jsr.2003.12.001 [doi]",
#'     NA,
#'     "10.1080/15428119791012261 [doi]");
#' metabefor::extract_doi(
#'   exampleVector
#' );
#' @export
extract_doi <- function(x) {
  
  ### Using the regular expressions from
  ### https://www.crossref.org/blog/dois-and-matching-regular-expressions/
  ### Originally, these were:
  # "^10\\.\\d{4,9}\/[-._;()/:A-Z0-9]+$"
  # "^10\\.1002/[^\s]+$"
  # "^10\\.\\d{4}/\\d+-\\d+X?(\\d+)\d+<[\\d\\w]+:[\\d\\w]*>\\d+.\\d+.\w+;\\d$"
  # "^10\\.1021/\\w\\w\\d++$"
  # "^10\\.1207/[\\w\\d]+\\&\\d+_\d+$"
  
  regexes <-
    c("(10\\.\\d{4,9}\\/[-._;()\\/:A-Z0-9]+)",
      "(10\\.1002\\/[^\\s]+)",
      "(10\\.\\d{4}\\/\\d+-\\d+X?(\\d+)\\d+<[\\d\\w]+:[\\d\\w]*>\\d+.\\d+.\\w+;\\d)",
      "(10\\.1021\\/\\w\\w\\d+)",
      "(10\\.1207\\/[\\w\\d]+\\&\\d+_\\d+)");
  
  ### For testing using https://regex101.com
  # cat(paste0(regexes, collapse="\n"));
  
  regexMatchInfo <-
    lapply(
      regexes,
      regexec,
      text = x,
      ignore.case = TRUE
    );
  
  regexMatches <-
    lapply(
      regexMatchInfo,
      regmatches,
      x = x
    );
  
  mergedMatches <-
    lapply(
      seq_along(x),
      function(i) {
        return(
          c(
            unlist(
              lapply(
                seq_along(regexMatches),
                function(j) {
                  return(regexMatches[[j]][[i]])
                }
              )
            )
          )
        );
      }
    );
  
  lowerCaseMergedMatches <-
    lapply(
      mergedMatches,
      tolower
    );
  
  duplicateElementIndices <-
    lapply(
      lowerCaseMergedMatches,
      duplicated
    );
  
  fullRes <-
    lapply(
      seq_along(mergedMatches),
      function(i) {
        return(mergedMatches[[i]][!duplicateElementIndices[[i]]])
      }
    );
  
  res <-
    unlist(
      lapply(
        seq_along(fullRes),
        function(i) {
          return(fullRes[[i]][1]);
        }
      )
    );
  
  additionalDOIs <-
    unlist(
      lapply(
        seq_along(fullRes),
        function(i) {
          if (length(fullRes[[i]]) > 1) {
            additionalDOIs <-
              fullRes[[i]][2:length(fullRes[[i]])];
            ### Sometimes the additional DOI is just the DOI with
            ### additional, irrelevant, stuff attached
            notReallyAdditionalDOIs <-
              grepl(
                pattern = fullRes[[i]][1],
                x = additionalDOIs,
                fixed = TRUE
              );
            ### Sometimes it's the other way around
            notReallyAdditionalDOIs <-
              notReallyAdditionalDOIs |
              grepl(
                pattern = additionalDOIs,
                x = fullRes[[i]][1],
                fixed = TRUE
              );
            if (any(!notReallyAdditionalDOIs)) {
              return(additionalDOIs[!notReallyAdditionalDOIs]);
            } else {
              return(NA);
            }
          } else {
            return(NA);
          }
        }
      )
    );
  
  attr(res, "hasAdditional") <-
    !is.na(additionalDOIs);
  
  attr(res, "additional") <-
    additionalDOIs;
  
  return(res);
  
}
