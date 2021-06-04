#' @rdname matchingUniqueEntityIdentifiers
#' @export
studyTree_matchingUniqueEntityIdentifiers <- function(studyTree,
                                                      regex,
                                                      includeValueListsOfMatch = TRUE,
                                                      excludeParentWhenValueListReturned = TRUE) {
  
  res <-
    studyTree$Get(
      function(node) {
        if (is.null(node$value)) {
          return(NULL);
        } else if (grepl(regex, node$name)) {
          names <- grep(regex, node$name, value=TRUE);
          res <- data.frame(name = names,
                            fromList = FALSE);
          if (includeValueListsOfMatch && is.list(node$value)) {
            res2 <-
              data.frame(
                name = names(node$value),
                fromList = TRUE
              );
            if (excludeParentWhenValueListReturned) {
              res <- res2;
            } else {
              res <-
                rbind(res, res2);
            }
          }
        } else if (is.list(node$value)) {
          names <-
            grep(regex, names(node$value), value=TRUE);
          if (length(names) == 0) {
            return(NULL);
          } else {
            res <- data.frame(name = names,
                              fromList = TRUE);
          }
        } else {
          return(NULL);
        }
        return(res);
      }
    );
  
  ### Eliminate non-data.frame elements
  res <- res[
    unlist(
      lapply(
        res,
        is.data.frame
      )
    )
  ];
  
  res <-
    do.call(
      rbind,
      res
    );
  
  row.names(res) <- NULL;
  
  res <- unique(res);
  
  return(res);
  
}

