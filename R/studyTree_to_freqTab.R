#' @rdname studies_to_freqTab 
#' @export
studyTree_to_freqTab <- function(x,
                                 rowRegex,
                                 colRegex,
                                 rowTargetValue = NULL,
                                 colTargetValue = NULL,
                                 rowColMultiplicationFunction = `*`,
                                 rowTargetFunction = `==`,
                                 colTargetFunction = `==`,
                                 includeValueListsOfMatch = TRUE,
                                 excludeParentWhenValueListReturned = TRUE) {

  rowEntityIDs <- 
    studyTree_matchingUniqueEntityIdentifiers(
      x,
      regex = rowRegex,
      includeValueListsOfMatch = includeValueListsOfMatch,
      excludeParentWhenValueListReturned = excludeParentWhenValueListReturned
    );
  
  colEntityIDs <- 
    studyTree_matchingUniqueEntityIdentifiers(
      x,
      regex = colRegex,
      includeValueListsOfMatch = includeValueListsOfMatch,
      excludeParentWhenValueListReturned = excludeParentWhenValueListReturned
    );
  
  rowValues <-
    unlist(
      lapply(
        rowEntityIDs$name,
        metabefor::getSingleValue_fromTree,
        x = x
      )
    );
  
  colValues <-
    unlist(
      lapply(
        colEntityIDs$name,
        metabefor::getSingleValue_fromTree,
        x = x
      )
    );
  
  if (!is.null(rowTargetValue)) {
    rowValues <- do.call(
      rowTargetFunction,
      list(
        rowValues,
        rowTargetValue
      )
    );
  }
  
  if (!is.null(colTargetValue)) {
    colValues <- do.call(
      colTargetFunction,
      list(
        colValues,
        colTargetValue
      )
    );
  }
  
  combinations <- sapply(t(colValues),
                         rowColMultiplicationFunction,
                         rowValues);
  
  rownames(combinations) <- rowEntityIDs$name;
  colnames(combinations) <- colEntityIDs$name;
  
  return(combinations);
  
}

