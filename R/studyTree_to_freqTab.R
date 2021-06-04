#' @rdname studies_to_freqTab 
#' @export
studyTree_to_freqTab <- function(x,
                                 rowRegex,
                                 colRegex,
                                 rowTargetValue = NULL,
                                 colTargetValue = NULL,
                                 valuePreprocessingFunction = vectorValue_to_valueList,
                                 rowColMultiplicationFunction = `*`,
                                 rowTargetFunction = `==`,
                                 colTargetFunction = `==`,
                                 includeValueListsOfMatch = TRUE,
                                 excludeParentWhenValueListReturned = TRUE,
                                 silent = metabefor::opts$get("silent")) {

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
  
  rowNames <- rowEntityIDs$name;
  colNames <- colEntityIDs$name;
  
  rowValues <-
    unlist(
      lapply(
        rowNames,
        metabefor::getSingleValue_fromTree,
        x = x
      )
    );
  
  colValues <-
    unlist(
      lapply(
        colNames,
        metabefor::getSingleValue_fromTree,
        x = x
      )
    );
  
  if ((!all(rowEntityIDs$fromList)) &&
      (!all(colEntityIDs$fromList))) {
    if (!is.null(valuePreprocessingFunction)) {
      rowValues <-
        valuePreprocessingFunction(rowValues);
      colValues <-
        valuePreprocessingFunction(colValues);
      rowNames <- names(rowValues);
      colNames <- names(colValues);
    }
    
    if (is.list(rowValues)) {
      rowValues <- unlist(rowValues);
    }
    if (is.list(colValues)) {
      colValues <- unlist(colValues);
    }
  }

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

  combinations <- 
    matrix(
      sapply(
        as.matrix(colValues, nrow=1),
        rowColMultiplicationFunction,
        as.matrix(rowValues, ncol=1)),
      ncol=length(colValues)
    );
  
  rownames(combinations) <- rowNames;
  colnames(combinations) <- colNames;
  
  return(combinations);
  
}

