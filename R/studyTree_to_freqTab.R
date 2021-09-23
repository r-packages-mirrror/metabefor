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
                                 rowLabels = NULL,
                                 colLabels = NULL,
                                 rowOrder = NULL,
                                 colOrder = NULL,
                                 sortRowsAlphabetically = TRUE,
                                 sortColsAlphabetically = TRUE,
                                 includeValueListsOfMatch = TRUE,
                                 flattenValues = TRUE,
                                 excludeParentWhenValueListReturned = TRUE,
                                 silent = metabefor::opts$get("silent")) {

  if (!metabefor::studyTree_has_entity(x, rowRegex)) {
    stop("The study tree you passed has no entities with an identifier ",
         "that matches the `rowRegex` you passed ('", rowRegex, "').");
  }

  if (!metabefor::studyTree_has_entity(x, colRegex)) {
    stop("The study tree you passed has no entities with an identifier ",
         "that matches the `colRegex` you passed ('", colRegex, "').");
  }
  
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
  
  if (!is.null(rowOrder)) {
    if (all(rowNames %in% rowOrder)) {
      stop("When specifying a rowOrder, it must contain all entities ",
           "that I find, but it doesn't. As rowOrder, I received ",
           vecTxtQ(rowOrder), " and the entities I found were ",
           vecTxtQ(rowNames), ".");
    } else {
      rowNames <- stats::setNames(rowNames, rowNames)[rowOrder];
    }
  }

  if (!is.null(colOrder)) {
    if (all(colNames %in% colOrder)) {
      stop("When specifying a colOrder, it must contain all entities ",
           "that I find, but it doesn't. As colOrder, I received ",
           vecTxtQ(colOrder), " and the entities I found were ",
           vecTxtQ(colNames), ".");
    } else {
      colNames <- stats::setNames(colNames, colNames)[colOrder];
    }
  }
  
  msg("Found the following entity identifiers for the rows: ",
      vecTxtQ(rowNames), ".\n",
      silent = silent);
  
  msg("Found the following entity identifiers for the columns: ",
      vecTxtQ(colNames), ".\n",
      silent = silent);
  
  rowValues <-
    unlist(
      lapply(
        rowNames,
        metabefor::get_singleValue_fromTree,
        x = x,
        silent = silent,
        flattenVectorsInDf = flattenValues
      )
    );
  
  colValues <-
    unlist(
      lapply(
        colNames,
        metabefor::get_singleValue_fromTree,
        x = x,
        silent = silent,
        flattenVectorsInDf = flattenValues
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

  ### Check whether we have values for the rows and columns
  
  if (is.null(colValues)) {
    stop("After having applied the `colTargetFunction` (you passed ",
         "a function named '", substitute(deparse(colTargetFunction)),
         "', no column values were produced.");
  }
  
  if (is.null(rowValues)) {
    stop("After having applied the `rowTargetFunction` (you passed ",
         "a function named '", substitute(deparse(rowTargetFunction)),
         "', no row values were produced.");
  }

  combinations <- 
    matrix(
      sapply(
        as.matrix(colValues, nrow=1),
        rowColMultiplicationFunction,
        as.matrix(rowValues, ncol=1)),
      ncol=length(colValues)
    );
  
  if (is.null(rowLabels)) {
    rownames(combinations) <- rowNames;
  } else {
    rownames(combinations) <- rowLabels[rowNames];
  }
  
  if (is.null(colLabels)) {
    colnames(combinations) <- colNames;
  } else {
    colnames(combinations) <- colLabels[colNames];
  }
  
  if (sortRowsAlphabetically) {
    combinations <-
      combinations[sort(rownames(combinations))
                   ,
                   ,
                   drop=FALSE];
  }  

  if (sortColsAlphabetically) {
    combinations <-
      combinations[,
                   sort(colnames(combinations)),
                   drop=FALSE];
  }  
  
  return(combinations);
  
}

