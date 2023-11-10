#' Get values for multiple entities from a Rxs tree, list of trees, or full Rxs project object
#'
#' @param x The tree, tree list, or full Rxs project object.
#' @param entityIds The entity identifiers of the values to get
#' @param entityIdsRegex A regular expression to match again entity identifiers
#' to obtain the `entityIds`.
#' @param lookInValueLists Whether to also look inside value lists
#' @param pathString_regex_select Regex that the target entities' path strings
#' have to match (otherwise, the entity is excluded)
#' @param silent Whether to be silent or chatty.
#'
#' @return A dataframe
#'
#' @export
#' 
get_multipleValues <- function(x,
                               entityIds = NULL,
                               entityIdsRegex = NULL,
                               lookInValueLists = TRUE,
                               silent = metabefor::opts$get("silent")) {
  
  allEntityIds <-
    rxsTree_get_entityIds(x);
  
  if (is.null(entityIds) && is.null(entityIdsRegex)) {
    entityIds <- allEntityIds;
  } else if (!is.null(entityIds) && !is.null(entityIdsRegex)) {
    stop("Specify one of `entityIds` and `entityIdsRegex` (or neither), but ",
         "not both!");
  } else if (!is.null(entityIdsRegex)) {
    entityIds <-
      grep(
        entityIdsRegex,
        allEntityIds,
        value = TRUE
      );
  }
  
  nonExistentEntityIds <- entityIds[!(entityIds %in% allEntityIds)];
  
  if (length(nonExistentEntityIds) > 0) {
    msg("\nNot all specified entityIds exist in the rxs object; removing ",
        vecTxtQ(nonExistentEntityIds), ".",
        silent = silent);
    entityIds <- entityIds[entityIds %in% allEntityIds];
  }
  
  if (length(entityIds) == 0) {
    msg("\nNo entity identifiers in list; returning NA.",
        silent = silent);
    return(NA);
  }
  
  msg("\nStarting to get the values for entities with identifiers ",
      vecTxtQ(entityIds), ".",
      silent = silent);
  
  if (inherits(x, "rxs_parsedExtractionScripts")) {
    
    allValueList <-
      lapply(
        entityIds,
        metabefor::get_singleValue,
        x = x,
        lookInValueLists = lookInValueLists,
        returnDf = TRUE,
        flattenVectorsInDf = TRUE,
        returnLongDf = FALSE,
        pathString_regex_select = ".*",
        silent = silent
      );

    names(allValueList) <- entityIds;
    
    nrows <-
      unlist(lapply(allValueList, nrow));
    
    entityIdsWithRows <-
      names(nrows);
    
    if (any(nrows == 0)) {
      stop("One or more Rxs trees did not return any results (specifically, ",
           vecTxtQ(entityIdsWithRows[which(nrows == 0)]), 
           ").");
    } else if (length(unique(nrows)) > 1) {
      stop("Not all results have the same number of rows - this means you ",
           "may have repeating entities (which means I cannot construct ",
           "a 'wide' dataframe, where columns are entities), or some sources ",
           "did not return a value.");
    }

    allValuesOnly <-
      lapply(
        entityIdsWithRows,
        function(colName) {
          return(
            allValueList[[colName]][, colName, drop=FALSE]
          );
        }
      );
    
    sourceIds <- allValueList[[entityIdsWithRows[1]]][, 1, drop=FALSE];
    
    doCallArgs <-
      c(list(sourceIds),
        allValuesOnly);
    
    return(
      do.call(
        cbind,
        doCallArgs
      )
    );

  } else if (inherits(x, "rxs") && inherits(x, "rxsObject")) {
    
    ### Not yet implemented
    
    stop("not implemented yet");
    
    # return(
    #   get_singleValue_fromTree(
    #     x = x,
    #     entityId = entityId,
    #     lookInValueLists = lookInValueLists,
    #     returnDf = returnDf,
    #     flattenVectorsInDf = flattenVectorsInDf,
    #     pathString_regex_flatten = pathString_regex_flatten,
    #     pathString_regex_explode = pathString_regex_explode,
    #     fieldname_regex_alwaysFlatten = fieldname_regex_alwaysFlatten,
    #     returnLongDf = returnLongDf,
    #     pathString_regex_select = pathString_regex_select,
    #     silent = silent
    #   )
    # );
    
  } else {
    
    stop("The object you passed is not an object with parsed Rxs files! It has class(es) ",
         vecTxtQ(class(x)), ".");
    
  }
  
}