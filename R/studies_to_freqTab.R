#' @rdname studies_to_freqTab 
#' @export
studies_to_freqTab <- function(x,
                               rowRegex,
                               colRegex,
                               rowTargetValue = NULL,
                               colTargetValue = NULL,
                               fillValue = 0,
                               rowColMultiplicationFunction = `*`,
                               rowTargetFunction = `==`,
                               colTargetFunction = `==`,
                               aggregationFunction = `+`,
                               includeValueListsOfMatch = TRUE,
                               excludeParentWhenValueListReturned = TRUE) {
  
  return(
    studyTreeList_to_freqTab(
      x = x$rxsTrees,
      rowRegex = rowRegex,
      colRegex = colRegex,
      rowTargetValue = rowTargetValue,
      colTargetValue = colTargetValue,
      fillValue = fillValue,
      rowColMultiplicationFunction = rowColMultiplicationFunction,
      rowTargetFunction = rowTargetFunction,
      colTargetFunction = colTargetFunction,
      aggregationFunction = aggregationFunction,
      includeValueListsOfMatch = includeValueListsOfMatch,
      excludeParentWhenValueListReturned = excludeParentWhenValueListReturned
    )
  );
  
}

