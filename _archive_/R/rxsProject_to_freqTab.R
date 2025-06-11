#' @rdname freqTab_heatMaps
#' @export
#' @examples ### Load an example Rxs Project
#' example_rxsProject_1 <- metabefor::example_rxsProject_1;
#' 
#' ### Create a crosstable with frequencies
#' metabefor::rxsProject_to_freqTab(
#'   example_rxsProject_1,
#'   "sourceAuthors",
#'   "publicationYear"
#' );
rxsProject_to_freqTab <- function(x,
                                  rowRegex,
                                  colRegex,
                                  rowTargetValue = NULL,
                                  colTargetValue = NULL,
                                  fillValue = 0,
                                  rowColMultiplicationFunction = `*`,
                                  rowTargetFunction = `==`,
                                  colTargetFunction = `==`,
                                  aggregationFunction = `+`,
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

  return(
    rxsTreeList_to_freqTab(
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
      rowLabels = rowLabels,
      colLabels = colLabels,
      rowOrder = rowOrder,
      colOrder = colOrder,
      sortRowsAlphabetically = sortRowsAlphabetically,
      sortColsAlphabetically = sortColsAlphabetically,
      flattenValues = flattenValues,
      includeValueListsOfMatch = includeValueListsOfMatch,
      excludeParentWhenValueListReturned = excludeParentWhenValueListReturned,
      silent = silent
    )
  );
  
}

