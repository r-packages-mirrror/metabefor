#' Create a heatmap or frequency table from a list of Rxs trees
#'
#' These functions create a heatmap showing the frequencies with which
#' specific possible entity values occur for the intersection of two entities -
#' or they create the underlying frequency table for the set of sources or just
#' one source. You usually only use `heatMap_from_rxsProject`, which takes the
#' object with Rxs trees and creates a heatmap.
#' 
#' The underlying frequency table is produced in a number of steps. First,
#' the values of the entities matching the regular expressions are obtained.
#' Second, the row and column target functions are applied to these values (as 
#' first argument) and to the specified row and column target values (as second
#' argument). Then, the `rowColMultiplicationFunction` is applied to the
#' result to obtain the value for the cell. Finally, if `rxs_to_freqTab`
#' is called, `aggregationFunction` is called on the frequency tables of the
#' separate sources.
#'
#' @param x The Rxs object or the Rxs tree for which to produce the
#' frequency table.
#' @param rowRegex,colRegex Regular expressions used to find the entities
#' that will form the rows or columns
#' @param rowTargetValue,colTargetValue Value to consider a 'hit' for the
#' row and column entities
#' @param fillValue The value to insert in rows or columns that have to
#' be added because of inconsistencies between extraction scripts
#' @param rowColMultiplicationFunction The function to use to multiply the
#' rows and columns into the matrix
#' @param rowTargetFunction,colTargetFunction Functions to use to compare,
#' the entity values found for the rows and columns to the target
#' values (e.g. `==`, `>`, `<`, etc).
#' @param aggregationFunction The function to use to aggregate matrices
#' @param rowLabels,colLabels A names vector used to replace row and column
#' labels; the indices (element names) should be the entity identifiers and
#' the values the labels to use.
#' @param rowOrder,colOrder The order of the row and column entities/values.
#' @param sortRowsAlphabetically,sortColsAlphabetically Whether to sort
#' columns or rows alphabetically.
#' @param includeValueListsOfMatch Whether to also include the value lists
#' inside matching entities (useful for quickly selecting e.g. all
#' results)
#' @param flattenValues Whether to flatten values that are vectors into
#' single string values that represents those values, or not (in which
#' case they remain vectors, and so single sources occur multiple times
#' in the frequency table).
#' @param excludeParentWhenValueListReturned Whether, if an entity matches,
#' has a value list as value, and those value lists are returns (i.e.
#' `includeValueListsOfMatch` is `TRUE`), the parent entity (that matched
#' the regular expression) should be excluded.
#' @param valuePreprocessingFunction The function to use to preprocess
#' values - only for advanced use.
#' @param freqTabArgs Arguments to pass to the frequency table functions.
#' @param xLabelRotationAngle The angel to rotate the labels on the X axis.
#' @param legend.position The position of the legend.
#' @param silent Whether to be silent or chatty.
#'
#' @return A ggplot or a frequency table
#' @rdname freqTab_heatMaps 
#' @export
rxsTreeList_to_freqTab <- function(x,
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
  
  usableElements <-
    unlist(
      lapply(
        x,
        function(singleX) {
          return(
            inherits(singleX, "rxs") && inherits(singleX, "Node")
          );
        }
      )
    );
  
  if (any(!usableElements)) {
    warning("Some rxsTrees are invalid, specifically: ",
            metabefor::vecTxtQ(names(usableElements)[which(!usableElements)]),
            ".");
  }
  
  x <- x[usableElements];
  
  res <-
    lapply(
      names(x),
      function(i) {
        if (!silent) {
          cat("\nProcessing ", i, "...");
        }
        return(
          rxsTree_to_freqTab(
            x[[i]],
            rowRegex = rowRegex,
            colRegex = colRegex,
            rowLabels = rowLabels,
            colLabels = colLabels,
            rowOrder = rowOrder,
            colOrder = colOrder,
            sortRowsAlphabetically = sortRowsAlphabetically,
            sortColsAlphabetically = sortColsAlphabetically,
            rowColMultiplicationFunction = rowColMultiplicationFunction,
            includeValueListsOfMatch = includeValueListsOfMatch,
            flattenValues = flattenValues,
            excludeParentWhenValueListReturned = excludeParentWhenValueListReturned,
            silent = silent
          )
        );
      }
    );
  
  if (!is.null(res)) {
    names(res) <- names(x);
  }

  res <-
    maximizeMatrices(
      res,
      fillValue = fillValue
    );
  
  res <-
    Reduce(aggregationFunction, res);
  
  return(res);
  
}
