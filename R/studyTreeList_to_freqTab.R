#' Create a frequency table from a list of study trees
#'
#' @param x 
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
#' @param includeValueListsOfMatch Whether to also include the value lists
#' inside matching entities (useful for quickly selecting e.g. all
#' results)
#' @param excludeParentWhenValueListReturned Whether, if an entity matches,
#' has a value list as value, and those value lists are returns (i.e.
#' `includeValueListsOfMatch` is `TRUE`), the parent entity (that matched
#' the regular expression) should be excluded.
#'
#' @return
#' @rdname studies_to_freqTab 
#' @export
#'
#' @examples
studyTreeList_to_freqTab <- function(x,
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
                                     includeValueListsOfMatch = TRUE,
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
          studyTree_to_freqTab(
            x[[i]],
            rowRegex = rowRegex,
            colRegex = colRegex,
            rowLabels = rowLabels,
            colLabels = colLabels,
            rowColMultiplicationFunction = rowColMultiplicationFunction,
            includeValueListsOfMatch = includeValueListsOfMatch,
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
