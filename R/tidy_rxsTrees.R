#' Flatten one or more Rxs trees into a tidy data frame
#'
#' @param x A list of Rxs trees
#' @param rxsTree The Rxs tree to convert
#' @param sourceId The source identifier
#' @param flattenToString Whether to flatten lists to a string
#' or explode to separate variables.
#' @param silent Whether to be silent or chatty.
#'
#' @return A data frame
#' @rdname tidy_rxsTrees
#' @export
tidy_rxsTrees <- function(x,
                          flattenToString = TRUE,
                          silent = metabefor::opts$get('silent')) {
  
  eC <- metabefor::opts$get('entityColNames');
  
  if (inherits(x, "rxs_parsedExtractionScripts")) {
    x <- x$rxsTrees;
  } else if (!is.list(x)) {
    stop("As `x`, pass either an `rxs_parsedExtractionScripts` object or ",
         "the `$rxsTrees` stored inside it.");
  }
  
  ### A long (i.e. 'tidy') dataframe, with all values in a column called 'value'
  res <-
    metabefor::rbind_df_list(
      lapply(
        names(x),
        function(sourceId) {
          if (!silent) {
            cat0("\n\nStarting to process Rxs tree for source identifier ",
                 sourceId, "...");
          }
          return(
            tidy_rxsTree(
              rxsTree = x[[sourceId]],
              sourceId = sourceId,
              flattenToString = flattenToString,
              silent = silent
            )
          );
        }
      )
    );
  
  return(res);
  
}
