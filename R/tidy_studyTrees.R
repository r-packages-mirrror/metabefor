#' Flatten one or more study trees into a tidy data frame
#'
#' @param x A list of study trees
#' @param studyTree The study tree to convert
#' @param studyName The name of the study
#' @param eC The names of the entity columns
#' @param flattenToString Whether to flatten lists to a string
#' or explode to separate variables.
#' @param silent Whether to be silent or chatty.
#'
#' @return A data frame
#' @rdname tidy_studyTrees
#' @export
tidy_studyTrees <- function(x,
                            eC = metabefor::opts$get('entityColNames'),
                            flattenToString = TRUE,
                            silent = metabefor::opts$get('silent')) {
  
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
        function(treeName) {
          if (!silent) {
            cat0("\n\nStarting to process study tree with name ", treeName, "...");
          }
          return(
            tidy_studyTree(
              studyTree = x[[treeName]],
              studyName = treeName,
              eC = eC,
              flattenToString = flattenToString,
              silent = silent
            )
          );
        }
      )
    );
  
  return(res);
  
}
