#' Flatten one or more study trees into a tidy data frame
#'
#' @param x A list of study trees
#' @param studyTree The study tree to convert
#' @param studyName The name of the study
#' @param eC The names of the entity columns
#' @param flattenToString Whether to flatten lists to a string
#' or explode to separate variables.
#'
#' @return A data frame
#' @rdname tidy_studyTrees
#' @export
tidy_studyTrees <- function(x,
                            eC = metabefor::opts$get('entityColNames'),
                            flattenToString = TRUE) {
  
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
          return(
            tidy_studyTree(
              studyTree = x[[treeName]],
              studyName = treeName,
              eC = eC,
              flattenToString = flattenToString
            )
          );
        }
      )
    );
  
  return(res);
  
}
