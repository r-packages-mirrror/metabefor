#' Get a single value from a Rxs tree, list of trees, or full Rxs project object
#'
#' @param x The tree, tree list, or full Rxs project object.
#' @param entityId The entity identifier of the value to get
#' @param lookInValueLists Whether to also look inside value lists
#' @param returnDf Whether to return a data frame or not
#' @param pathString_regex_select Regex that the target entities' path strings
#' have to match (otherwise, the entity is excluded)
#' @param flattenVectorsInDf The default action to apply for values not matching
#' one of `pathString_regex_flatten` and `pathString_regex_explode`: to flatten
#' by default, pass `TRUE`; to explode by default, pass `FALSE`.
#' @param pathString_regex_flatten,pathString_regex_explode Regular expressions
#' matched against each entity node's path string (i.e. its path from the root,
#' delimited by slashes). Vectors in entity nodes matching
#' `pathString_regex_flatten` will be flattened into a single character
#' string value; vectors in entity nodes matching `pathString_regex_explode`
#' will be exploded into multiple rows.
#' @param fieldname_regex_alwaysFlatten A regular expression to force
#' flattening of fields regardless of matching to other regular expressions.
#' @param returnLongDf Whether to return a long ('tidy') data frame, with all
#' values on a separate row, or a wide ('multivariate') data frame, where
#' columns represent different variables. Setting this to `FALSE` is not yet
#' supported.
#' @param nullValue,naValue Values to return when encountering `NULL` or `NA`.
#' @param warningValues Values to return warnings for.
#' @param warningFunctions A list of functions to run on the values that can
#' return a character value - if they do, that is shown as a warning or printed
#' (depending on the value of `silent`).
#' @param silent Whether to be silent or chatty.
#'
#' @return A list or a dataframe (if `returnDf` is `TRUE`)
#'
#' @export
#' 
#' @rdname get_singleValue
#'
rxsProject_filter <- function(x,
                              filterCriteria,
                              lookInValueLists = TRUE,
                              silent = metabefor::opts$get("silent")) {
  
  if (!inherits(x, "rxs_parsedExtractionScripts")) {
    stop("With metabefor::rxsProject_filter(), you can only filter ",
         "an entire Rxs project. However, you passed on object of ",
         "class(es) ", vecTxtQ(class(x)), ".");
  } 

  
  
  df <- metabefor::get_singleValue(
    x = x,
    entityId = entityId,
    returnDf = TRUE,
    returnLongDf = TRUE
  );
  
  
  
  
}
