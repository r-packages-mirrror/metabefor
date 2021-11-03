#' Flatten one or more node values
#' 
#' "Flattening" a ndoe value means pasting together all values in the vector
#' to one character value.
#'
#' @param singleValue The single node value
#' @param x A list of node values
#'
#' @return A character value, or a vector of values.
#' @export
#'
#' @rdname flatteningNodeValues
#' 
#' @examples metabefor::flattenNodeValue(1:8);
#' 
#' metabefor::flattenNodeValues(
#'   list(
#'     1:8,
#'     letters[1:4],
#'     LETTERS[10:17]
#'   )
#' );
flattenNodeValue <- function(singleValue) {
  
  if (is.expression(singleValue)) {
    singleValue <-
    tryCatch({
      return(eval(singleValue));
    }, error = function(e) {
      return(paste0("Expression ", deparse(substitute(singleValue)),
                    " returned error '", e$message, "' when executed."));
    });
  }
  
  if (is.null(singleValue)) {
    return(NULL);
  } else if (all(is.na(singleValue))) {
    return(NA);
  } else if (length(singleValue) == 1) {
    return(singleValue);
  } else if (is.vector(singleValue)) {
    return(vecTxtQ(singleValue));
  } else if (is.matrix(singleValue)) {
    return(paste(apply(singleValue, 1, vecTxtQ), collapse="\n"));
  } else {
    return(utils::capture.output(utils::str(singleValue)));
  }
}
