#' Convert a vector value to a value list
#'
#' @param value The vector value to convert
#' @param allPossibleValues A list of all possible values
#' @param labels Optionally, a named vector with labels to replace the actual
#' values with; names are values, values are labels.
#' @param valueForPresence Value to assign for the elements in the vector
#' @param valueForAbsence Value to assign for the elements
#' in `allPossibleValues` that do not occur in the vector
#'
#' @return The list
#' @export
#'
#' @examples
vectorValue_to_valueList <- function(value,
                                     allPossibleValues = NULL,
                                     labels = NULL,
                                     valueForPresence = 1,
                                     valueForAbsence = 0) {
  
  if (is.list(value)) {
    return(invisible(value));
  }
  
  valueList <- as.list(rep(valueForPresence, length(value)));
  
  if (!is.null(labels)) {
    valueList <- labels[valueList];
  }
  
  names(valueList) <- value;
  
  if (!is.null(allPossibleValues)) {
    valueNamesToAdd <-
      allPossibleValues[!(allPossibleValues %in% value)];
    
    valuesToAdd <- as.list(rep(valueForAbsence, length(valueNamesToAdd)));
    
    names(valuesToAdd) <- valueNamesToAdd;
    
    valueList <-
      c(valueList,
        valuesToAdd);
    
    valueList <-
      valueList[allPossibleValues];
    
  }
  
  return(valueList);
  
}