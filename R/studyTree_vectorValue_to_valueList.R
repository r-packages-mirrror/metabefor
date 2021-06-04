#' In an entity, convert a vector to a value list
#' 
#' For example, `c('red', 'brown')` becomes `list(red = 1, brown = 1)`.
#'
#' @param x A study tree
#' @param entityId The entity Id
#' @param allPossibleValues A list of all possible values; if provided,
#' the value list will contain all those.
#'
#' @return
#' @rdname vectorValue_to_valueList
#' @export
#'
#' @examples
studyTree_vectorValue_to_valueList <- function(x,
                                               entityId,
                                               allPossibleValues = NULL) {
  
  foundNode <- data.tree::FindNode(
    x,
    entityId
  );
  
  value <- foundNode$value;
  
  valueList <- as.list(rep(1, length(value)));
  
  names(valueList) <- value;
  
  if (!is.null(allPossibleValues)) {
    valueNamesToAdd <-
      allPossibleValues[!(allPossibleValues %in% value)];
    
    valuesToAdd <- as.list(rep(0, length(valueNamesToAdd)));
    
    names(valuesToAdd) <- valueNamesToAdd;

    valueList <-
      c(valueList,
        valuesToAdd);
    
    valueList <-
      valueList[allPossibleValues];
    
  }
  
  foundNode$`__originalValue__` <-
    value;
  
  foundNode$value <-
    valueList
  
  return(invisible(valueList));
  
}