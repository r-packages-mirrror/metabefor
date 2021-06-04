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
  
  if (!(inherits(x, c("rxs", "Node")))) {
    return(invisible(x));
  }

  res <- x;

  foundNode <- data.tree::FindNode(
    res,
    entityId
  );
  
  if (is.null(foundNode)) {
    return(invisible(x));
  }

  value <- foundNode$Get('value');
  
  valueList <- vectorValue_to_valueList(value);

  foundNode$Set(`__originalValue__` = value);
  
  foundNode$value <- valueList;

  return(invisible(res));
  
}
