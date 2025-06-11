#' @rdname vectorValue_to_valueList
#' @export
rxsProject_vectorValue_to_valueList <- function(x,
                                                entityId,
                                                allPossibleValues = NULL) {
  
  x$rxsTrees <-
    rxsTreeList_vectorValue_to_valueList(
      x$rxsTrees,
      entityId = entityId,
      allPossibleValues = allPossibleValues
    );
  
  return(x);
  
}
