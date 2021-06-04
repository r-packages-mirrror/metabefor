#' @rdname vectorValue_to_valueList
#' @export
studies_vectorValue_to_valueList <- function(x,
                                             entityId,
                                             allPossibleValues = NULL) {
  
  x$rxsTrees <-
    studyTreeList_vectorValue_to_valueList(
      x$rxsTrees,
      entityId = entityId,
      allPossibleValues = allPossibleValues
    );
  
  return(x);
  
}
