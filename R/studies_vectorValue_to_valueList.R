#' @rdname vectorValue_to_valueList
#' @export
studies_vectorValue_to_valueList <- function(x,
                                             entityId,
                                             allPossibleValues = NULL) {
  
  x$rxsTrees <-
    lapply(x$rxsTrees,
           studyTree_vectorValue_to_valueList,
           entityId = entityId,
           allPossibleValues = allPossibleValues);
  
  return(x$rxsTrees);
  
}
