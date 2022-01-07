#' @rdname vectorValue_to_valueList
#' @export
rxsTreeList_vectorValue_to_valueList <- function(x,
                                                 entityId,
                                                 allPossibleValues = NULL) {

  data.tree::Do(
    x,
    rxsTree_vectorValue_to_valueList,
    entityId = entityId,
    allPossibleValues = allPossibleValues
  );

  return(invisible(x));
  
}
