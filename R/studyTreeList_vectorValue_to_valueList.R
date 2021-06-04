#' @rdname vectorValue_to_valueList
#' @export
studyTreeList_vectorValue_to_valueList <- function(x,
                                                   entityId,
                                                   allPossibleValues = NULL) {

  data.tree::Do(
    x,
    studyTree_vectorValue_to_valueList,
    entityId = entityId,
    allPossibleValues = allPossibleValues
  );

  return(invisible(x));
  
}
