#' @rdname add_aggregationTree_information
#' @export
add_aggregationTree_information <- function(studies,
                                            aggregationTree,
                                            fieldName,
                                            prefixes = NULL,
                                            suffixes = NULL) {
  
  for (i in seq_along(studies$rxsTrees)) {
    if (inherits(studies$rxsTrees[[i]], "Node")) {
      add_aggregationTree_information_toStudyTree(
        studyTree = studies$rxsTrees[[i]],
        aggregationTree = aggregationTree,
        fieldName = fieldName,
        prefixes = prefixes,
        suffixes = suffixes
      );
    }
  }
  
  return(invisible(studies));
  
}
