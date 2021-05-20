#' Title
#'
#' @param studyTree THe study tree
#' @param targetNode_id The identifier of the target node (the node to
#' supplement)
#' @param targetNode_sourceNodeIdField Inside the target node, the field name
#' holding that identifier of the source node (the node supplying the data)
#' @param idField_in_TargetNode `NULL` if the target node's name is also its
#' identifier; otherwise, the name of the field containing the identifier in
#' the target node.
#' @param idField_in_sourceNode `NULL` if the source node's name is also its
#' identifier; otherwise, the name of the field containing the identifier in
#' the source node.
#'
#' @return
#' @export
#'
#' @examples
supplement_data_from_list <- function(studyTree,
                                      targetNode_id,
                                      targetNode_sourceNodeIdField,
                                      idField_in_sourceNode = NULL,
                                      idField_in_targetNode = NULL) {
  
  if (is.null(idField_in_sourceNode)) {
    
    
    
  } else {
    
    ### The `sourceNode_idField`
    
  }
  
}
                                      