#' @rdname rxsTree_get_entityIds
#' @export
rxsProject_get_entityIds <- function(x,
                                     includeClusteredEntities = TRUE) {
  
  return(
    rxsTree_get_entityIds(
      x,
      includeClusteredEntities
    )
  );
  
}