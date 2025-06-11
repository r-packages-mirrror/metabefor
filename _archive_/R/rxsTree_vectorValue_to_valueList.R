#' In an entity, convert a vector to a value list
#' 
#' For example, `c('red', 'brown')` becomes `list(red = 1, brown = 1)`.
#'
#' @param x A Rxs tree
#' @param entityId The entity Id
#' @param allPossibleValues A list of all possible values; if provided,
#' the value list will contain all those.
#'
#' @return The Rxs tree specified in `x`, invisibly.
#' @rdname vectorValue_to_valueList
#' @export
#'
#' @examples ### Load an example Rxs project
#' data('example_rxsProject_1', package="metabefor");
#' 
#' ### Look at the sample size specified in
#' ### an Rxs tree for a source
#' example_rxsProject_1$rxsTrees$qurid_7h50rzmz$methods$sample$sampleSize$value;
#' 
#' ### Set it to a vector
#' example_rxsProject_1$rxsTrees$qurid_7h50rzmz$methods$sample$sampleSize$value <-
#'   c(80, 62);
#' 
#' ### Convert the values to a value list
#' rxsTree_vectorValue_to_valueList(
#'   example_rxsProject_1$rxsTrees$qurid_7h50rzmz,
#'   "sampleSize"
#' );
#' 
#' ### Verify that this happened
#' example_rxsProject_1$rxsTrees$qurid_7h50rzmz$methods$sample$sampleSize$value;
#' 
rxsTree_vectorValue_to_valueList <- function(x,
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
