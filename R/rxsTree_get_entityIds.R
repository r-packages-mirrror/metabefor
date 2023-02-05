#' Get all identifiers of all entities in an Rxs tree or project
#'
#' This is a wrapper for the `data.tree` method `$Get` to get the names of
#' all nodes in the tree object or project.
#'
#' @param x The rxs tree object or project object.
#'
#' @return A vector with entity names.
#' @export
#'
#' @examples
rxsTree_get_entityIds <- function(x) {
  if (inherits(x, "rxs_parsedExtractionScripts")) {
    return(
      unique(
        unname(
          unlist(
            lapply(
              x$rxsTrees,
              function(x) {
                return(x$Get('name',
                             filterFun = function(node) !data.tree::isRoot(node)));
              }
            )
          )
        )
      )
    );
  } else if (inherits(x, "rxs") && inherits(x, "rxsObject")) {
    return(
      unname(
        unlist(
          x$Get('name',
                filterFun = function(node) !data.tree::isRoot(node))
        )
      )
    );
  } else {
    stop("The object you passed is not an `rxsObject` - it has class(es) ",
         vecTxtQ(class(x)), ".");
  }
}