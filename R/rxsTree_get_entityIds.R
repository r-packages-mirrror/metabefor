#' Get all identifiers of all entities in an Rxs tree or project
#'
#' This is a wrapper for the `data.tree` method `$Get` to get the names of
#' all nodes in the tree object or project.
#'
#' @param x The rxs tree object or project object.
#' @param includeClusteredEntities Whether to also include clustered
#' entities (entities in a `list()` value).
#'
#' @return A vector with entity names.
#' @export
#'
#' @examples ### Load an example Rxs project
#' example_rxsProject <-
#'   metabefor::example_rxsProject;
#'
#' metabefor::rxsProject_get_entityIds(
#'   example_rxsProject
#' );
rxsTree_get_entityIds <- function(x,
                                  includeClusteredEntities = TRUE) {
  
  if (inherits(x, "rxs_parsedExtractionScripts")) {
    
    entityIds <- 
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
      );
    
    if (includeClusteredEntities) {
      
      clusteredIds <-
        unique(
          unname(
            unlist(
              lapply(
                x$rxsTrees,
                function(x) {
                  return(x$Get(
                    function(node) {
                      if (!is.null(node$value) &&
                          is.list(node$value)) {
                        return(names(node$value));
                      } else {
                        return(NULL);
                      }
                    },
                    filterFun = function(node) !data.tree::isRoot(node)));
                }
              )
            )
          )
        );
      
      entityIds <-
        c(entityIds,
          clusteredIds);
      
    }
    
  } else if (inherits(x, "rxs") && inherits(x, "rxsObject")) {
    
    entityIds <- 
      unname(
        unlist(
          x$Get('name',
                filterFun = function(node) !data.tree::isRoot(node))
        )
      );
    
    if (includeClusteredEntities) {
      
      clusteredIds <- 
        unname(
          unlist(
            x$Get(
              function(node) {
                if (!is.null(node$value) &&
                    is.list(node$value)) {
                  return(names(node$value));
                } else {
                  return(NULL);
                }
              },
              filterFun = function(node) !data.tree::isRoot(node))
          )
        );
      
      entityIds <-
        c(entityIds,
          clusteredIds);
      
    }

  } else {
    stop("The object you passed is not an `rxsObject` - it has class(es) ",
         vecTxtQ(class(x)), ".");
  }
  
  return(entityIds);
  
}