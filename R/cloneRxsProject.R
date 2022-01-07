#' Clone a full Rxs project object
#'
#' @param x The full Rxs project object
#' @param silent Whether to be chatty or silent
#'
#' @return The cloned object
#' @export
#'
#' @examples
cloneRxsProject <- function(x,
                            silent = metabefor::opts$get("silent")) {
  
  res <- x;
  
  ### First the raw Rxs trees
  
  res$rxsTrees_raw <-
    lapply(
      names(x$rxsTrees_raw),
      function(y) {
        if (!silent) {
          cat("\nCloning raw tree '", y, "'... ", sep="");
        }
        if (inherits(x$rxsTrees_raw[[y]], "Node")) {
          if (!silent) {
            cat("Cloning rxsTree.");
          }
          return(data.tree::Clone(x$rxsTrees_raw[[y]]));
        } else {
          if (!silent) {
            cat("No rxsTree - just copying object.");
          }
          return(x$rxsTrees_raw[[y]]);
        }
      }
    );
  
  names(res$rxsTrees_raw) <-
    names(x$rxsTrees_raw);
  
  ### Then the merged Rxs trees
  
  res$rxsTrees <-
    lapply(
      names(x$rxsTrees),
      function(y) {
        if (!silent) {
          cat("\nCloning merged Rxs tree '", y, "'... ", sep="");
        }
        if (inherits(x$rxsTrees[[y]], "Node")) {
          if (!silent) {
            cat("Cloning rxsTree.");
          }
          return(data.tree::Clone(x$rxsTrees[[y]]));
        } else {
          if (!silent) {
            cat("No rxsTree - just copying object.");
          }
          return(x$rxsTrees[[y]]);
        }
      }
    );
  
  names(res$rxsTrees) <-
    names(x$rxsTrees);
  
  return(res);
  
}
  
