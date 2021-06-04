#' Clone a parsed Rxs object (a 'studies object')
#'
#' @param x The studies object
#' @param silent Whether to be chatty or silent
#'
#' @return The cloned object
#' @export
#'
#' @examples
cloneStudiesObject <- function(x,
                               silent = metabefor::opts$get("silent")) {
  
  res <- x;
  
  res$rxsTrees <-
    lapply(
      names(x$rxsTrees),
      function(x) {
        if (!silent) {
          cat("\nCloning ", x, "... ", sep="");
        }
        if (inherits(rxs_raw$rxsTrees[[x]], "Node")) {
          if (!silent) {
            cat("Cloning studyTree.");
          }
          return(data.tree::Clone(rxs_raw$rxsTrees[[x]]));
        } else {
          if (!silent) {
            cat("No studyTree - just copying object.");
          }
          return(rxs_raw$rxsTrees[[x]]);
        }
      }
    );
  
  names(res$rxsTrees) <-
    names(x$rxsTrees);
  
  return(res);
  
}
  