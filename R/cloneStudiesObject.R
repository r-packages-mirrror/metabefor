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
      function(y) {
        if (!silent) {
          cat("\nCloning ", y, "... ", sep="");
        }
        if (inherits(x$rxsTrees[[y]], "Node")) {
          if (!silent) {
            cat("Cloning studyTree.");
          }
          return(data.tree::Clone(x$rxsTrees[[y]]));
        } else {
          if (!silent) {
            cat("No studyTree - just copying object.");
          }
          return(x$rxsTrees[[y]]);
        }
      }
    );
  
  names(res$rxsTrees) <-
    names(x$rxsTrees);
  
  return(res);
  
}
  