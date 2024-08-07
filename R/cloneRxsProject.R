#' Clone a full Rxs project object
#'
#' @param x The full Rxs project object
#' @param silent Whether to be chatty or silent
#'
#' @return The cloned object
#' @export
#'
#' @examples ### Load an example Rxs project
#' example_rxsProject <-
#'   metabefor::example_rxsProject;
#'   
#' ### Check whether in this object, value 'hi' exists:
#' example_rxsProject$rxsTrees$qurid_7h50rzpq$hi;
#' 
#' ### (should be NULL)
#'
#' ### Try to make a copy 'the conventional way'
#' copy_rxsProject <-
#'   example_rxsProject;
#'   
#' ### Change something
#' copy_rxsProject$rxsTrees$qurid_7h50rzpq$hi <- "Hello";
#' 
#' ### Check that worked
#' copy_rxsProject$rxsTrees$qurid_7h50rzpq$hi;
#' 
#' ### Show how magically stuff also changed in the
#' ### original even though we didn't change anything
#' example_rxsProject$rxsTrees$qurid_7h50rzpq$hi;
#'
#' ### Clone it instead
#' copy_rxsProject <-
#'   metabefor::cloneRxsProject(example_rxsProject);
#'   
#' ### Make thes ame change in the copy
#' copy_rxsProject$rxsTrees$qurid_7h50rzpq$hi <- "Goodbye";
#' 
#' ### Show how the value in the original didn't change again
#' example_rxsProject$rxsTrees$qurid_7h50rzpq$hi;
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
  
