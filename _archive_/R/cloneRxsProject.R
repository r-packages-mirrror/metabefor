#' Clone a full Rxs project object
#'
#' @param x The full Rxs project object
#' @param silent Whether to be chatty or silent
#'
#' @return The cloned object
#' @export
#'
#' @examples ### Load an example Rxs project
#' data('example_rxsProject_1', package="metabefor");
#'   
#' ### Check whether in this object, value 'hi' exists:
#' example_rxsProject_1$rxsTrees$qurid_7h50rzpq$hi;
#' 
#' ### (should be NULL)
#'
#' ### Try to make a copy 'the conventional way'
#' copy_rxsProject_1 <-
#'   example_rxsProject_1;
#'   
#' ### Change something
#' copy_rxsProject_1$rxsTrees$qurid_7h50rzpq$hi <- "Hello";
#' 
#' ### Check that worked
#' copy_rxsProject_1$rxsTrees$qurid_7h50rzpq$hi;
#' 
#' ### Show how magically stuff also changed in the
#' ### original even though we didn't change anything
#' example_rxsProject_1$rxsTrees$qurid_7h50rzpq$hi;
#'
#' ### Clone it instead
#' copy_rxsProject_1 <-
#'   metabefor::cloneRxsProject(example_rxsProject_1);
#'   
#' ### Make the same change in the copy
#' copy_rxsProject_1$rxsTrees$qurid_7h50rzpq$hi <- "Goodbye";
#' 
#' ### Show how the value in the original didn't change again
#' example_rxsProject_1$rxsTrees$qurid_7h50rzpq$hi;
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
  
