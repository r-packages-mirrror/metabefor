#' Override default values with bespoke alternatives
#' 
#' This function takes two names lists or two named vectors and replaces
#' all value with a bespoke specification with that bespoke specification.
#'
#' @param default The default values
#' @param bespoke The bespoke alternatives
#'
#' @return A vector that is the union of `default` and `bespoke`, but for
#' all elements that occur in both, the version from `bespoke`.
#' @export
#'
#' @examples override(
#'   default = c(a = "banana",
#'               b = "grape",
#'               c = "pineapple"),
#'   bespoke = c(b = "peach",
#'               c = "apple")
#' );
override <- function(default, bespoke) {
  
  res <- c(default, 
          setdiff(bespoke, default));
  
  overrides <- intersect(bespoke, default);
  
  res[overrides] <- bespoke[overrides];
  
  return(res);
  
}

