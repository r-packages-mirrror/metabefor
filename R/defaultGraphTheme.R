#' Get default graph theme from options
#' 
#' Convenience function that just returns
#' `metabefor::opts$get('defaultGraphTheme')`.
#'
#' @return `metabefor::opts$get('defaultGraphTheme')`
#' @export
#'
#' @examples metabefor::defaultGraphTheme();
defaultGraphTheme <- function() {
  return(
    metabefor::opts$get(
      'defaultGraphTheme'
    )
  );
}
