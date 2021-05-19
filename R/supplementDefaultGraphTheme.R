#' Supplement / edit the default graph theme
#'
#' @param graphTheme Changes, as a list with vectors, where each vector has
#' three elements corresponding to field name, field value, and what to set
#' that setting for, e.g. `c("fontname", "Arial", "graph")`,
#' `c("shape", "box", "node")`, or `c("dir", "none", "edge")`.
#' @param defaultGraphTheme The default graph theme to use / supplement / edit.
#'
#' @return The supplemented / edited graph theme.
#' @export
#'
#' @examples supplementDefaultGraphTheme(
#'   list(
#'     c("dir", "both", "edge")
#'   )
#' );
supplementDefaultGraphTheme <- function (graphTheme,
                                         defaultGraphTheme =
                                           metabefor::defaultGraphTheme()
                                         ) {
  
  if (is.null(graphTheme)) {
    return(defaultGraphTheme);
  }
  
  names(defaultGraphTheme) <-
    unlist(
      lapply(
        defaultGraphTheme,
        function(x) paste0(x[1], "_", x[3])
      )
    );
  
  names(graphTheme) <-
    unlist(
      lapply(
        graphTheme,
        function(x) paste0(x[1], "_", x[3])
      )
    );
  
  ### Complement with default settings that were not overridden
  graphTheme <-
    unname(
      c(graphTheme,
        defaultGraphTheme[
          setdiff(
            names(defaultGraphTheme),
            names(graphTheme)
          )
        ]
      )
    );
  
  return(graphTheme);
  
}