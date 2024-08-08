#' Render a tree as a graph
#'
#' @param tree The tree
#' @param theme The theme
#' @param returnDot Whether to return the specification in Dot.
#'
#' @return The result of `DiagrammeR::render_graph()`, or, if `returnDot=TRUE`,
#' a character vector with the specification in Dot.
#' @export
#'
#' @examples ### Load an example Rxs project
#' data('example_rxsProject_1', package="metabefor");
#' 
#' ### Return the produced Dot specification
#' dot <-
#'   metabefor::renderTreeAsGraph(
#'     example_rxsProject_1$rxsTrees$qurid_7h50rzpq, 
#'     returnDot=TRUE
#'   );
#' 
#' ### Show it
#' cat(dot);
#' 
#' ### Some Dot parsers do not accept single quotes;
#' ### at the time of writing, that is what the
#' ### underlying DiagrammeR function produces, so we
#' ### need to replace the single quotes with double ones.
#' doubleDot <-
#'   gsub("'", '"', dot);
#'
#' ### Show that result:
#' cat(doubleDot);
#' 
#' ### Render it
#' DiagrammeR::grViz(doubleDot);
renderTreeAsGraph <- function(tree,
                              theme = metabefor::defaultGraphTheme(),
                              returnDot = FALSE) {
  
  res <-
    data.tree::ToDiagrammeRGraph(
      tree,
    );
  
  res <-
    metabefor::apply_graph_theme(
      res,
      theme
    );
  
  if (returnDot) {
    DiagrammeR::generate_dot(
      res
    );
  } else {
    DiagrammeR::render_graph(
      res
    );
  }

}
