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
#' ### Show the tree for one source as a graph
#' metabefor::renderTreeAsGraph(
#'   example_rxsProject_1$rxsTrees$qurid_7h50rzpq
#' );
#' 
#' ### You can also return the produced Dot specification
#' dot <-
#'   metabefor::renderTreeAsGraph(
#'     example_rxsProject_1$rxsTrees$qurid_7h50rzpq, 
#'     returnDot=TRUE
#'   );
#' 
#' ### Show it (and maybe tweak it)
#' cat(dot);
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
