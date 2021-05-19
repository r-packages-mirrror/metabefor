#' Render a tree as a graph
#'
#' @param tree The tree
#' @param theme The theme
#'
#' @return The result of `DiagrammeR::render_graph()`.
#' @export
#'
#' @examples 
renderTreeAsGraph <- function(tree,
                              theme = metabefor::defaultGraphTheme()) {
  
  res <-
    data.tree::ToDiagrammeRGraph(
      tree,
    );
  
  res <-
    metabefor::apply_graph_theme(
      res,
      theme
    );

  DiagrammeR::render_graph(
    res
  );

}
