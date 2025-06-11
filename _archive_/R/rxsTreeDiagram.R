rxsTreeDiagram <- function(extractionScriptTree,
                           graphTheme) {
  
  rxsTreeDiagram_simple_prep <-
    data.tree::Clone(
      extractionScriptTree
    );

  rxsTreeDiagram_simple_prep$Do(
    function(node) {
      data.tree::SetNodeStyle(
        node,
        label =
          sanitize_for_DiagrammeR(
            node$title
          )
      );
    }
  );
  
  rxsTreeDiagram_simple <-
    data.tree::ToDiagrammeRGraph(
      rxsTreeDiagram_simple_prep
    );
  
  graphTheme <-
    supplementDefaultGraphTheme(
      graphTheme
    );
  
  rxsTreeDiagram_simple <-
    do.call(
      apply_graph_theme,
      c(
        list(graph = rxsTreeDiagram_simple),
        graphTheme
      )
    );
  
  return(rxsTreeDiagram_simple);
  
}