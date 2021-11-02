#' Apply multiple DiagrammeR global graph attributes
#'
#' @param graph The [DiagrammeR::DiagrammeR] graph to apply the attributes to.
#' @param ... One or more character vectors of length three, where the first element is
#' the attribute, the second the value, and the third, the attribute type (`graph`,
#' `node`, or `edge`).
#'
#' @return The [DiagrammeR::DiagrammeR] graph.
#' @examples ### Dus
#' #miniGraph <-
#'  # apply_graph_theme(data.tree::ToDiagrammeRGraph(parsedSource$deductiveCodeTrees),
#'   #                  c("color", "#0000AA", "node"),
#'    #                 c("shape", "triangle", "node"),
#'     #                c("fontcolor", "#FF0000", "node"));
#' \donttest{
#' # DiagrammeR::render_graph(miniGraph);
#' }
#' @export
apply_graph_theme <- function(graph,
                              ...) {
  
  settingList <- list(...);
  
  if ((length(settingList) == 1) && is.list(settingList[[1]])) {
    settingList <- settingList[[1]];
  }
  
  for (currentSetting in settingList) {
    if ((length(currentSetting) != 3) && is.character(currentSetting)) {
      stop("Only provide character vectors of length 3 in the dots (...) argument!");
    } else {
      graph <-
        DiagrammeR::add_global_graph_attrs(graph,
                                           currentSetting[1],
                                           currentSetting[2],
                                           currentSetting[3]);
    }
  }
  return(graph);
}
