#' Show an imported Rxs structure
#' 
#' This function shows the Rxs structure that was imported from
#' an Rxs specification
#'
#' @param x The parsed Rxs specifications as read
#' with [metabefor::rxs_fromSpecifications()].
#' @param output,outputModuleFilename When not NULL, pass a path to a 
#' filename where the RxsTree should be written to. If multiple modules were
#' read, the filename is determine by `outputModuleFilename`, where `%s` will
#' be replaced with the module name.
#' @param headingLevel The level of the heading to use is multiple modules
#' were specified.
#'
#' @return Invisibly, x.
#' @export
#'
#' @examples
show_rxsTree_in_rxsStructure <- function(x,
                                         output = NULL,
                                         outputModuleFilename = "extraction-tree--simple--%s.pdf",
                                         headingLevel=3) {
  
  if (!inherits(x, "rxsStructure")) {
    stop("As `x`, you have to pass an object with an Rxs structure, i.e., ",
         "parsed Rxs specifications as read with `rxs_fromSpecifications()`.",
         "Instead, you passed an object of class(es) ", vecTxtQ(class(x)), ".");
  }
  
  if ("rxsStructure" %in% names(x)) {
    
    print(x$rxsStructure$parsedEntities$extractionScriptTree);
    
    if (isTRUE(getOption('knitr.in.progress'))) {
      print(metabefor::knitDiagram(x$rxsTreeDiagram_simple));
    } else {
      print(DiagrammeR::render_graph(x$rxsTreeDiagram_simple));
    }
    
    if (!is.null(output)) {
      DiagrammeR::export_graph(
        x$rxsTreeDiagram_simple,
        output
      );
    }

    return(invisible(x));
    
  } else if ("rxsStructures" %in% names(x)) {
    
    if (!is.null(output)) {
      if (!dir.exists(output)) {
        output <- dirname(output);
      }
    }
    
    for (currentModule in x$rxsStructures) {
      
      metabefor::heading(currentModule, headingLevel = headingLevel);
      
      print(x$rxsStructures[[currentModule]]$parsedEntities$extractionScriptTree);
      
      if (isTRUE(getOption('knitr.in.progress'))) {
        print(metabefor::knitDiagram(x$rxsTreeDiagrams_simple[[currentModule]]));
      } else {
        print(DiagrammeR::render_graph(x$rxsTreeDiagrams_simple[[currentModule]]));
      }

      if (!is.null(output)) {
        DiagrammeR::export_graph(
          x$rxsTreeDiagram_simple,
          file.path(
            output,
            sprintf(outputModuleFilename, currentModule)
          )
        );
      }
      
    }

    return(invisible(x));
    
  } else {
    stop("In `x`, despite it having the correct class ('rxsStructure'), ",
         "I cannot find the Rxs structure itself, which should be stored ",
         "with 'rxsStructure' or 'rxsStructures'. Instead, `x` contains ",
         "objects with names ", vecTxtQ(names(x)), ".");
  }
  
}