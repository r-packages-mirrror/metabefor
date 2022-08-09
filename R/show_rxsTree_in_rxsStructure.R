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
#' @return Invisibly; or, when knitting, an object that knitr will print.
#' @export
#'
#' @examples
show_rxsTree_in_rxsStructure <- function(x,
                                         output = NULL,
                                         outputModuleFilename = "extraction-tree--simple--%s.pdf",
                                         headingLevel=3) {
  
  if (!(inherits(x, "rxsStructure") || inherits(x, "rxsStructures")) {
    stop("As `x`, you have to pass an object with an Rxs structure, i.e., ",
         "parsed Rxs specifications as read with `rxs_fromSpecifications()`.",
         "Instead, you passed an object of class(es) ", vecTxtQ(class(x)), ".");
  }
  
  knitSingleTree <- function(extractionScriptTree,
                             rxsTreeDiagram_simple) {
    res <-
      paste0(
        "<pre>",
        paste0(
          capture.output(
            print(extractionScriptTree)),
          collapse="\n"
        ),
        "</pre>\n\n",
        knitDiagram(rxsTreeDiagram_simple)
      );
    return(res);
  }
  
  if ("rxsStructure" %in% names(x)) {
    
    if (!is.null(output)) {
      DiagrammeR::export_graph(
        x$rxsTreeDiagram_simple,
        output
      );
    }
    
    if (isTRUE(getOption('knitr.in.progress'))) {
      return(
        knitr::asis_output(
          knitSingleTree(
            x$rxsStructure$parsedEntities$extractionScriptTree,
            x$rxsTreeDiagram_simple
          )
        )
      );
    } else {
      print(x$rxsStructure$parsedEntities$extractionScriptTree);
      print(DiagrammeR::render_graph(x$rxsTreeDiagram_simple));
      return(invisible(x));
    }

  } else if ("rxsStructures" %in% names(x)) {
    
    if (!is.null(output)) {
      if (!dir.exists(output)) {
        output <- dirname(output);
      }
    }
    
    if (isTRUE(getOption('knitr.in.progress'))) {
      res <- "";
    }
    
    for (currentModule in names(x$rxsStructures)) {

      if (isTRUE(getOption('knitr.in.progress'))) {
        browser();
        res <- paste0(
          res,
          metabefor::heading(currentModule, headingLevel = headingLevel, cat=FALSE),
          knitSingleTree(
            x$rxsStructures[[currentModule]]$parsedEntities$extractionScriptTree,
            x$rxsTreeDiagrams_simple[[currentModule]]
          )
        );
      } else {
        metabefor::heading(currentModule, headingLevel = headingLevel);
        print(x$rxsStructures[[currentModule]]$parsedEntities$extractionScriptTree);
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

    if (isTRUE(getOption('knitr.in.progress'))) {
      return(
        knitr::asis_output(
          res
        )
      );
    } else {
      return(invisible(x));
    }
    
  } else {
    stop("In `x`, despite it having the correct class ('rxsStructure'), ",
         "I cannot find the Rxs structure itself, which should be stored ",
         "with 'rxsStructure' or 'rxsStructures'. Instead, `x` contains ",
         "objects with names ", vecTxtQ(names(x)), ".");
  }
  
}