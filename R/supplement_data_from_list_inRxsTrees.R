#' @rdname supplement_data_from_list
#' @export
supplement_data_from_list_inRxsTrees <- function(x,
                                                 sourceEntityNodeIdField_in_targetEntity,
                                                 idField_in_targetEntityNode = NULL,
                                                 idField_in_sourceEntityNode = NULL,
                                                 fieldsToCopy_regex = NULL,
                                                 sourcePathString_regex = NULL,
                                                 targetPathString_regex = NULL,
                                                 forceCopyingOfExistingValues = FALSE,
                                                 prefix = "supplemented_",
                                                 suffix = "",
                                                 silent = metabefor::opts$get("silent")) {
  
  if (is.null(x)) {
    if (!silent) {
      cat0("What you passed as `x` is NULL!");
    }
    return(invisible(NULL));
  }
  
  if (inherits(x, "rxs_parsedExtractionScripts")) {
    
    if (!silent) {
      cat0("\nYou passed an object with parsed Rxs files. I'm going to ",
           "call myself on each of the ", length(x$rxsTrees),
           " Rxs trees in this object.\n");
    }
    
    if (is.null(names(x$rxsTrees))) {
      rxsTreeNames <- seq_along(x$rxsTrees);
    } else {
      rxsTreeNames <- names(x$rxsTrees);
    }
    
    for (i in rxsTreeNames) {
      
      if (!silent) {
        cat0("\n\nStarting to process Rxs tree ", i, "...\n");
      }
      
      supplement_data_from_list_inRxsTrees(
        x = x$rxsTrees[[i]],
        sourceEntityNodeIdField_in_targetEntity = sourceEntityNodeIdField_in_targetEntity,
        idField_in_targetEntityNode = idField_in_targetEntityNode,
        idField_in_sourceEntityNode = idField_in_sourceEntityNode,
        fieldsToCopy_regex = fieldsToCopy_regex,
        sourcePathString_regex = sourcePathString_regex,
        targetPathString_regex = targetPathString_regex,
        forceCopyingOfExistingValues = forceCopyingOfExistingValues,
        prefix = prefix,
        suffix = suffix,
        silent = silent
      );
    }
    
    if (!silent) {
      cat0("\n\nDone processing all ", length(x$rxsTrees),
           " Rxs trees you passed.\n");
    }
    
    return(invisible(x));
    
  }
  
  if (!(inherits(x, "Node"))) {
    if (!silent) {
      cat0("What you passed as `x` is not actually an Rxs tree, ",
           "nor an object with parsed Rxs files that contains a set of ",
           "Rxs trees. Instead, it has class(es) ",
           vecTxtQ(class(x)), ".\n");
    }
    return(invisible(x));
  }
  
  ###---------------------------------------------------------------------------
  ### Start looking for the target nodes
  ###---------------------------------------------------------------------------
  
  ### (if we get to this point, `x` is actually a single Rxs tree)
  
  targetNodes <-
    data.tree::Traverse(
      x,
      filterFun = function(node) {
        if (is.null(node$value)) {
          return(FALSE);
        } else if (!is.list(node$value)) {
          return(FALSE);
        } else if (sourceEntityNodeIdField_in_targetEntity %in% names(node$value)) {
          if (is.null(targetPathString_regex)) {
            return(TRUE);
          } else {
            return(
              grepl(
                targetPathString_regex,
                node$pathString
              )
            );
          }
        } else {
          return(FALSE);
        }
      }
    );

  targetNodeNames <-
    unlist(
      lapply(
        targetNodes,
        function(node) {
          return(node$name)
        }
      )
    );

  if (!silent) {
    cat0("\nFound ", length(targetNodeNames),
         " target entity node identifiers (",
         vecTxtQ(targetNodeNames),
         ") in this Rxs tree. Starting to process them one by one.\n");
  }

  ### Process each in turn

  for (targetEntityNodeId in targetNodeNames) {

    if (!silent) {
      cat0("\nProcessing ", targetEntityNodeId, "... ");
    }

    x <-
      supplement_data_from_list_inSingleNode(
        rxsTree = x,
        targetEntityNodeId = targetEntityNodeId,
        sourceEntityNodeIdField_in_targetEntity = sourceEntityNodeIdField_in_targetEntity,
        idField_in_targetEntityNode = idField_in_targetEntityNode,
        idField_in_sourceEntityNode = idField_in_sourceEntityNode,
        fieldsToCopy_regex = fieldsToCopy_regex,
        sourcePathString_regex = sourcePathString_regex,
        targetPathString_regex = targetPathString_regex,
        forceCopyingOfExistingValues = forceCopyingOfExistingValues,
        prefix = prefix,
        suffix = suffix,
        silent = silent
      );

  }

  if (!silent) {
    cat0("\nProcessed all entities in this Rxs tree.\n");
  }

  return(invisible(x));

}
