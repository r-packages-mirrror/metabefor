#' @rdname supplement_data_from_list
#' @export
supplement_data_from_lists <- function(rxsTree,
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
  
  if (is.null(rxsTree)) {
    if (!silent) {
      cat0("What you passed as `rxsTree` is NULL!");
    }
    return(invisible(NULL));
  }
  
  if (inherits(rxsTree, "rxs_parsedExtractionScripts")) {
    
    if (!silent) {
      cat0("\nYou passed an object with parsed Rxs files. I'm going to ",
           "call myself on each of the ", length(rxsTree$rxsTrees),
           " Rxs trees in this object.\n");
    }
    
    if (is.null(names(rxsTree$rxsTrees))) {
      rxsTreeNames <- seq_along(rxsTree$rxsTrees);
    } else {
      rxsTreeNames <- names(rxsTree$rxsTrees);
    }
    
    for (i in rxsTreeNames) {
      
      if (!silent) {
        cat0("\n\nStarting to process Rxs tree ", i, "...\n");
      }
      
      supplement_data_from_lists(
        rxsTree = rxsTree$rxsTrees[[i]],
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
    return(invisible(rxsTree));
  }
  
  if (!(inherits(rxsTree, "Node"))) {
    if (!silent) {
      cat0("What you passed as `rxsTree` is not actually a Rxs tree, ",
           "nor an object with parsed Rxs files that contains a set of ",
           "Rxs trees. Instead, it has class(es) ",
           vecTxtQ(class(rxsTree)), ".\n");
    }
    return(invisible(rxsTree));
  }
  
  ###---------------------------------------------------------------------------
  ### Start looking for the target nodes
  ###---------------------------------------------------------------------------
  
  targetNodes <-
    data.tree::Traverse(
      rxsTree,
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
         "). Starting to process them one by one.\n");
  }
    
  ### Process each in turn
  
  for (targetEntityNodeId in targetNodeNames) {
    
    if (!silent) {
      cat0("\nProcessing ", targetEntityNodeId, "... ");
    }
    
    rxsTree <-
      supplement_data_from_list_inSingleNode(
        rxsTree = rxsTree,
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
    cat0("\nProcessed all entities.");
  }
 
  return(invisible(rxsTree)); 
  
}
