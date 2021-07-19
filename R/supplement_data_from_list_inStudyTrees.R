#' @rdname supplement_data_from_list
#' @export
supplement_data_from_list_inStudyTrees <- function(studies,
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
  
  if (is.null(studies)) {
    if (!silent) {
      cat0("What you passed as `studies` is NULL!");
    }
    return(invisible(NULL));
  }
  
  if (inherits(studies, "rxs_parsedExtractionScripts")) {
    
    if (!silent) {
      cat0("\nYou passed an object with parsed Rxs files. I'm going to ",
           "call myself on each of the ", length(studies$rxsTrees),
           " study trees in this object.\n");
    }
    
    if (is.null(names(studies$rxsTrees))) {
      studyTreeNames <- seq_along(studies$rxsTrees);
    } else {
      studyTreeNames <- names(studies$rxsTrees);
    }
    
    for (i in studyTreeNames) {
      
      if (!silent) {
        cat0("\n\nStarting to process study tree ", i, "...\n");
      }
      
      supplement_data_from_list_inStudyTrees(
        studies = studies$rxsTrees[[i]],
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
      cat0("\n\nDone processing all ", length(studies$rxsTrees),
           "study trees you passed.\n");
    }
    
    return(invisible(studies));
    
  }
  
  if (!(inherits(studies, "Node"))) {
    if (!silent) {
      cat0("What you passed as `studies` is not actually a study tree, ",
           "nor an object with parsed Rxs files that contains a set of ",
           "study trees. Instead, it has class(es) ",
           vecTxtQ(class(studies)), ".\n");
    }
    return(invisible(studies));
  }
  
  ###---------------------------------------------------------------------------
  ### Start looking for the target nodes
  ###---------------------------------------------------------------------------
  
  ### (if we get to this point, `studies` is actually a single study tree)
  
  targetNodes <-
    data.tree::Traverse(
      studies,
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
         ") in this study tree. Starting to process them one by one.\n");
  }

  ### Process each in turn

  for (targetEntityNodeId in targetNodeNames) {

    if (!silent) {
      cat0("\nProcessing ", targetEntityNodeId, "... ");
    }

    studies <-
      supplement_data_from_list_inSingleNode(
        studyTree = studies,
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
    cat0("\nProcessed all entities in this study tree.\n");
  }

  return(invisible(studies));

}
