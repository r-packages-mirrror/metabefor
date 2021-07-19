#' @rdname supplement_data_with_value
#' @export
supplement_data_with_value_inStudyTrees <- function(studies,
                                                    sourceEntityNodeId,
                                                    targetEntityNodeId = NULL,
                                                    targetEntityNode_requiredField = NULL,
                                                    forceCopyingOfExistingValues = FALSE,
                                                    sourcePathString_regex = NULL,
                                                    targetPathString_regex = NULL,
                                                    targetNodeListCreation_prefix = "",
                                                    targetNodeListCreation_suffix = "_value",
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
      
      supplement_data_with_value_inStudyTrees(
        studies = studies$rxsTrees[[i]],
        sourceEntityNodeId = sourceEntityNodeId,
        targetEntityNodeId = targetEntityNodeId,
        targetEntityNode_requiredField = targetEntityNode_requiredField,
        forceCopyingOfExistingValues = forceCopyingOfExistingValues,
        sourcePathString_regex = sourcePathString_regex,
        targetPathString_regex = targetPathString_regex,
        targetNodeListCreation_prefix = targetNodeListCreation_prefix,
        targetNodeListCreation_suffix = targetNodeListCreation_suffix,
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
  
  if (!is.null(targetEntityNodeId)) {
    
    targetNodes <-
      data.tree::Traverse(
        studies,
        filterFun = function(node) {
          return(node$name == targetEntityNodeId);
        }
      );
    
  } else if (!is.null(targetEntityNode_requiredField)) {
    
    targetNodes <-
      data.tree::Traverse(
        studies,
        filterFun = function(node) {
          if (is.null(node$value)) {
            return(FALSE);
          } else if (!is.list(node$value)) {
            return(FALSE);
          } else if (targetEntityNode_requiredField %in% names(node$value)) {
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
    
  } else {
    
    stop("You must pass exactly one of `targetEntityNodeId` and ",
         "`targetEntityNode_requiredField` (where the former takes ",
         "precedence). However, you passed neither.");
    
  }
  
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
         ") in this study tree that matched the path string regex (`",
         targetPathString_regex, "`). Starting to process them one by one.\n");
  }

  ### Process each in turn

  for (targetEntityNodeId in targetNodeNames) {

    if (!silent) {
      cat0("\nProcessing ", targetEntityNodeId, "... ");
    }

    studyTree <-
      supplement_data_with_value_inSingleNode(
        studyTree = studies,
        targetEntityNodeId = targetEntityNodeId,
        sourceEntityNodeId = sourceEntityNodeId,
        forceCopyingOfExistingValues = forceCopyingOfExistingValues,
        sourcePathString_regex = sourcePathString_regex,
        targetPathString_regex = targetPathString_regex,
        targetNodeListCreation_prefix = targetNodeListCreation_prefix,
        targetNodeListCreation_suffix = targetNodeListCreation_suffix,
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
