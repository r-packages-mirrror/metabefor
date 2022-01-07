#' Supplement a target entity node with a value from another entity node
#' 
#' This function supplements an entity node with a value from another entity
#' node. Note that the source entity *cannot be* a clustering entity (i.e.
#' the value stored for that entity must be a single value); and the target
#' entity *must* be a clustering entity (i.e. the value stored for that entity
#' must be a list of values for clustered entities).
#' Whereas for `supplement_data_with_value_inSingleNode()`, you have to specify
#' the entity identifier for the entity you want to supplement, for
#' `supplement_data_with_value_inRxsTrees()`, use `targetPathString_regex` to
#' specify which entities you would like to supplement (i.e. copy the relevant
#' value to). Of course, this can also be an entity identifier if you only want
#' to copy to a single entity but in all Rxs trees in an object with parsed
#' Rxs files.
#'  
#' @param rxsTree,x The Rxs tree or the Rxs project object.
#' @param targetEntityNodeId The identifier of the target entity node (the
#' entity node to supplement)
#' @param sourceEntityNodeId The identifier of the source entity node.
#' @param prefix,suffix A text string to prepend and append to the entity
#' identifier when copying it to the target entity.
#' @param sourcePathString_regex,targetPathString_regex Regular expressions
#' that must match the path string of the source of target node.
#' @param forceCopyingOfExistingValues Whether to force copying (and so
#' overwriting) existing values if encountered in the target entity. If
#' `FALSE`, existing values will not be overwritten.
#' @param targetNodeListCreation_prefix,targetNodeListCreation_suffix A prefix
#' and suffix to add when a list is created.
#' @param targetEntityNode_requiredField A required field that, if it does not
#' occur in the target entity, causes that target entity to be skipped. This
#' provides additional control over which target entities to process.
#' @param silent Whether to be quiet or chatty.
#'
#' @return Invisibly, the (altered) Rxs tree - but note that since
#' the `data.tree` package uses `R6`'s pass-by-reference semantics, that object
#' is altered in place, and sothe returned object can be discarded.
#' @rdname supplement_data_with_value
#' @export
#'
#' @examples
supplement_data_with_value_inSingleNode <- function(rxsTree,
                                                    targetEntityNodeId,
                                                    sourceEntityNodeId,
                                                    forceCopyingOfExistingValues = FALSE,
                                                    sourcePathString_regex = NULL,
                                                    targetPathString_regex = NULL,
                                                    targetNodeListCreation_prefix = "",
                                                    targetNodeListCreation_suffix = "_value",
                                                    prefix = "supplemented_",
                                                    suffix = "",
                                                    silent = metabefor::opts$get("silent")) {
  
  if (is.null(rxsTree)) {
    if (!silent) {
      cat0("What you passed as `rxsTree` is NULL!");
    }
    return(invisible(NULL));
  }
  
  if (!(inherits(rxsTree, "Node"))) {
    if (!silent) {
      cat0("What you passed as `rxsTree` is not actually a Rxs tree!");
    }
    return(invisible(rxsTree));
  }
  
  ###---------------------------------------------------------------------------
  ### Start looking for the target node
  ###---------------------------------------------------------------------------
  
  targetNode <- data.tree::FindNode(
    rxsTree,
    targetEntityNodeId
  );
    
  ###---------------------------------------------------------------------------
  ### Check whether a node was found
  ###---------------------------------------------------------------------------
  
  if (is.null(targetNode)) {
    if (!silent) {
      cat0("The rxsTree you passed does not contain the node you ",
           "specified as target entity node (", targetEntityNodeId, ").");
    }
    return(invisible(rxsTree));
  }
  
  ###---------------------------------------------------------------------------
  ### Check for a match with the pathstring regex, if provided
  ###---------------------------------------------------------------------------

  if (!is.null(targetPathString_regex)) {
    if (!grepl(targetPathString_regex, targetNode$pathString)) {
      if (!silent) {
        cat0("The rxsTree you passed contains the node you ",
             "specified as target entity node (", targetEntityNodeId, "),",
             " but its path string does not match the regular expression ",
             "you passed ('", targetPathString_regex, "').");
      }
      return(invisible(rxsTree));
    }
  }

  ###---------------------------------------------------------------------------
  ### Start looking for the source node
  ###---------------------------------------------------------------------------

  sourceNode <- data.tree::FindNode(
    rxsTree,
    sourceEntityNodeId
  );

  ###---------------------------------------------------------------------------
  ### Check whether a source node was found
  ###---------------------------------------------------------------------------
  
  if (is.null(sourceNode)) {
    if (!silent) {
      cat("In the rxsTree you passed, I found the target entity node you ",
          "specified, but I could not find the source entity node you ",
          "specified (", sourceEntityNodeId, ") in the rxsTree you passed.");
    }
    return(invisible(rxsTree));
  }
  
  ###---------------------------------------------------------------------------
  ### Check for a match with the pathstring regex, if provided
  ###---------------------------------------------------------------------------
  
  if (!is.null(sourcePathString_regex)) {
    if (!grepl(sourcePathString_regex, sourceNode$pathString)) {
      if (!silent) {
        cat0("In the rxsTree you passed, I found both the target entity node ",
             "and the source entity node you specified.",
             "However, the source entity path string does not ",
             "match the regular expression you passed ('",
             sourcePathString_regex, "').");
      }
      return(invisible(rxsTree));
    }
  }
    
  ###---------------------------------------------------------------------------
  ### Start copying over values
  ###---------------------------------------------------------------------------
  
  sourceNodeValue <-
    sourceNode$Get("value", simplify=FALSE)[[sourceEntityNodeId]];

  targetNodeValue <-
    targetNode$Get("value", simplify=FALSE)[[targetEntityNodeId]];
  
  if (!is.list(targetNodeValue)) {
    targetNodeValue <- list(targetNodeValue);
    names(targetNodeValue) <-
      paste0(targetNodeListCreation_prefix,
             targetEntityNodeId,
             targetNodeListCreation_suffix);
  }
    
  if (!is.list(sourceNodeValue)) {
    sourceNodeValue <- list(sourceNodeValue);
    names(sourceNodeValue) <- sourceEntityNodeId
  }

  ### Rename values with the prefix and suffix
  names(sourceNodeValue) <-
    paste0(prefix, names(sourceNodeValue), suffix);
  
  if (!forceCopyingOfExistingValues) {
    if (any(names(sourceNodeValue) %in% names(targetNodeValue))) {
      overlappingNames <-
        intersect(names(sourceNodeValue), names(targetNodeValue));
      if (!silent) {
        cat0("In the rxsTree you passed, I found both the target entity node ",
             "you specified and the source entity node referred to in the target ",
             "entity node. However, one or more names occur in both lists (",
             vecTxtQ(overlappingNames),
             "). This should not be possible normally, as entity identifiers ",
             "should be unique. Maybe you already ran this command? ",
             "I am aborting. If you want to override this error, use ",
             "argument 'forceCopyingOfExistingValues=TRUE'.");
      }
      return(invisible(rxsTree));
    }
  };
  
  targetNode$value <-
    c(targetNodeValue,
      sourceNodeValue);
  
  if (!silent) {
    cat0("Succesfully copied over ", length(sourceNodeValue),
         " fields (", vecTxtQ(names(sourceNodeValue)), ") from the source ",
         "entity node with identifier '", sourceEntityNodeId, "' to the ",
         "target entity node with identifier '", targetEntityNodeId, "'.\n");
  }
  
  return(invisible(rxsTree));
  
}
                                      
