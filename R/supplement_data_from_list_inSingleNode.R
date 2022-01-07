#' Supplement a target entitity node with clustered entity values from another entity node
#' 
#' These function supplement a entity nodes with clustered entity values
#' from other entities specified in the target entity nodes. Both the
#' target entity and the source entity have to be clustering entities.
#' Use `supplement_data_from_list_inSingleNode()` for one entity node, and
#' `supplement_data_from_list_inRxsTrees()` for all nodes containing the
#' field that identifies the node to copy the values from (i.e. as specified
#' in `sourceEntityNodeIdField_in_targetEntity`).
#'
#' @param rxsTree The Rxs tree
#' @param x Either a single Rxs tree or an object with parsed Rxs
#' files.
#' @param targetEntityNodeId The identifier of the target entity node (the node to
#' supplement)
#' @param sourceEntityNodeIdField_in_targetEntity Inside the target entity node,
#' the name holding the identifier of the source entity node (the node supplying
#' the data). Note that that field is itself an entity as specified in the entity
#' specification spreadsheet.
#' @param idField_in_targetEntityNode `NULL` if the target node's name is also its
#' identifier; otherwise, the name of the field containing the identifier in
#' the target node.
#' @param idField_in_sourceEntityNode `NULL` if the source node's name is also its
#' identifier; otherwise, the name of the field containing the identifier in
#' the source node.
#' @param forceCopyingOfExistingValues Whether to force copying (and so
#' overwriting) existing values if encountered in the target entity. If
#' `FALSE`, existing values will not be overwritten.
#' @param prefix,suffix A text string to prepend and append to the names of the
#' values that are copied.
#' @param fieldsToCopy_regex A regular expression that can optionally be
#' used to select fields to copy over from the source node to the target node.
#' @param sourcePathString_regex,targetPathString_regex Regular expressions
#' that must match the path string of the source of target node.
#' @param silent Whether to be quiet or chatty.
#'
#' @return Invisibly, the (altered) input - but note that since the `data.tree`
#' package uses `R6`'s pass-by-reference semantics, that object is altered in
#' place, and sothe returned object can be discarded.
#' 
#' @rdname supplement_data_from_list
#' @export
#'
supplement_data_from_list_inSingleNode <- function(rxsTree,
                                                   targetEntityNodeId,
                                                   sourceEntityNodeIdField_in_targetEntity,
                                                   idField_in_targetEntityNode = NULL,
                                                   idField_in_sourceEntityNode = NULL,
                                                   fieldsToCopy_regex = NULL,
                                                   forceCopyingOfExistingValues = FALSE,
                                                   sourcePathString_regex = NULL,
                                                   targetPathString_regex = NULL,
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
  
  if (is.null(idField_in_targetEntityNode)) {
    
    ### The node identifier is its name
    targetNode <- data.tree::FindNode(
      rxsTree,
      targetEntityNodeId
    );
    
  } else {
    
    ### The node identifier is stored as a value in a value list inside the
    ### target node
    targetNode <- data.tree::Traverse(
      rxsTree,
      filterFun = function(node) {
        
        if ( is.null(node$value)) return(FALSE);
        if (!is.list(node$value)) return(FALSE);
        if (!(idField_in_targetEntityNode %in% names(node$value))) {
          return(FALSE);
        }
        if (node$value[[idField_in_targetEntityNode]] == targetEntityNodeId) {
          return(TRUE);
        } else {
          return(FALSE);
        }
      }
    );
  }
  
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
  ### Start looking for the field specifying the source node
  ###---------------------------------------------------------------------------
  
  targetNodeValue <-
    targetNode$Get("value", simplify=FALSE)[[targetEntityNodeId]];

  if (is.null(targetNodeValue)) {
    if (!silent) {
      cat0("In the rxsTree you passed, I found the target entity node you ",
           "specified. However, within that node, there is no value list ",
           "specified (i.e. the target node does not seem to be a clustering ",
           "node), so I can't look for the field specifying the ",
           "source entity node (",
           sourceEntityNodeIdField_in_targetEntity, ").");
    }
    return(invisible(rxsTree));
  }
  
  if (!is.list(targetNodeValue)) {
    if (!silent) {
      cat0("In the rxsTree you passed, I found the target entity node you ",
           "specified. However, the value stored within that node, is not a ",
           "value list (i.e. the target node does not seem to be a clustering ",
           "node), so I can't look for the field specifying the ",
           "source entity node (",
           sourceEntityNodeIdField_in_targetEntity, ").");
    }
    return(invisible(rxsTree));
  }

  if (!(sourceEntityNodeIdField_in_targetEntity %in% names(targetNodeValue))) {
    if (!silent) {
      cat0("In the rxsTree you passed, I found the target entity node you ",
           "specified, but in the value list it stored, I cannot find the ",
           "field field specifying the source entity node ('",
           sourceEntityNodeIdField_in_targetEntity, "').\n\nThe names that ",
           "did occur were: ", vecTxtQ(names(targetNodeValue)), ".");
    }
    return(invisible(rxsTree));
  }
  
  sourceEntityNodeId <-
    targetNodeValue[[sourceEntityNodeIdField_in_targetEntity]];

  if (is.null(sourceEntityNodeId) ||
      is.na(sourceEntityNodeId) ||
      (nchar(sourceEntityNodeId) == 0)) {
    if (!silent) {
      cat0("In the rxsTree you passed, I found the target entity node you ",
           "specified, and within it, the entity node that you said would ",
           "contain the reference to the source entity node ",
           sourceEntityNodeIdField_in_targetEntity,
           ". However, its value is NULL, NA, or empty ('').");
    }
    return(invisible(rxsTree));
  }
  
  ###---------------------------------------------------------------------------
  ### Start looking for the source node
  ###---------------------------------------------------------------------------
  
  if (is.null(idField_in_sourceEntityNode)) {
    
    ### The node identifier is its name
    sourceNode <- data.tree::FindNode(
      rxsTree,
      sourceEntityNodeId
    );
    
  } else {
    
    ### The node identifier is stored as a value in a value list inside the
    ### target node
    sourceNode <- data.tree::Traverse(
      rxsTree,
      filterFun = function(node) {
        
        if ( is.null(node$value)) return(FALSE);
        if (!is.list(node$value)) return(FALSE);
        if (!(idField_in_sourceEntityNode %in% names(node$value))) {
          return(FALSE);
        }
        if (node$value[[idField_in_sourceEntityNode]] == sourceEntityNodeId) {
          return(TRUE);
        } else {
          return(FALSE);
        }
      }
    );
  }
  
  ###---------------------------------------------------------------------------
  ### Check whether a source node was found
  ###---------------------------------------------------------------------------
  
  if (is.null(sourceNode)) {
    if (!silent) {
      cat("In the rxsTree you passed, I found the target entity node you ",
          "specified, and within it, I found the reference ",
          "to the source entity node in field ",
          sourceEntityNodeIdField_in_targetEntity,
          ". However, the source entity node specified there (",
          sourceEntityNodeId,
          ") does not exist in the rxsTree you passed.");
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
             "you specified and the source entity node referred to in the target ",
             "entity node. However, the source entity path string does not ",
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
  
  if (!is.list(sourceNodeValue)) {
    if (!silent) {
      cat0("In the rxsTree you passed, I found both the target entity node ",
           "you specified and the source entity node referred to in the target ",
           "entity node. However, the value stored in the source node is not ",
           "a value list (i.e. the source node does not seem to be a clustering ",
           "node), so I can't copy any values over to the target node.");
    }
    return(invisible(rxsTree));
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
                                      
