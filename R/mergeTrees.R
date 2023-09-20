#' Merge two Rxs trees (`data.tree` nodes)
#' 
#' Note this this function is used internally and was written to merge Rxs
#' trees (hence the name), so it may not fit other use cases: for example,
#' when merging two leaf nodes with the same name and path, if the `value`
#' attribute is a list, those lists are combined using [base::c()].
#'
#' @param tree1,tree2 The trees to merge.
#' @param sourceId,filename1,filename2 The identifier of the source and the
#' original file names of the extraction scripts.
#' @param spaces The number of spaces to use in messages (if `silent = metabefor::opts$get("silent")`)
#' @param silent Whether to be silent or chatty.
#'
#' @return The merged tree, a (cloned) `data.tree` `Node` object.
#' @export
#'
#' @examples
mergeTrees <- function(tree1, 
                       tree2,
                       sourceId,
                       filename1,
                       filename2,
                       spaces = 2,
                       silent = metabefor::opts$get("silent")) {

  mergedTree <- data.tree::Clone(tree1);
  
  start <- paste0("\n", repStr(" ", spaces), "- ");
  nestedStart <- paste0("\n", repStr(" ", spaces+2), "- ");
  
  if (tree1$isLeaf && (!tree2$isLeaf)) {
    stop(wrap_error("When merging extraction scripts for a source with identifier (sourceId) '",
                    sourceId,
                    "', the first rxs tree (or 'node') I have to merge ('",
                    tree1$name, "') consists of a single entity; but the ",
                    "second rxs tree ('",
                    tree2$name, "') has children (i.e. is a container ",
                    "entity). That suggest an error in the specification of ",
                    "your modules, or an error made during extraction. I ",
                    "cannot perform this merge.\n\nYou may want to check the ",
                    "extraction scripts: '",
                    filename1, "' and '", filename2, "'."));
  } else if (!(tree1$isLeaf) && (tree2$isLeaf)) {
    stop(wrap_error("When merging extraction scripts for a source with identifier (sourceId) '",
                    sourceId, 
                    "', the first rxs tree (or 'node') I have to merge, ('",
                    tree1$name, "'), has children (i.e. is a container ",
                    "entity); but the second rxs tree ('",
                    tree2$name, "') consists of a ",
                    "single entity. That suggest an error in the",
                    "specification of your modules, or an error made during",
                    "extraction. I cannot perform this merge.\n\nYou may want to check the ",
                    "extraction scripts: '",
                    filename1, "' and '", filename2, "'."));
  } else if (tree1$isLeaf && tree2$isLeaf) {
    
    ### Neither has children, i.e. is a container entity
    
    if (is.list(tree1$value) && (!is.list(tree2$value))) {
      stop(wrap_error("When merging extraction scripts for a source with identifier (sourceId) '",
                      sourceId,
                      "', the first rxs tree (or 'node') I have to merge ('",
                      tree1$name, "') is a clustering entity ; but the ",
                      "second rxs tree ('",
                      tree2$name, "') is not (i.e. is a single extracted ",
                      "entity). That suggest an error in the specification of ",
                      "your modules, or an error made during extraction. I ",
                      "cannot perform this merge.\n\nYou may want to check the ",
                      "extraction scripts: '",
                      filename1, "' and '", filename2, "'."));
    } else if (!(is.list(tree1$value)) && is.list(tree2$value)) {
      stop(wrap_error("When merging extraction scripts for a source with identifier (sourceId) '",
                      sourceId,
                      "', the first rxs tree (or 'node') I have to merge ('",
                      tree1$name, "') is a single extracted ",
                      "entity; but the second rxs tree ('",
                      tree2$name, "') is not (i.e. is a clustering entity). ",
                      "That suggest an error in the specification of ",
                      "your modules, or an error made during extraction. I ",
                      "cannot perform this merge.\n\nYou may want to check the ",
                      "extraction scripts: '",
                      filename1, "' and '", filename2, "'."));
    } else if (is.list(tree1$value) && is.list(tree2$value)) {
      
      ### Both are clustering entities: merge the value lists
      
      msg(start,
          "Both the first rxs tree (or 'node') I have to merge ('",
          tree1$name, "') and the second second rxs tree ('",
          tree2$name, "') are clustering entites (i.e. entities ",
          "with a `value` consisting of a list where multiple ",
          "single extracted entities are efficiently specified). ",
          "Merging them and returning the result.",
          silent = silent);
      
      mergedTree$value <- c(tree1$value, tree2$value);
      
    } else {
      stop(wrap_error("When merging extraction scripts for a source with identifier (sourceId) '",
                      sourceId,
                      "', both rxs trees (or 'nodes') I have to merge ('",
                      tree1$name, "' and '", tree2$name, "') are single ",
                      "extracted entities. ",
                      "That suggest an error in the specification of ",
                      "your modules, or an error made during extraction. I ",
                      "cannot perform this merge.\n\nYou may want to check the ",
                      "extraction scripts: '",
                      filename1, "' and '", filename2, "'."));
    }
    
  } else {
    ### Both have children, i.e. are container entities

    msg(start,
        "Both the first rxs tree (or 'node') I have to merge ('",
        tree1$name, "') and the second second rxs tree ('",
        tree2$name, "') are container entities (i.e. entities ",
        "that themselves do not contain any extracted data, but ",
        "that function to organize other extracted entities). ",
        "Processing those other contained entities now.",
        silent = silent);
    
    tree1_node <- mergedTree; ### Convenience; note that data.tree uses R6
    tree2_node <- tree2;
    
    tree1_childNames <- names(tree1_node$children);
    tree2_childNames <- names(tree2_node$children);
    
    for (currentName in tree2_childNames) {
      if (!(currentName %in% tree1_childNames)) {

        msg(nestedStart,
            "The second container entity ('",
            tree2$name, "') contains an entity that does not exist in the ",
            "first container entity ('", tree1$name,"'): '",
            currentName, "'. Adding it to the first container entity).",
            silent = silent);
        
        ### We can add this node without having to check child entities
        tree1_node$AddChildNode(
          data.tree::Clone(tree2_node[[currentName]])
        );
        
      } else {
        
        ### A child node with the name of the current
        ### node also exists as a child node of tree1.
        ### We therefore have to merge those two nodes
        ### by calling ourselves on them and storing
        ### the result in the merged tree.
        
        msg("\n    - The second container entity ('",
            tree2$name, "') contains an entity that already exists in the ",
            "first container entity ('", tree1$name,"'): '",
            currentName, "'. Calling myself to merge those two entities.",
            silent = silent);
        
        tree1_node$AddChildNode(
          metabefor::mergeTrees(
            tree1 = tree1_node[[currentName]],
            tree2 = tree2_node[[currentName]],
            sourceId = sourceId,
            filename1 = filename1,
            filename2 = filename2,
            spaces = spaces + 2,
            silent = silent
          )
        );
        
      }
      
    }

  }

  return(mergedTree); ### (Which is tree1_node because R6 passes by reference)
  
}
