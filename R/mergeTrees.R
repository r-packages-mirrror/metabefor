#' Merge two `data.tree` trees
#' 
#' Note this this function is used internally and was written to merge Rxs
#' trees, so it may not fit other use cases.
#'
#' @param tree1,tree2 The trees to merge.
#'
#' @return The merged tree, a (cloned) `data.tree` `Node` object.
#' @export
#'
#' @examples
mergeTrees <- function(tree1,
                       tree2) {
  
  mergedTree <- data.tree::Clone(tree1);
  
  tree1_node <- mergedTree;
  tree2_node <- tree2;
  
  tree1_childNames <- names(tree1_node$children);
  tree2_childNames <- names(tree2_node$children);
  
  for (currentName in tree2_childNames) {
    if (!(currentName %in% tree1_childNames)) {
      
      ### We can add this node without having to check child entities
      tree1_node$AddChildNode(
        data.tree::Clone(tree2_node[[currentName]])
      );
      
    } else {
      
      ### We have to add the children of this node one by one.
      
    }
    
  }

  return(mergedTree);
  
}