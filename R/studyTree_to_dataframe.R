studyTree_to_valueDf <- function(studyTree) {
  
  res <- studyTree$Get(function(node) {
    nodeName <- node$name;
    nodeValue <- node$value;
    if (is.null(nodeValue)) {
      nodeValue <- "NULL (element absent from imported extraction script)";
    }
    if (is.list(nodeValue)) {
      pathString <- node$pathString;
      
      ### Note - this will become problematic if the list contains
      ### more complicated values such as vectors or tables!!!
      
      return(data.frame(path = rep(pathString, length(nodeValue)),
                        entity = names(nodeValue),
                        nodeValue = flattenNodeValues(nodeValue),
                        stringsAsFactors = FALSE));
    } else {
      pathString <- node$parent$pathString;
      return(data.frame(path = pathString,
                        entity = nodeName,
                        nodeValue = flattenNodeValues(nodeValue),
                        stringsAsFactors = FALSE));
    }
  }, filterFun = data.tree::isLeaf,
  simplify=FALSE);
  
  res <- do.call("rbind", res);

  return(res);
  
}