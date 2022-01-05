#' Create a long ('tidy') data frame with the full study tree
#'
#' The data frame has columns "`path`", "`entity`", and "`nodeValue`".
#'
#' @param studyTree The study tree
#'
#' @return A data frame
#' @export
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
      
      res <- data.frame(rep(pathString, length(nodeValue)),
                        names(nodeValue),
                        flattenNodeValues(nodeValue),
                        stringsAsFactors = FALSE);
      names(res) <- c("path", "entity", "nodeValue");
      
      if (ncol(res) > 3) {
        warning("When converting a study tree to a data frame, in ",
                "'", nodeName,
                "', a data frame with too many columns was produced.");
      }
      
      return(res);
      
    } else {
      pathString <- node$parent$pathString;
      res <- data.frame(pathString,
                        nodeName,
                        flattenNodeValue(nodeValue),
                        stringsAsFactors = FALSE);
      names(res) <- c("path", "entity", "nodeValue");
      
      if (ncol(res) > 3) {
        warning("When converting a study tree to a data frame, in ",
                "'", nodeName,
                "', a data frame with too many columns was produced.");
      }
      
      return(res);
      
    }
  }, filterFun = data.tree::isLeaf, simplify=FALSE);

  res <- metabefor::rbind_df_list(res);
  
  row.names(res) <- NULL;

  return(res);
  
}