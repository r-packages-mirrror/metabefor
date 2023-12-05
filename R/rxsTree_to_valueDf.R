#' Create a long ('tidy') data frame with the full Rxs tree
#'
#' The data frame has columns "`path`", "`entity`", and "`nodeValue`".
#'
#' @param rxsTree The Rxs tree
#' @param zapNewlines Whether to call [zapNewlines()] on the values
#'
#' @return A data frame
#' @export
rxsTree_to_valueDf <- function(rxsTree,
                               zapNewlines = TRUE) {
  
  if ((!inherits(rxsTree, "rxsObject")) &&
      (!(inherits(rxsTree, "rxs") && inherits(rxsTree, "Node")))) {
    stop(wrap_error(
      "As `x`, you have to pass an Rxs tree, but instead, ",
      "you passed an object with class(es) ", vecTxtQ(class(rxsTree)), "."
    ));
  }
  
  res <- rxsTree$Get(function(node) {

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
        warning("When converting a Rxs tree to a data frame, in ",
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
      
      if (zapNewlines) {
        res$nodeValue <-
          unlist(
            lapply(
              res$nodeValue,
              zapNewlines
            )
          );
      }
      
      if (ncol(res) > 3) {
        warning("When converting a Rxs tree to a data frame, in ",
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
