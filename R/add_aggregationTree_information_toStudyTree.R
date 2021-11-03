#' Add information from an aggregation tree to a study tree
#'
#' @param studyTree,studies The study tree (as `studyTree`) or the object with
#' multiple study trees (as `studies`)
#' @param aggregationTree The aggregation tree
#' @param fieldName The field storing the values along which to aggregate
#' @param prefixes,suffixes Vector of pre- and suffixes to add to the
#' `fieldname` when storing the values for progressive aggregation
#'
#' @return Invisibly (and, irrelevantly), the study tree
#' @rdname add_aggregationTree_information
#' @export
#'
add_aggregationTree_information_toStudyTree <- function(studyTree,
                                                        aggregationTree,
                                                        fieldName,
                                                        prefixes = NULL,
                                                        suffixes = NULL) {
  
  maxDepth <- max(aggregationTree$Get("level")) - 1;
  
  if (is.null(prefixes)) {
    prefixes <-
      rep("", maxDepth);
  }
  
  if (is.null(suffixes)) {
    suffixes <-
      paste0("_aggr", 1:maxDepth);
  }
  
  studyTree$Do(
    function(node) {
      
      if (is.null(node$value)) {
        return(invisible(node));
      }
      
      if (is.list(node$value)) {
        
        if (fieldName %in% names(node$value)) {
          
          aggTreeNode <- data.tree::FindNode(
            aggregationTree,
            name = node$value[[fieldName]]
          );
          
          if (is.null(aggTreeNode)) {
            return(invisible(studyTree));
          }
          
          reqAggregationValueLength <- max(length(prefixes),
                                           length(suffixes));
          
          aggregationValues <- rev(utils::tail(aggTreeNode$path, -1));
          
          aggregationValueVector <-
            rep(utils::tail(aggregationValues, 1), reqAggregationValueLength);
          
          aggregationValueVector[seq_along(aggregationValues)] <-
            aggregationValues;
          
          aggregationFieldNames <-
            paste0(prefixes,
                   fieldName,
                   suffixes);
          
          aggregationFieldNames[seq_along(aggregationValues)] <-
            paste0(prefixes[seq_along(aggregationValues)],
                   fieldName,
                   suffixes[seq_along(aggregationValues)]);
          
          names(aggregationValueVector) <-
            aggregationFieldNames;
          
          for (valueName in names(aggregationValueVector)) {
            node$value[[valueName]] <-
              aggregationValueVector[[valueName]];
          }
          
          return(invisible(node));
          
        }
        
      } else {
        
        ### If no list value is available
        
      }
      
    }
  );
  
  return(invisible(studyTree));
  
}
