#' Create a long ('tidy') data frame with the full Rxs tree from each source
#'
#' The data frame has columns "`sourceId`", "`path`", "`entity`", and "`nodeValue`".
#'
#' @param rxsTree The Rxs tree
#' @param zapNewlines Whether to call [zapNewlines()] on the values
#'
#' @return A data frame
#' @export
rxsProject_to_valueDf <- function(rxsObject,
                                  zapNewlines = TRUE) {
  
  if ((!inherits(rxsTree, "rxsObject")) &&
      (!(inherits(rxsTree, "rxs") && inherits(rxsTree, "Node")))) {
    stop(wrap_error(
      "As `x`, you have to pass an Rxs tree, but instead, ",
      "you passed an object with class(es) ", vecTxtQ(class(rxsTree)), "."
    ));
  }
  
  sourceIds <- names(rxsObject$rxsTrees);
  
  res <-
    rbind_df_list(
      lapply(
        sourceIds,
        function(currentSourceId) {
          
          df <- rxsTree_to_valueDf(
            rxsObject$rxsTrees[[currentSourceId]],
            zapNewlines = zapNewlines
          );
          
          df <-
            cbind(
              data.frame(sourceId = rep(currentSourceId, nrow(df)))
            );
          
          return(df);
          
        }
      )
    );

  return(res);
  
}
