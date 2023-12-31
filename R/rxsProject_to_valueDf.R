#' Create a long ('tidy') data frame with the full Rxs tree from each source
#'
#' The data frame has columns "`sourceId`", "`path`", "`entity`", and "`nodeValue`".
#'
#' @param x The Rxs object
#' @param zapNewlines Whether to call [zapNewlines()] on the values
#'
#' @return A data frame
#' @export
rxsProject_to_valueDf <- function(x,
                                  zapNewlines = TRUE) {
  
  if (!inherits(x, "rxs_parsedExtractionScripts")) {
    stop(wrap_error(
      "As `x`, you have to pass an object of parsed Rxs scripts ",
      "(with class 'rxs_parsedExtractionScript'), but instead, ",
      "you passed an object with class(es) ", vecTxtQ(class(x)), "."
    ));
  }
  
  sourceIds <- names(x$rxsTrees);
  
  res <-
    rbind_df_list(
      lapply(
        sourceIds,
        function(currentSourceId) {
          
          df <- rxsTree_to_valueDf(
            x$rxsTrees[[currentSourceId]],
            zapNewlines = zapNewlines
          );
          
          df <-
            cbind(
              data.frame(sourceId = rep(currentSourceId, nrow(df))),
              df
            );
          
          return(df);
          
        }
      )
    );

  return(res);
  
}
