purgeListsFromDf <- function(x) {

  if (any(unlist(lapply(x, is.list)))) {
    x <-
      as.data.frame(
        lapply(
          x,
          function(column) {
            if (is.list(column)) {
              return(
                vecTxtQ(
                  as.character(
                    unlist(
                      column
                    )
                  )
                )
              );
            } else {
              return(column);
            }
          }
        )
      );
  }
  
  return(x);
  
}