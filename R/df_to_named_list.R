#' Convert a data frame into a named list
#'
#' @param df The data frame
#' @param nameCol The column name or index that holds the names
#' @param otherCols The other columns to include (by name or index)
#'
#' @return A named list
#' @export
#'
#' @examples metabefor::df_to_named_list(
#'   data.frame(
#'     nameCol = c('name1', 'name2', 'name3'),
#'     contentCol = c('contents1', 'contents2', 'contents3')
#'   )
#' );
df_to_named_list <- function(df,
                             nameCol = 1,
                             otherCols = 2:ncol(df)) {

  if(!is.data.frame(df)) {
    stop("As `df`, you have to pass a data frame!");
  }
  
  if (!(all(c(nameCol, otherCols) %in% names(df)))) {
    stop("Not all names specified in `nameCol` and `otherCols` exist in `df`!");
  }

  if (length(otherCols) == 1) {
    res <- apply(df[, otherCols, drop=FALSE], 1, c, simplify = FALSE);
  } else {
    res <- apply(df[, otherCols, drop=FALSE], 1, list, simplify = FALSE);
  }

  names(res) <- df[, nameCol];
  
  return(res);
  
}