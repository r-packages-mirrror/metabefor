checkForColName <- function(x, colname) {
  if (!(colname %in% names(x))) {
    stop("The txs specification spreadsheet you passed does not contain ",
         "a column with the entity identifier column name ('",
         colname, "'). It ",
         "has columns ", vecTxtQ(names(x)), ".");
  }
  return(invisible(TRUE));
}