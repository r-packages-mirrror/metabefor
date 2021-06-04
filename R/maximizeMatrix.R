#' Adding columns or rows to a matrix
#' 
#' Created to add missing rows or columns to a matrix
#'
#' @param x Either a single data frame or matrix (for `maximizeMatrix`) or,
#' more commonly, a list of data frames or matrces (for `maximizeMatrices`).
#' @param rowNames,colNames The names or the rows and columns as they
#' should be in the final result
#' @param fillValue The value to fill the new rows and columns with
#'
#' @return A matrix with the specified rows and columns
#' @rdname maximizeMatrices
#' @export
#'
#' @examples metabefor::maximizeMatrix(
#'   mtcars[1:3, 1:4],
#'   rowNames = c('Mazda RX4', letters[1:3]),
#'   colNames = c('mpg', 'cyl', LETTERS[1:4])
#' );
maximizeMatrix <- function(x,
                           rowNames,
                           colNames,
                           fillValue = 0) {
  
  rowsToAdd <- rowNames[!(rowNames %in% rownames(x))];
  colsToAdd <- colNames[!(colNames %in% colnames(x))];
  
  if (length(rowsToAdd) > 0) {
    
    newRows <-
      t(
        sapply(
          rep(fillValue, length(rowsToAdd)),
          rep,
          times=ncol(x)
        )
      );
    
    colnames(newRows) <- colnames(x);
    rownames(newRows) <- rowsToAdd;
    
    x <- rbind(
      x,
      newRows
    );
    
  }
  
  if (length(colsToAdd) > 0) {
    
    newCols <-
      sapply(
        rep(fillValue, length(colsToAdd)),
        rep,
        times=nrow(x)
      );
    
    colnames(newCols) <- colsToAdd;
    
    x <- cbind(
      x,
      newCols
    );
  }

  res <-
    x[rowNames,
      colNames];
  
  return(res);
  
}
