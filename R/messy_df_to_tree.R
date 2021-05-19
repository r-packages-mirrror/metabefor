#' Convert a potentially messy 'network data frame' to a data.tree
#'
#' @param x The data frame
#' @param rootName The name of the root element 
#' @param childCol The index (as number or name) of the child column
#' @param parentCol The index (as number or name) of the parent column
#'
#' @return The data.tree object
#' @export
#'
#' @examples messy_df <-
#'   data.frame(
#'     childCol = c("A", "B", "C", "D", "E"),
#'     parentCol = c("B", "B", "A", "", "B")
#'   );
#' 
#' messy_df_to_tree(
#'   messy_df
#' );
#'
#' ### Note that by default, data.tree will deform
#' ### or not accept messy data frames
#' # data.tree::FromDataFrameNetwork(messy_df);
messy_df_to_tree <- function(x,
                             rootName = "base",
                             childCol = 1,
                             parentCol = 2) {
  
  x <- as.data.frame(x);
  
  ### Move parents without specified child to children column
  x[
    is.na(x[, childCol]) | (nchar(trimws(x[, childCol])) == 0),
    childCol
  ] <-
    x[
      is.na(x[, childCol]) | (nchar(trimws(x[, childCol])) == 0),
      parentCol
    ];

  ### For nodes without a specified parent, designate the root
  x[
    is.na(x[, parentCol]) | (nchar(trimws(x[, parentCol])) == 0),
    parentCol
  ] <- rootName;
  
  ### Give the orphans a parent (an artificial root);

  ### First get nodes without parent
  orphans <-
    x[, parentCol][!(x[, parentCol] %in% x[, childCol])];

  orphanDf <-
    as.data.frame(t(rep("", ncol(x))));
  names(orphanDf) <- names(x);
  
  orphanDf[, childCol] <- orphans;
  orphanDf[, parentCol] <- rootName;

  resDf <-
    rbind(
      as.data.frame(x),
      orphanDf
    );

  ### For nodes that are their own parent,
  ### replace that parent with an articial root
  resDf[
    resDf[, childCol] == resDf[, parentCol],
    parentCol
  ] <- rootName;
  
  ### Delete nodes where the child is the root
  resDf <-
    resDf[
      resDf[, childCol] != rootName,
    ];

  ### First get nodes without parent
  orphans <-
    resDf[, parentCol][!(resDf[, parentCol] %in% resDf[, childCol])];
  
  res <-
    data.tree::FromDataFrameNetwork(resDf);
  
  return(res);
  
}