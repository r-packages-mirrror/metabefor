#' Read an aggregation tree from a Google Sheets URL
#'
#' @param x The URL (make sure it's publicly viewable)
#'
#' @return The data.tree object
#' @export
read_aggregationTree_from_gs <- function(x) {
  
  ### Indicate we want to use the Google Sheets API without authenticating
  googlesheets4::gs4_deauth();
  
  aggregationTree_asDf <-
    googlesheets4::read_sheet(
      x
    );
  
  res <-
    messy_df_to_tree(
      aggregationTree_asDf
    );
  
  return(res);
  
}

