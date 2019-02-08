#' Specify a search
#'
#' @param date The date that the search is conducted.
#' @param query The query used in the search (as specified using the
#' functions for [queries]).
#' @param databases The databases that were searched.
#' @param fields The fields in which the query terms were searched.
#'
#' @return An `mbf_search` object with the search specifications.
#' @export
specify_search <- function(date,
                           query,
                           databases,
                           fields) {
  
  res <-
    list(date = date,
         query = query,
         databases=databases,
         fields=fields);
  
  class(res) <- "mbf_search";
  
  return(res);
  
}
