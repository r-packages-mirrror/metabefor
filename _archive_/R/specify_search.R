#' Specify a search
#'
#' @param date The date that the search is conducted.
#' @param query The query used in the search (as specified using the
#' functions for queries, e.g. [query_full()]).
#' @param databases The databases that were searched, as a named list of
#' vectors, where each vector's name is the name of the interface used to
#' access the databases in the vector.
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
         databases = databases,
         fields = fields);
  
  class(res) <- "mbf_search";
  
  return(res);
  
}
