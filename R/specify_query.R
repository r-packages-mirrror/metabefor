specify_search <- function(date,
                           query,
                           databases,
                           fields) {
  
  res <-
    list(data = date,
         query = query,
         databases=databases,
         fields=fields);
  
  class(res) <- "mbf_specify_query";
  
  return(res);
  
}
