rxs_fragmentPrep_strwrap_with_prefix <- function(x,
                                                 width,
                                                 prefix) {
  
  x <- gsub("\r\n", "\n", x);
  x <- unlist(strsplit(x, "\n"));
  x <-
    strwrap(x, width=width, prefix=prefix);
  x[nchar(x) == 0] <- prefix;
  
  return(x);

}