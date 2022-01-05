htmlComment <- function(x,
                        padding = "~",
                        length = 78,
                        pre = "<!-- ",
                        after = " ",
                        post = "-->") {
  
  pad_to <-
    length - nchar(pre) - nchar(after) - nchar(post);
  
  x <- unlist(strsplit(x, "\n", fixed=TRUE));
  
  x <- unlist(trimws(x, which="right"));
  
  x <- unlist(strwrap(x, width = pad_to));
  
  x <-
    paste0(
      pre,
      x,
      rep(after, length(x)),
      unlist(lapply(nchar(x), function(i) {
        if (i < pad_to) {
          return(paste(rep(padding, pad_to - i), collapse=""));
        } else {
          return(" ");
        }
      })),
      post
    );
  
  return(x);
  
}
