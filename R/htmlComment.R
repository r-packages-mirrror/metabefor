htmlComment <- function(x = NULL,
                        padding = "~",
                        length = 78,
                        pre = "<!--",
                        before = " ",
                        after = " ",
                        post = "-->") {
  
  
  if (is.null(x)) {
    pad_to <-
      length - nchar(pre) - nchar(post);
    x <- "";
    beforeX <- NULL;
    afterX <- NULL;
  } else {
    pad_to <-
      length - nchar(pre) - nchar(before) - nchar(after) - nchar(post);
    
    x <- unlist(strsplit(x, "\n", fixed=TRUE));
    
    x <- unlist(trimws(x, which="right"));
    
    x <- unlist(strwrap(x, width = pad_to));
    beforeX <- rep(before, length(x));
    afterX <- rep(after, length(x));
  }
  
  x <-
    paste0(
      pre,
      beforeX,
      x,
      afterX,
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
