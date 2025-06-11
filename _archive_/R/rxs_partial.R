#' Printing a tree of R extraction scripts
#'
#' @param printPlots Whether to print plots
#'
#' @return The partial, or for the print function, the printed
#' information (invisibly).
#' 
#' @rdname printrxs
#' 
#' @export
rxs_partial <- function(x,
                        rxsStructure=NULL,
                        headingLevel = x$headingLevel,
                        printPlots = TRUE,
                        echoPartial = FALSE,
                        partialFile = NULL,
                        ...) {
  
  ### Get filename
  if (!is.null(partialFile) && file.exists(partialFile)) {
    rmdPartialFilename <-
      partialFile;
  } else {
    rmdPartialFilename <-
      system.file("partials", "_rxs_partial.Rmd", package="metabefor");
  }
  
  rmdpartials::partial(rmdPartialFilename,
                       ...);
  
}
