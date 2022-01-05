#' Printing a tree of R extraction scripts
#'
#' @param x The study tree
#' @param rxsStructure Optionally, the rxs structure
#' @param headingLevel The level of the top-most R Markdown heading
#' @param printPlots Whether to print plots
#' @param echoPartial Whether to echo (show) the R code chunks in the partial
#' @param partialFile Optionally (and advanced) the path to a file with an
#' R Markdown partial.
#' @param forceKnitrOutput Force output as if knitr would be knitting (to
#' test stuff in the console).
#' @param ... Passed on to [rmdpartials::partial()].
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

###-----------------------------------------------------------------------------

#' @rdname printrxs
#' @method knit_print rxs
#' @importFrom knitr knit_print
#' @export
knit_print.rxs <- function(x,
                           rxsStructure=NULL,
                           headingLevel = 3,
                           echoPartial = FALSE,
                           partialFile = NULL,
                           ...) {
  
  rxs_partial(x = x,
              rxsStructure=rxsStructure,
              headingLevel=headingLevel,
              echoPartial=echoPartial,
              partialFile=partialFile,
              ...);
}

###-----------------------------------------------------------------------------

#' @export
#' @rdname printrxs
print.rxs <- function(x,
                      rxsStructure=NULL,
                      headingLevel = 3,
                      forceKnitrOutput = FALSE,
                      echoPartial = FALSE,
                      partialFile = NULL,
                      ...) {
  
  if (isTRUE(getOption('knitr.in.progress')) || forceKnitrOutput) {
    
    rxs_partial(x = x,
                rxsStructure = rxsStructure,
                headingLevel = headingLevel,
                echoPartial = echoPartial,
                partialFile = partialFile,
                ...);
    
  } else {
    
    studyTree <- x;
    
    headerPrefix <-
      paste0(paste(rep("#", headingLevel), collapse=""), " " );
    
    res <-
      studyTree_to_valueDf(studyTree);

    cat(paste0(headerPrefix, " Tree of extracted entities\n\n"));

    printableStudyTree <- data.tree::Clone(studyTree);
    class(printableStudyTree) <- setdiff(class(studyTree), c("rxs","rxsObject"));
    
    ### Suppress warnings until bug in data.tree is fixed, see:
    ### https://github.com/gluc/data.tree/issues/106
    suppressWarnings(print(printableStudyTree));

    cat(paste0(headerPrefix, " Table with extracted entities and extracted values\n\n"));
    
    print(res);
    
    return(invisible(res));

  }
  
}
