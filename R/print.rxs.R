#' Printing a tree of R extraction scripts
#'
#' @param studyTree The study tree
#' @param rxsStructure Optionally, the rxs structure
#' @param headerPrefix 
#' @param ... 
#'
#' @return invisibly, the dataframe with paths and values.
#' 
#' @rdname printrxs
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
  
  rmdpartials::partial(rmdPartialFilename);
  
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
  
  rxs_partialfunction(x = x,
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
      studyTree_to_dataframe(studyTree);

    print(paste0(headerPrefix, " Tree of extracted entities\n\n"));
    
    if (getOption('metabefor.debug', FALSE)) {
      cat("\n\n\n\n");
    }
    
    printableStudyTree <- data.tree::Clone(studyTree);
    class(printableStudyTree) <- setdiff(class(studyTree), "rxs");
    
    if (knit) cat("\n\n<pre>");
    ### Suppress warnings until bug in data.tree is fixed, see:
    ### https://github.com/gluc/data.tree/issues/106
    suppressWarnings(print(printableStudyTree));
    if (knit) cat("</pre>\n\n");
    
    print(paste0(headerPrefix, " Table with extracted entities and extracted values\n\n"));
    
    if (knit) {
      # cat(knitr::knit(text = "\n\n```{r, echo=FALSE, cache=FALSE, message=FALSE, results='asis' }\n  knitr::kable(res, row.names=FALSE);\n```\n\n",
      #                 quiet = TRUE));
      print(knitr::kable(res, row.names=FALSE));
      return(invisible(res));
    } else {
      return(res);
    }
    
    
  }
  
}
