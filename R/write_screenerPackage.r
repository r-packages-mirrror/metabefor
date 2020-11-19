#' @export
write_screenerPackage <- function(bibliographyDf,
                                  outputPath,
                                  screeners = c("a", "b"),
                                  screenerFieldsPrefix = "screener",
                                  screenerFieldsSuffix = "status",
                                  duplicateField = NULL,
                                  basename = "screening_",
                                  suffixedConfigFiles = FALSE,
                                  ...) {
  
  res<- list(input = c(list(call = sys.call()),
                       as.list(environment()),
                       list(sysCall = as.list(sys.call()))),
             intermediate = list(),
             output = list());
  
  ### Check whether we have information about duplicates
  if (!is.null(duplicateField) && (duplicateField %in% names(bibliographyDf))) {
    ### Store whether a record is a duplicate in the screener fields
    for (currentScreener in 1:length(screeners)) {
      bibliographyDf[, paste0(screenerFieldsPrefix, screeners[currentScreener], screenerFieldsSuffix)] <-
        bibliographyDf[, duplicateField];
    }
  }
  
  res$intermediate$screenerDirs <- list();
  res$intermediate$screenerLibraryNames <- list();
  for (currentScreener in 1:length(screeners)) {
    res$intermediate$screenerDirs[[currentScreener]] <-
      file.path(
        outputPath,
        paste0(basename, screeners[currentScreener])
      );
    res$intermediate$screenerLibraryNames[[currentScreener]] <-
      paste0(basename, screeners[currentScreener], ".bib");
    
    ### Create directory if it doesn't exist yet
    if (!file.exists(res$intermediate$screenerDirs[[currentScreener]])) {
      dir.create(res$intermediate$screenerDirs[[currentScreener]]);
    }
    
    ### Store bibliography
    revtools::write_bibliography(
      bibliographyDf,
      filename =
        file.path(
          res$intermediate$screenerDirs[[currentScreener]],
          res$intermediate$screenerLibraryNames[[currentScreener]]
        ),
      format = "bib"
    );

    ### Generate Jabref configuration files
    configFiles <- write_JabRef_Config(screeners = screeners[currentScreener],
                                       screenerFieldsPrefix = screenerFieldsPrefix,
                                       screenerFieldsSuffix = screenerFieldsSuffix,
                                       outputPath = res$intermediate$screenerDirs[[currentScreener]],
                                       ...);
  }
  
  return(invisible(res));

}