#' Write a 'screener package' to disk
#' 
#' This function prepares a 'screener package': a set of files that one or
#' more screeners can use to independently screen a set of bibliographic
#' records.
#'
#' @param bibliographyDf A dataframe with bibliographic entries
#' @param outputPath The path where to write the screener package(s)
#' @param screeners The unique identifiers to use for each screener; a character
#' vector where each value represented one screener. This will be used for the
#' directory and filenames as well as fields in the bibliographic database.
#' @param screenerFieldsPrefix,screenerFieldsSuffix The pre- and suffixes to
#' pre- and append to the screener identifier in the field names in the
#' bibliographic database
#' @param prevRoundField A field containing information from previous screening
#' rounds
#' @param duplicateField The field containing information about duplicates;
#' if specified, information from this field is appended to whatever is already
#' stored in the 
#' @param basename The base name for the directories and files (appended
#' with the screener identifiers)
# #' @param suffixedConfigFiles 
#' @param ... Any additional arguments are passed to [write_JabRef_Config()].
#'
#' @return Invisibly, a results object with intermediate results and the
#' written configuration file
#' @export
#'
#' @examples
write_screenerPackage <- function(bibliographyDf,
                                  outputPath,
                                  screeners = c("a", "b"),
                                  screenerFieldsPrefix = "screener",
                                  screenerFieldsSuffix = "status",
                                  prevRoundField = NULL,
                                  duplicateField = NULL,
                                  basename = "screening_",
                                  #suffixedConfigFiles = FALSE,
                                  ...) {
  
  if (!requireNamespace("synthesisr", quietly = TRUE)) {
    stop("To write a screener package, the `synthesisr` package is required!",
         " You can install it with:\n\n  install.packages('synthesisr');\n");
  }
  
  res<- list(input = c(list(call = sys.call()),
                       as.list(environment()),
                       list(sysCall = as.list(sys.call()))),
             intermediate = list(),
             output = list(bibliographyDfs = list(),
                           configFiles = list()));
  
  res$intermediate$screenerFields <-
    lapply(
      screeners,
      function(currentScreener) {
        return(
          paste0(
            screenerFieldsPrefix,
            screeners[currentScreener],
            screenerFieldsSuffix)
        );
      }
    );
  names(res$intermediate$screenerFields) <-
    screeners;
  
  ### Create initialization field for this round
  
  initializationFieldName <-
    paste0(basename, "_init");

  if (!(initializationFieldName %in% names(bibliographyDf))) {
    bibliographyDf[, initializationFieldName] <- "";
  }

  ### Check whether we have information from a previous round
  if (!is.null(prevRoundField) && (prevRoundField %in% names(bibliographyDf))) {
    
    bibliographyDf[, initializationFieldName] <-
      ifelse(
        nchar(bibliographyDf[, initializationFieldName]) > 0 &
          nchar(bibliographyDf[, prevRoundField]) > 0,
        paste0(
          bibliographyDf[, initializationFieldName],
          ">",
          bibliographyDf[, prevRoundField]
        ),
        bibliographyDf[, initializationFieldName]
      );
        
  }
  
  ### Check whether we have information about duplicates
  if (!is.null(duplicateField) && (duplicateField %in% names(bibliographyDf))) {
    
    bibliographyDf[, initializationFieldName] <-
      ifelse(
        nchar(bibliographyDf[, initializationFieldName]) > 0 &
          nchar(bibliographyDf[, duplicateField]) > 0,
        paste0(
          bibliographyDf[, initializationFieldName],
          ">",
          bibliographyDf[, duplicateField]
        ),
        bibliographyDf[, initializationFieldName]
      );

  }
  
  res$intermediate$screenerDirs <- list();
  res$intermediate$screenerLibraryNames <- list();
  
  for (currentScreener in 1:length(screeners)) {
    
    currentScreenerField <-
      res$intermediate$screenerFields[[currentScreener]];
    
    if (!(currentScreenerField %in% names(bibliographyDf))) {
      bibliographyDf[, currentScreenerField] <-
        "";
    }
    
    bibliographyDf[, currentScreenerField] <-
      ifelse(
        nchar(bibliographyDf[, currentScreenerField]) > 0 &
          nchar(bibliographyDf[, initializationFieldName]) > 0,
        paste0(
          bibliographyDf[, currentScreenerField],
          ">",
          bibliographyDf[, initializationFieldName]
        ),
        bibliographyDf[, currentScreenerField]
      );

    res$intermediate$screenerDirs[[currentScreener]] <-
      file.path(
        outputPath,
        paste0(basename, screeners[currentScreener])
      );
    
    res$intermediate$screenerLibraryNames[[currentScreener]] <-
      paste0(basename, screeners[currentScreener], ".bib");
    
    ### Create directory if it doesn't exist yet
    if (!file.exists(res$intermediate$screenerDirs[[currentScreener]])) {
      dir.create(res$intermediate$screenerDirs[[currentScreener]],
                 recursive = TRUE);
    }
    
    res$output$bibliographyDf[[currentScreener]] <-
      bibliographyDf;
    
    ### Store bibliography
    synthesisr::write_refs(
      bibliographyDf,
      file =
        file.path(
          res$intermediate$screenerDirs[[currentScreener]],
          res$intermediate$screenerLibraryNames[[currentScreener]]
        ),
      format = "bib"
    );

    ### Generate Jabref configuration files
    res$output$configFiles[[currentScreener]] <-
      write_JabRef_Config(
        screeners = screeners[currentScreener],
        screenerFieldsPrefix = screenerFieldsPrefix,
        screenerFieldsSuffix = screenerFieldsSuffix,
        outputPath = res$intermediate$screenerDirs[[currentScreener]],
        ...
      );
    
  }
  
  return(invisible(res));

}