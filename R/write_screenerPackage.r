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
#' @param initializeScreenerFields Whether to copy the contents of the
#' initialization field to the screener fields.
#' @param basename The base name for the directories and files (appended
#' with the screener identifiers)
# #' @param suffixedConfigFiles 
#' @param ... Any additional arguments are passed to [write_JabRef_Config()].
#' @param silent Whether to be silent or chatty.
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
                                  initializeScreenerFields = TRUE,
                                  basename = "screening_",
                                  #suffixedConfigFiles = FALSE,
                                  silent = metabefor::opts$get("silent"),
                                  ...) {
  
  if (!requireNamespace("synthesisr", quietly = TRUE)) {
    stop("To write a screener package, the `synthesisr` package is required!",
         " You can install it with:\n\n  install.packages('synthesisr');\n");
  }
  
  if (!silent) {
    cat0("Starting to write screener packages for ", length(screeners),
         " screeners with identifiers ", vecTxtQ(screeners), ".");
  }
  
  if (inherits(bibliographyDf, "mbfSearch")) {
    bibliographyDf <- bibliographyDf$bibHitDf;
  }

  if (!inherits(bibliographyDf, "mbfBibHitDf")) {
    warning("The `bibliographyDf` you passed does not have class ",
            "`mbfBibHitDf` or `mbfSearch`. Proceeding, but note that ",
            "this function was made for processing a bibliography data frame ",
            "as produced by a call to metabefor::import_search_results().");
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
            currentScreener,
            screenerFieldsSuffix)
        );
      }
    );
  names(res$intermediate$screenerFields) <-
    screeners;
  
  ### For convenience
  screenerFields <- res$intermediate$screenerFields;
  
  ### Create initialization field for this round
  
  initializationFieldName <-
    paste0(basename, "_init");

  if (!(initializationFieldName %in% names(bibliographyDf))) {
    bibliographyDf[, initializationFieldName] <- "";
  }

  ### Check whether we have information from a previous round
  if (!is.null(prevRoundField) && (prevRoundField %in% names(bibliographyDf))) {
    
    if (!silent) {
      cat0("\nCopying info in previous round field `", prevRoundField,
           "` (", length(unique(bibliographyDf[, prevRoundField])),
           " distinct values (for ",
           sum((!is.na(bibliographyDf[, prevRoundField])) &
                 nchar(bibliographyDf[, prevRoundField]) > 0),
           " records) to initialization field `",
           initializationFieldName, "`.");
    }
    
    bibliographyDf[, initializationFieldName] <-
      ifelse(
        is.na(bibliographyDf[, initializationFieldName]) |
          nchar(bibliographyDf[, initializationFieldName]) == 0,
        bibliographyDf[, prevRoundField],
        ifelse(
          (!is.na(bibliographyDf[, prevRoundField])) &
            nchar(bibliographyDf[, prevRoundField]) > 0,
          paste0(
            bibliographyDf[, initializationFieldName],
            ">",
            bibliographyDf[, prevRoundField]
          ),
          bibliographyDf[, initializationFieldName]
        )
      );

  } else {
    
    if (!silent) {
      cat0("\nNo previous round field specified or it was not present in the ",
           "data frame, so not copying its content to `",
           initializationFieldName, "`.");
    }
    
  }
  
  ### Check whether we have information about duplicates
  if (!is.null(duplicateField) && (duplicateField %in% names(bibliographyDf))) {
    
    if (!silent) {
      cat0("\nCopying info in duplicate record field `", duplicateField,
           "` (", length(unique(bibliographyDf[, duplicateField])),
           " distinct values (for ",
           sum((!is.na(bibliographyDf[, duplicateField])) &
                 nchar(bibliographyDf[, duplicateField]) > 0),
           " records) to initialization field `",
           initializationFieldName, "`.");
    }
    
    bibliographyDf[, initializationFieldName] <-
      ifelse(
        is.na(bibliographyDf[, initializationFieldName]) |
          nchar(bibliographyDf[, initializationFieldName]) == 0,
        bibliographyDf[, duplicateField],
        ifelse(
          (!is.na(bibliographyDf[, duplicateField])) &
            nchar(bibliographyDf[, duplicateField]) > 0,
          paste0(
            bibliographyDf[, initializationFieldName],
            ">",
            bibliographyDf[, duplicateField]
          ),
          bibliographyDf[, initializationFieldName]
        )
      );
    
  } else {
    if (!silent) {
      cat0("\nNo duplicate record field specified or it was not present in the ",
           "data frame, so not copying its contents to `",
           initializationFieldName, "`.");
    }
  }
  
  res$intermediate$screenerDirs <- list();
  res$intermediate$screenerLibraryNames <- list();

  if (!silent) {
    cat0("\nThe initialization field `", initializationFieldName, "` now has ",
         length(unique(bibliographyDf[, initializationFieldName])),
         " distinct values (for ",
         sum((!is.na(bibliographyDf[, initializationFieldName])) &
               nchar(bibliographyDf[, initializationFieldName]) > 0),
         " records) which will ",
         ifelse(initializeScreenerFields, "also", "not"),
         " be copied to the screener fields (", vecTxtQ(screenerFields),
         ").");
  }
  
  if (!silent) {
    cat("\nStarting to process screener identifiers.\n");
  }
  
  for (currentScreener in screeners) {
    
    if (!silent) {
      cat0("\nStarting to process screener identifier '",
           currentScreener, "'.");
    }
    
    currentScreenerField <-
      res$intermediate$screenerFields[[currentScreener]];
    
    if (!(currentScreenerField %in% names(bibliographyDf))) {
      bibliographyDf[, currentScreenerField] <-
        "";
      if (!silent) {
        cat0("\nScreener field '", currentScreenerField,
             "' did not yet exist; created it.");
      }
    } else {
      if (!silent) {
        cat0("\nScreener field '", currentScreenerField,
             "' already existed.");
      }
    }
    
    if (initializeScreenerFields) {
      
      bibliographyDf[, currentScreenerField] <-
        ifelse(
          is.na(bibliographyDf[, currentScreenerField]) |
            nchar(bibliographyDf[, currentScreenerField]) == 0,
          bibliographyDf[, initializationFieldName],
          ifelse(
            (!is.na(bibliographyDf[, initializationFieldName])) &
              nchar(bibliographyDf[, initializationFieldName]) > 0,
            paste0(
              bibliographyDf[, currentScreenerField],
              ">",
              bibliographyDf[, initializationFieldName]
            ),
            bibliographyDf[, currentScreenerField]
          )
        );

      if (!silent) {
        cat0("\nCopied contents of the initialization field to the contents ",
             "of the current screeners field where that was still empty - ",
             "it is now empty for ",
             sum(is.na(bibliographyDf[, currentScreenerField]) |
                   (nchar(bibliographyDf[, currentScreenerField]) == 0)),
             " records.");
      }
      
    }

    res$intermediate$screenerDirs[[currentScreener]] <-
      file.path(
        outputPath,
        paste0(basename, currentScreener)
      );
    
    res$intermediate$screenerLibraryNames[[currentScreener]] <-
      paste0(basename, currentScreener, ".bib");
    
    ### Create directory if it doesn't exist yet
    if (!file.exists(res$intermediate$screenerDirs[[currentScreener]])) {
      dir.create(res$intermediate$screenerDirs[[currentScreener]],
                 recursive = TRUE);
    }
    
    res$output$bibliographyDf[[currentScreener]] <-
      bibliographyDf;

    ### Store bibliography
    synthesisr::write_refs(
      structure(bibliographyDf, class="data.frame"),
      file =
        file.path(
          res$intermediate$screenerDirs[[currentScreener]],
          res$intermediate$screenerLibraryNames[[currentScreener]]
        ),
      format = "bib"
    );

    if (!silent) {
      cat0("\nStored bibliography to '",
           file.path(
             res$intermediate$screenerDirs[[currentScreener]],
             res$intermediate$screenerLibraryNames[[currentScreener]]
           ),
           "'.");
    }
    
    ### Generate Jabref configuration files
    res$output$configFiles[[currentScreener]] <-
      write_JabRef_Config(
        screeners = currentScreener,
        screenerFieldsPrefix = screenerFieldsPrefix,
        screenerFieldsSuffix = screenerFieldsSuffix,
        outputPath = res$intermediate$screenerDirs[[currentScreener]],
        ...
      );
    
    if (!silent) {
      cat0("\nStored JabRef configuration file to that same path.\n");
    }
    
  }
  
  return(invisible(res));

}