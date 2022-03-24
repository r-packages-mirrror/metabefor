#' Check for duplicate sources
#' 
#' @param primarySources The primary dataframe with sources
#' @param secondarySources The secondary dataframe with sources
#' @param useStringDistances Whether to use string distances - note that
#' that can be very slow and take along time if you have thousands of sources.
#' @param stringDistance The string distance for titles
#' @param stringDistanceMethod Method to use for string distance computation
#' @param charsToZap The characters to delete from fields before looking for duplicates
#' @param doiCol The name of the column with the DOIs
#' @param forDeduplicationSuffix Suffix to add to optional deduplication columns
#' @param returnRawStringDistances Whether to return the raw string distances
#' or not (this can be _very_ large).
#' @param silent Whether to be silent or chatty.
#'
#' @return A vector indicating for each record whether it's a duplicate, with
#' an attribute called `duplicateInfo` that holds more detailed information
#' and that can be accessed using the [attributes()] function.
#' @export
#'
#' @examples
check_duplicate_sources <- function(primarySources,
                                    secondarySources = NULL,
                                    useStringDistances = FALSE,
                                    stringDistance = 5,
                                    stringDistanceMethod = "osa",
                                    charsToZap = "[^A-Za-z0-9]",
                                    doiCol = "doi",
                                    matchFully = c("year", "title", "author"),
                                    matchStart = c("title" = 40,
                                                   "author" = 30),
                                    matchEnd = c("title" = 40,
                                                 "author" = 30),
                                    forDeduplicationSuffix = "_forDeduplication",
                                    returnRawStringDistances = FALSE,
                                    silent = metabefor::opts$get("silent")) {
  
  if (useStringDistances) {
    if (!requireNamespace("stringdist", quietly = TRUE)) {
      stop("To deduplicate using string distances, ",
           "you need the `stringdist` package! You can ",
           "install it with:\n\n  install.packages('stringdist');\n");
    }
  }
  
  if (!(doiCol %in% names(primarySources))) {
    stop("The column specified for the DOIs with argument `doiCol`, '",
         doiCol, "', does not occur in the `primarySources` dataframe.");
  }
  
  ### For convenience
  doi_forDeduplicationCol <- paste0(doiCol, forDeduplicationSuffix);
  
  ### Columns to look at
  
  colsToPreprocess <- c();

  if (hasValidValue(matchFully)) {
    colsToPreprocess <- c(colsToPreprocess, matchFully);
  }
  if (hasValidValue(matchStart)) {
    colsToPreprocess <- c(colsToPreprocess, names(matchStart));
  }
  if (hasValidValue(matchEnd)) {
    colsToPreprocess <- c(colsToPreprocess, names(matchEnd));
  }
  
  colsToPreprocess <- unique(colsToPreprocess);
  
  ###-------------------------------------------------------------------------
  ### Preprocess primary (and potentially secondary) sources
  ###-------------------------------------------------------------------------
  
  ### Preprocess DOIs
  primarySources[, doi_forDeduplicationCol] <-
    trimws(tolower(primarySources[, doiCol]));
  if (!is.null(secondarySources)) {
    secondarySources[, doi_forDeduplicationCol] <-
      trimws(tolower(secondarySources[, doiCol]));
  }

  ### Preprocess other fields

  if (hasValidValue(colsToPreprocess)) {
    
    if (!all(colsToPreprocess %in% names(primarySources))) {
      stop("One or more of the columns specified to look at ",
           "does not occur in the `primarySources` dataframe.");
    }
    
    msg("Starting to preprocess columns by 'zapping' characters. Processing ",
        "columns ",
        vecTxtQ(colsToPreprocess),
        ".\n",
        silent = silent);
    
    colsToPreprocess_forDeduplicationColnames <-
      paste0(colsToPreprocess, forDeduplicationSuffix);
    names(colsToPreprocess_forDeduplicationColnames) <- colsToPreprocess;
    
    for (currentCol in colsToPreprocess) {
      
      current_forDeduplicationCol <-
        paste0(currentCol, forDeduplicationSuffix);
      
      primarySources[, current_forDeduplicationCol] <-
        dedup_zap(charsToZap, primarySources[, currentCol]);
      
      ### Preprocess secondary sources
      if (!is.null(secondarySources)) {
        secondarySources[, current_forDeduplicationCol] <-
          dedup_zap(charsToZap, secondarySources[, currentCol]);
      }
      
    }
    
  }

  if (is.null(secondarySources)) {
    
    ###-------------------------------------------------------------------------
    ###-------------------------------------------------------------------------
    ### Primary sources only
    ###-------------------------------------------------------------------------
    ###-------------------------------------------------------------------------
    
    if (!silent) {
      cat("Starting to look for internally duplicate DOIs within ",
          nrow(primarySources),
          " primary sources.\n",
          sep="");
    }
    
    res <-
      primarySources[
        ,
        c(
          colsToPreprocess,
          doiCol,
          colsToPreprocess_forDeduplicationColnames,
          doi_forDeduplicationCol
        )
      ];
    
    res$doiMatch <-
      dedup_findDuplicatesInVector_trimmed(
        primarySources[, doi_forDeduplicationCol]
      );

    msg("Found ",
        sum(res$doiMatch),
        " duplicates based on DOI.\n",
        silent = silent);

    ###-------------------------------------------------------------------------
    ### Start looking for full matches
    ###-------------------------------------------------------------------------
    
    if (hasValidValue(matchFully)) {
      for (currentCol in matchFully) {
        
        msg("Looking for full matches, processing column '",
            currentCol,
            "'.\n",
            silent = silent);

        res[, paste0("fullMatch_", currentCol)] <- 
          dedup_findDuplicatesInVector_trimmed(
            primarySources[, currentCol]
          );

        msg("Found ",
            sum(res[, paste0("fullMatch_", currentCol)]),
            " duplicates based on full string matching.\n",
            silent = silent);
        
      }
    }
    
    ###-------------------------------------------------------------------------
    ### Start looking for matches of first characters
    ###-------------------------------------------------------------------------
    
    if (hasValidValue(matchStart)) {
      for (currentCol in names(matchStart)) {
        
        msg("Looking for matches , processing column '",
            currentCol,
            "'.\n",
            silent = silent);
        
        res[, paste0("startMatch_", currentCol)] <- 
          dedup_findDuplicatesInVector_trimmed(
            primarySources[, currentCol],
            start = matchStart[currentCol]
          );
        
        msg("Found ",
            sum(res[, paste0("startMatch_", currentCol)]),
            " duplicates based on matching the first ",
            matchStart[currentCol], " characters.\n",
            silent = silent);
        
      }
    }
    
    ###-------------------------------------------------------------------------
    ### Start looking for matches of last characters
    ###-------------------------------------------------------------------------
    
    if (hasValidValue(matchEnd)) {
      for (currentCol in names(matchEnd)) {
        
        msg("Looking for matches , processing column '",
            currentCol,
            "'.\n",
            silent = silent);
        
        res[, paste0("endMatch_", currentCol)] <- 
          dedup_findDuplicatesInVector_trimmed(
            primarySources[, currentCol],
            end = matchEnd[currentCol]
          );
        
        msg("Found ",
            sum(res[, paste0("endMatch_", currentCol)]),
            " duplicates based on matching the last ",
            matchEnd[currentCol], " characters.\n",
            silent = silent);
        
      }
    }
    
    ###-------------------------------------------------------------------------
    ### String distances
    ###-------------------------------------------------------------------------
    
    if (useStringDistances) {
      
      if (!silent) {
        cat("Starting to look for internal duplicates based on string distance.");
      }
      
      ### Get the string distances (takes a few seconds)
      stringDistances.raw <-
        stringdist::stringdistmatrix(
          primarySources[, title_forDeduplicationCol],
          primarySources[, title_forDeduplicationCol],
          method = stringDistanceMethod
        );
      
      ### Get lower diagonal
      stringDistances <- stringDistances.raw;
      stringDistances[
        upper.tri(
          stringDistances,
          diag = TRUE
        )
      ] <- NA;
      
    }
    
  } else {
    
    ###-------------------------------------------------------------------------
    ###-------------------------------------------------------------------------
    ### Primary *and* secondary sources
    ###-------------------------------------------------------------------------
    ###-------------------------------------------------------------------------
    
    res <-
      primarySources[
        ,
        c(
          colsToPreprocess,
          doiCol,
          colsToPreprocess_forDeduplicationColnames,
          doi_forDeduplicationCol
        )
      ];
    
    msg("Looking for duplicate occurrences of ",
        nrow(primarySources), " primary sources in ",
        nrow(secondarySources), " secondary sources.\n",
        "Starting to look for duplicate DOIs.\n",
        silent=silent);

    res$doiMatch <-
      dedup_matches_trimmed(
        secondarySources[, doi_forDeduplicationCol],
        primarySources[, doi_forDeduplicationCol],
      );
    
    msg("Found ",
        sum(res$doiMatch),
        " duplicates based on DOI.\n",
        silent = silent);
    
    ###-------------------------------------------------------------------------
    ### Start looking for full matches
    ###-------------------------------------------------------------------------
    
    if (hasValidValue(matchFully)) {
      for (currentCol in matchFully) {
        
        msg("Looking for full matches, processing column '",
            currentCol,
            "'.\n",
            silent = silent);
        
        res[, paste0("fullMatch_", currentCol)] <- 
          dedup_matches_trimmed(
            secondarySources[, currentCol],
            primarySources[, currentCol]
          );
        
        msg("Found ",
            sum(res[, paste0("fullMatch_", currentCol)]),
            " duplicates based on full string matching.\n",
            silent = silent);
        
      }
    }
    
    ###-------------------------------------------------------------------------
    ### Start looking for matches of first characters
    ###-------------------------------------------------------------------------
    
    if (hasValidValue(matchStart)) {
      for (currentCol in names(matchStart)) {
        
        msg("Looking for matches , processing column '",
            currentCol,
            "'.\n",
            silent = silent);
        
        res[, paste0("startMatch_", currentCol)] <- 
          dedup_findDuplicatesInVector_trimmed(
            secondarySources[, currentCol],
            primarySources[, currentCol],
            start = matchStart[currentCol]
          );
        
        msg("Found ",
            sum(res[, paste0("startMatch_", currentCol)]),
            " duplicates based on matching the first ",
            matchStart[currentCol], " characters.\n",
            silent = silent);
        
      }
    }
    
    ###-------------------------------------------------------------------------
    ### Start looking for matches of last characters
    ###-------------------------------------------------------------------------
    
    if (hasValidValue(matchEnd)) {
      for (currentCol in names(matchEnd)) {
        
        msg("Looking for matches , processing column '",
            currentCol,
            "'.\n",
            silent = silent);
        
        res[, paste0("endMatch_", currentCol)] <- 
          dedup_findDuplicatesInVector_trimmed(
            secondarySources[, currentCol],
            primarySources[, currentCol],
            end = matchEnd[currentCol]
          );
        
        msg("Found ",
            sum(res[, paste0("endMatch_", currentCol)]),
            " duplicates based on matching the last ",
            matchEnd[currentCol], " characters.\n",
            silent = silent);
        
      }
    }
    
    ###-------------------------------------------------------------------------
    ### String distances
    ###-------------------------------------------------------------------------
    
    if (useStringDistances) {
      
      if (!silent) {
        cat("Starting to look for duplicates based on string distance.");
      }
      
      ### Get the string distances (takes a few seconds)
      stringDistances <-
        stringdist::stringdistmatrix(
          secondarySources[, title_forDeduplicationCol],
          primarySources[, title_forDeduplicationCol],
          method = stringDistanceMethod
        );
      
      ### Flag duplicates
      stringDistancesFlagged <-
        stringDistances < stringDistance;
      
      ### Get indices of duplicates for each entry
      stringDistanceDuplicates <-
        apply(
          stringDistancesFlagged,
          1,
          which
        );
      
      stringDistances_forFlagged <-
        lapply(
          seq_along(stringDistanceDuplicates),
          function(rowIndex) {
            return(stringDistances[rowIndex, stringDistanceDuplicates[[rowIndex]]]);
          }
        );
      
      stringDistanceDuplicates_asString <-
        unlist(lapply(stringDistanceDuplicates, vecTxtQ));
      stringDistance_nrOfDuplicates <-
        unlist(lapply(stringDistanceDuplicates, length));
      stringDistance_hasDuplicates <-
        stringDistance_nrOfDuplicates > 0;
      stringDistance_duplicateTitles <-
        lapply(
          stringDistanceDuplicates,
          function(i) {
            return(primarySources[i, titleCol]);
          }
        );
      
      if (is.null(secondarySources)) {
        stringDistance_originalTitles <-
          lapply(
            which(stringDistance_hasDuplicates),
            function(i) {
              return(primarySources[i, titleCol]);
            }
          );
      } else {
        stringDistance_originalTitles <-
          lapply(
            which(stringDistance_hasDuplicates),
            function(i) {
              return(secondarySources[i, titleCol]);
            }
          );
      }
      
      res$stringDistance <-
        stringDistance_hasDuplicates;

      if (!silent) {
        cat("Found ",
            sum(grepl("strdist", res)),
            " duplicates based on string distance.\n",
            sep="");
      }
      
      res <- cbind(
        res,
        data.frame(
          stringDistance_secondaryRowsWithDuplicates = which(stringDistance_hasDuplicates),
          stringDistance_primaryDuplicateRows = sort(unique(unlist(stringDistanceDuplicates))),
          stringDistance_duplicates = stringDistanceDuplicates[which(stringDistance_hasDuplicates)],
          stringDistance_duplicateTitles = stringDistance_duplicateTitles[which(stringDistance_hasDuplicates)],
          stringDistance_originalTitles = stringDistance_originalTitles,
          stringDistances_actualStringDistances = stringDistances_forFlagged[which(stringDistance_hasDuplicates)]
        )
      );
      
      if (returnRawStringDistances) {
        attr(res, "rawStringDistances") <- stringDistances;
      }
      
    }

  }

  return(res);
  
}
