#' Detect duplicate sources
#'
#' @param primarySources The primary dataframe with sources
#' @param secondarySources The secondary dataframe with sources
#' @param stringDistance The string distance for titles
#' @param stringDistanceMethod Method to use for string distance computation
#' @param titleCharsToZap The title characters to delete from titles
#' @param titleCharacterTrimming The number of characters to trim titles to
#' @param doiCol The name of the column with the DOIs
#' @param titleCol The name of the column with the titles
#' @param forDeduplicationSuffix Suffix to add to optional deduplication columns
#' @param silent 
#'
#' @return
#' @export
#'
#' @examples
duplicate_sources <- function(primarySources,
                              secondarySources = NULL,
                              stringDistance = 5,
                              stringDistanceMethod = "osa",
                              titleCharsToZap = "[^A-Za-z0-9]",
                              titleCharacterTrimming = 1000,
                              doiCol = "doi",
                              titleCol = "title",
                              forDeduplicationSuffix = "_forDeduplication",
                              returnRawStringDistances = FALSE,
                              silent = metabefor::opts$get("silent")) {
  
  if (!(doiCol %in% names(primarySources))) {
    stop("The column specified for the DOIs with argument `doiCol`, '",
         doiCol, "', does not occur in the `primarySources` dataframe.");
  }
  if (!(titleCol %in% names(primarySources))) {
    stop("The column specified for the source titles with argument ",
         "`titleCol`, '", titleCol,
         "', does not occur in the `primarySources` dataframe.");
  }
  
  ### For convenience
  doi_forDeduplicationCol <- paste0(doiCol, forDeduplicationSuffix);
  title_forDeduplicationCol <- paste0(titleCol, forDeduplicationSuffix);

  ### Clean up DOIs
  primarySources[, doi_forDeduplicationCol] <-
    trimws(tolower(primarySources[, doiCol]));
  if (!is.null(secondarySources)) {
    secondarySources[, doi_forDeduplicationCol] <-
      trimws(tolower(secondarySources[, doiCol]));
  }
  
  ### Prepare titles by removing the specified characters
  primarySources[, title_forDeduplicationCol] <-
    gsub(titleCharsToZap, "", trimws(tolower(primarySources[, titleCol])));
  if (!is.null(secondarySources)) {
    secondarySources[, title_forDeduplicationCol] <-
      gsub(titleCharsToZap, "", trimws(tolower(secondarySources[, titleCol])));
  }
  
  ### Trim characters from end of title
  primarySources[, title_forDeduplicationCol] <-
    substr(primarySources[, title_forDeduplicationCol], 1, titleCharacterTrimming);
  if (!is.null(secondarySources)) {
    secondarySources[, title_forDeduplicationCol] <-
      substr(secondarySources[, title_forDeduplicationCol], 1, titleCharacterTrimming);
  }
  
  if (is.null(secondarySources)) {
    
    if (!silent) {
      cat("Starting to look for internally duplicate DOIs within ",
          nrow(primarySources),
          " primary sources.\n",
          sep="");
    }
    
    ### Prepare vector with results
    res <- rep("", nrow(primarySources));
    
    ### Find duplicates based on DOIs
    res <-
      ifelse(
        (!is.na(primarySources[, doi_forDeduplicationCol])) &
          (duplicated(primarySources[, doi_forDeduplicationCol])),
        paste0(res, ">doi"),
        res
      );
    
    if (!silent) {
      cat("Found ",
          sum(grepl("doi", res)),
          " duplicates based on DOI.\n",
          sep="");
    }
    
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
    
    
  } else {
    
    if (!silent) {
      cat("Looking for duplicate occurrences of ",
          nrow(primarySources), " primary sources in ",
          nrow(secondarySources), " secondary sources.\n",
          sep="");
    }

    if (!silent) {
      cat("Starting to look for duplicate DOIs.\n");
    }

    ### Prepare vector with results
    res <- rep("", nrow(secondarySources));

    ### Find duplicates based on DOIs
    res <-
      ifelse(
        (!is.na(secondarySources[, doi_forDeduplicationCol])) &
          (isTRUE(secondarySources[, doi_forDeduplicationCol] %in%
                 primarySources[, doi_forDeduplicationCol])),
        paste0(res, ">doi"),
        res
      );
    
    if (!silent) {
      cat("Found ",
          sum(grepl("doi", res)),
          " duplicates based on DOI.\n",
          sep="");
    }

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

  }

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
    unlist(lapply(stringDistanceDuplicates, ufs::vecTxtQ));
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

  ### Set duplicate, using the deduplication_identifier column
  ### earlier because tmpBibHitDf has less rows than bibHitDf
  res <-
    paste0(
      res,
      ifelse(
        stringDistance_hasDuplicates,
        ">strdist",
        ""
      )
    );
  
  if (!silent) {
    cat("Found ",
        sum(grepl("strdist", res)),
        " duplicates based on string distance.\n",
        sep="");
  }

  attr(res, "duplicateInfo") <-
    list(
      secondaryRowsWithDuplicates = which(stringDistance_hasDuplicates),
      primaryDuplicateRows = sort(unique(unlist(stringDistanceDuplicates))),
      stringDistanceDuplicates = stringDistanceDuplicates[which(stringDistance_hasDuplicates)],
      stringDistance_duplicateTitles = stringDistance_duplicateTitles[which(stringDistance_hasDuplicates)],
      stringDistance_originalTitles = stringDistance_originalTitles,
      actualStringDistances = stringDistances_forFlagged[which(stringDistance_hasDuplicates)]
    );
  
  if (returnRawStringDistances) {
    attr(res, "duplicateInfo") <-
      c(attr(res, "duplicateInfo"),
        list(rawStringDistances = stringDistances));
  }
  
  return(res);
  
}
