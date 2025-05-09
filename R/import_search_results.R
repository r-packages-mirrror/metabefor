#' Import search results
#' 
#' This is a wrapper around the `synthesisr` function `read_refs` that
#' processes the hits in a set of subdirectories in a directory, specifying
#' the original file, directory, and, if the files were names using a
#' convention, date, database, and interface (see the `search_metadataRegex`
#' argument).
#'
#' @param path The path to the files with the search results.
#' @param dirRegex The regular expression to match subdirectories against.
#' @param fileRegex The regular expression to match filenames against.
#' @param dirsToIgnoreRegex,filesToIgnoreRegex A regular expression to specify
#' which directories and files should be ignored.
#' @param recursive Whether to recursively read subdirectories.
#' @param perl Whether to use Perl regular expressions
#' @param fieldsToCopy Fields to copy over to other fields (e.g. `L3` sometimes
#' contains DOIs, instead of `DO`). This takes the form of a list of character
#' vectors, with each vector's name being the new field name (to copy to), and
#' each character vector listing the fields to copy (to that new field name).
#' Set to `NULL` or pass an empty list to not copy anything over.
#' @param preparatoryReplacements Specify any replacements to be made in the
#' file(s) to import, as a named character vector, where each element's *name*
#' is the replacement, and the corresponding value is a Perl regular expression
#' which will (case insensitively) be searched in the file (e.g. to replace
#' all RIS tags `T1` with `TI`, pass `c("TI" = "^T1")` as value
#' of `preparatoryReplacements`).
#' @param synthesisr_tag_naming The value to pass to
#' the `synthesisr::read_refs()` function as argument `tag_naming`.
#' @param copySep When copying fields over (see `fieldsToCopy`), the separator
#' to use when the new field is not empty (in which case the contents to copy
#' over will be appended).
#' @param idFieldName New name to use for the `id` field (column), a reserved
#' name in JabRef (if `NULL`, the field is not renamed).
#' @param parallel Whether to use multiple cores for parallel processing.
#' @param search_metadataRegex A regular expression to match against the
#' filenames. If it matches, metadata will be extracted in three capturing
#' groups, in the order date (using ISO standard 8601 format, i.e. 2022-03-05),
#' interface, and database, separated by underscores (`_`), with an optional
#' fourth element, again separated with an underscore, that can be used to
#' specify which query was run (in case multiple queries are used in the same
#' database / interface combination and on the same date).
#' @param silent Whether to be silent or chatty.
#'
#' @return An object with all the imported information, including, most
#' importantly, the data frame `bibHitDf` with all results.
#' @export
#'
#' @examples ### Path to extra files in {metabefor} package
#' metabefor_files_path <-
#'   system.file(
#'     "extdata",
#'     package = "metabefor"
#'   ); 
#' 
#' ### Path with OpenAlex exports
#' OpenAlexExport_path <-
#'   file.path(
#'     metabefor_files_path,
#'     "openalex-exports"
#'   ); 
#' 
#' bibHits_OpenAlex <-
#'   metabefor::import_search_results(
#'     OpenAlexExport_path
#'   );
#'
#' ### Look at the first five titles
#' bibHits_OpenAlex$bibHitDf$title[1:5]
#' 
#' ### Another example, using the filenames to
#' ### provide metadata about the date, database,
#' ### interface, and query specification
#' 
#' ### Path with Esbco exports
#' EbscoExport_path <-
#'   file.path(
#'     metabefor_files_path,
#'     "ebsco-exports"
#'   ); 
#'   
#' bibHits_Ebsco <-
#'   metabefor::import_search_results(
#'     EbscoExport_path
#'   );
#'   
#' ### Show the databases
#' metabefor::show_search_hits_by_database(
#'   bibHits_Ebsco
#' );
import_search_results <- function(path,
                                  dirRegex = ".*",
                                  fileRegex = "\\.ris$",
                                  dirsToIgnoreRegex = NULL,
                                  filesToIgnoreRegex = NULL,
                                  recursive = TRUE,
                                  perl = TRUE,
                                  fieldsToCopy = list(doi = c("L3", "DO"),
                                                      title = "T1"),
                                  preparatoryReplacements = NULL,
                                  synthesisr_tag_naming = "best_guess",
                                  copySep = " || ",
                                  idFieldName = "original_id",
                                  parallel = FALSE,
                                  search_metadataRegex = metabefor::opts$get("search_metadataRegex"),
                                  silent = metabefor::opts$get("silent")) {
  
  if (!requireNamespace("synthesisr", quietly = TRUE)) {
    stop("To use this function, you need to have the `synthesisr` package ",
         "installed. To install it, run:\n\n  ",
         "install.packages('synthesisr');\n");
  }
  
  search_originFile_col <- metabefor::opts$get("search_originFile_col");
  search_originDir_col <- metabefor::opts$get("search_originDir_col");
  search_originDate_col <- metabefor::opts$get("search_originDate_col");
  search_originInterface_col <- metabefor::opts$get("search_originInterface_col");
  search_originDatabase_col <- metabefor::opts$get("search_originDatabase_col");
  search_originQuerySpec_col <- metabefor::opts$get("search_originQuerySpec_col");

  ### Get all subdirectories; search hits are placed in alphabetically
  ### ordered subdirectories.
  searchHitDirs <-
    list.dirs(
      path,
      full.names = FALSE,
      recursive = recursive
    );

  ### Sort alphabetically
  searchHitDirs <-
    sort(searchHitDirs);
  
  ### Select directories using regex
  searchHitDirs <-
    grep(dirRegex, searchHitDirs, value=TRUE, perl=perl);
  
  ### Ignore directories using regex
  if (!is.null(dirsToIgnoreRegex)) {
    searchHitDirs <-
      grep(dirsToIgnoreRegex, searchHitDirs, value=TRUE, invert=TRUE, perl=perl);
  }

  ### Remove empty directories (e.g. `path`)
  searchHitDirs <-
    searchHitDirs[nchar(searchHitDirs)>0];

  if (length(searchHitDirs) == 0) {
    searchHitDirs <- basename(path);
    path <- dirname(path);
    msg("Identified no subdirectories in the specified path ('",
        path, "') that matched the regular expression. Using that ",
        "path as only directory to process.\n",
        silent = silent);
  } else {
    msg("Identified the following subdirectories in the specified path ('",
        path, "'): ", vecTxtQ(searchHitDirs), ".\n",
        silent = silent);
  }

  ### Get all files in each subdirectory
  searchHitFiles <-
    lapply(
      file.path(path, searchHitDirs),
      list.files,
      full.names = FALSE,
      recursive = FALSE
    );
  names(searchHitFiles) <- searchHitDirs;
  
  ### Select files matching the fileRegex, files matching fileToIgnoreRegex,
  ### and remove subdirectories
  searchHitFiles <-
    lapply(
      names(searchHitFiles),
      function(searchHitDirName) {
        if (length(searchHitFiles[[searchHitDirName]]) == 0) {
          return(NULL);
        } else {
          
          ### Select files using regex
          res <-
            grep(fileRegex,
                 searchHitFiles[[searchHitDirName]],
                 value=TRUE,
                 perl=perl);
          ### Ignore files using regex
          if (!is.null(filesToIgnoreRegex)) {
            res <-
              grep(filesToIgnoreRegex, res, value=TRUE, invert=TRUE, perl=perl);
          }
          ### Remove subdirectories (instead of files)          
          res <- res[
              file.exists(
                file.path(
                  path,
                  searchHitDirName,
                  res
                )
              )
            ];
          return(res);
        }
      }
    );
  names(searchHitFiles) <- searchHitDirs;

  ### Remove subdirectories with no remaining files
  searchHitFiles <-
    searchHitFiles[
      unlist(
        lapply(
          searchHitFiles,
          length
        )
      ) > 0
    ];
  
  if (length(unlist(searchHitFiles)) == 0) {
    stop("No files found that match your parameters!\n");
  } else {
    msg("Will now start to import the following files: ",
        vecTxtQ(unlist(searchHitFiles)), ".\n",
        silent = silent);
  }

  dir_file_combinations <-
    lapply(
      names(searchHitFiles),
      function(currentDirName) {
        return(
          mapply(
            c,
            rep(currentDirName, length(searchHitFiles[[currentDirName]])),
            searchHitFiles[[currentDirName]],
            SIMPLIFY = FALSE,
            USE.NAMES = FALSE
          )
        );
      }
    );
  
  ### Remove highest level of this nested list to get
  ### a list of vectors (no longer nested in directory)
  dir_file_combinations <-
    unlist(dir_file_combinations, recursive = FALSE);

  if (parallel) {
    
    stop("Parallel processing has not been implemented yet for importing ",
         "search hits.");
  
  } else {

    searchHitDfs <-
      lapply(
        dir_file_combinations,
        function(dir_and_file) {
          msg("  - ",
              silent = silent);
          
          if (is.null(preparatoryReplacements)) {
            
            res <-
              synthesisr::read_refs(
                filename = file.path(path, dir_and_file[1], dir_and_file[2]),
                tag_naming = synthesisr_tag_naming,
                verbose = !silent
              );
            
          } else {
            
            tmpFile <- tempfile(fileext = ".ris");
            tmpChar <- readLines(file.path(path, dir_and_file[1], dir_and_file[2]));
            
            for (i in seq_along(preparatoryReplacements)) {

              tmpChar <-
                gsub(preparatoryReplacements[i],
                     names(preparatoryReplacements)[i],
                     tmpChar,
                     perl = TRUE,
                     ignore.case = TRUE);
              
            }
            
            writeLines(tmpChar, tmpFile);

            res <-
              synthesisr::read_refs(
                filename = tmpFile,
                tag_naming = synthesisr_tag_naming,
                verbose = !silent
              );
            
          }
          
          res[, search_originFile_col] <- dir_and_file[2];
          res[, search_originDir_col] <- dir_and_file[1];
          
          msg("  - Imported ", nrow(res),
              " records and ", ncol(res), " fields.\n",
              silent = silent);
          
          return(res);
          
        }
      );

    bibHitDf <- metabefor::rbind_df_list(searchHitDfs);

  }
  
  if ((!is.null(fieldsToCopy)) && (length(fieldsToCopy) > 0)) {

    for (newField in names(fieldsToCopy)) {
      if (length(fieldsToCopy[[newField]]) > 0) {
        for (oldField in fieldsToCopy[[newField]]) {
          if (oldField %in% names(bibHitDf)) {
            if (newField %in% names(bibHitDf)) {
              bibHitDf[, newField] <-
                ifelse(
                  is.na(bibHitDf[, newField]) | (nchar(trimws(bibHitDf[, newField])) == 0),
                  bibHitDf[, oldField],
                  paste0(bibHitDf[, newField], copySep, bibHitDf[, oldField])
                );
            } else {
              bibHitDf[, newField] <- bibHitDf[, oldField];
            }
          }
        }
      }
    }
  }

  # searchHitDfs <-
  #   lapply(
  #     names(searchHitFiles),
  #     function(currentDirName) {
  #       return(
  #         lapply(
  #           searchHitFiles[[currentDirName]],
  #           function(currentFileName) {
  #             res <-
  #               synthesisr::read_refs(
  #                 filename = file.path(path, currentDirName, currentFileName),
  #                 verbose = !silent
  #               );
  #             res[, search_originFile_col] <- currentFileName;
  #             res[, search_originDir_col] <- currentDirName;
  #             return(res);
  #           }
  #         )
  #       );
  #     }
  #   );
  # names(searchHitDfs) <- names(searchHitFiles);
  # 
  # bibHitDf <-
  #   metabefor::rbind_df_list(
  #     lapply(
  #       searchHitDfs,
  #       metabefor::rbind_df_list
  #     )
  #   );

  justFileNames <- bibHitDf[, search_originFile_col];
  
  bibHitDf[, search_originDate_col] <-
    gsub(search_metadataRegex, "\\1", justFileNames,
         ignore.case = TRUE, perl = TRUE);
  bibHitDf[, search_originInterface_col] <-
    gsub(search_metadataRegex, "\\2", justFileNames,
         ignore.case = TRUE, perl = TRUE);
  bibHitDf[, search_originDatabase_col] <-
    gsub(search_metadataRegex, "\\3", justFileNames,
         ignore.case = TRUE, perl = TRUE);
  bibHitDf[, search_originQuerySpec_col] <-
    gsub(search_metadataRegex, "\\4", justFileNames,
         ignore.case = TRUE, perl = TRUE);
  
  if (all(bibHitDf[, search_originDate_col] == justFileNames)) {
    bibHitDf[, search_originDate_col] <- NULL;
  }
  if (all(bibHitDf[, search_originInterface_col] == justFileNames)) {
    bibHitDf[, search_originInterface_col] <- NULL;
  }
  if (all(bibHitDf[, search_originDatabase_col] == justFileNames)) {
    bibHitDf[, search_originDatabase_col] <- NULL;
  }
  if (all(bibHitDf[, search_originQuerySpec_col] == justFileNames)) {
    bibHitDf[, search_originQuerySpec_col] <- NULL;
  }
  
  if (!is.null(idFieldName)) {
    names(bibHitDf)[grepl("^[iI][dD]$", names(bibHitDf))] <-
      idFieldName;
  }

  res <-
    list(input = list(path = path,
                      dirRegex = dirRegex,
                      fileRegex = fileRegex,
                      recursive = recursive,
                      search_metadataRegex = search_metadataRegex,
                      silent = silent),
         searchHitDirs = searchHitDirs,
         searchHitFiles = searchHitFiles,
         searchHitDfs = searchHitDfs,
         bibHitDf = bibHitDf);

  class(res$bibHitDf) <- c("metabefor", "mbfBibHitDf", class(res$bibHitDf));
  
  class(res) <- c("metabefor", "mbfSearch", class(res));
  
  return(res);
  
}
