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
#' @param recursive Whether to recursively read subdirectories.
#' @param search_metadataRegex A regular expression to match against the
#' filenames. If it matches, metadata will be extracted in three capturing
#' groups, in the order data, interface, and database.
#' @param silent Whether to be silent or chatty.
#'
#' @return An object with all the imported information, including, most
#' importantly, the data frame `bibHitDf` with all results.
#' @export
#'
#' @examples
import_search_results <- function(path,
                                  dirRegex = ".*",
                                  fileRegex = "\\.ris$",
                                  recursive = TRUE,
                                  search_metadataRegex = metabefor::opts$get("silent"),
                                  silent = metabefor::opts$get("silent")) {
  
  if (!requireNamespace("synthesisr", quietly = TRUE)) {
    stop("To use this function, you need to have the `synthesisr` package ",
         "installed. To install it, run:\n\n  ",
         "install.packages('synthesisr');\n");
  }
  
  search_originFile_col <- metabefor::opts$get("search_originFile_col");
  search_originDir_col <- metabefor::opts$get("search_originDir_col");
  search_originDatabase_col <- metabefor::opts$get("search_originDatabase_col");
  search_originInterface_col <- metabefor::opts$get("search_originInterface_col");
  search_originDate_col <- metabefor::opts$get("search_originDate_col");

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
    grep(dirRegex, searchHitDirs, value=TRUE);
  
  msg("Identified the following subdirectories in the specified path ('",
      path, "': ", vecTxtQ(searchHitDirs), ".\n",
      silent = silent);
  
  ### Get all files in each subdirectory
  searchHitFiles <-
    lapply(
      file.path(path, searchHitDirs),
      list.files,
      full.names = FALSE,
      pattern = fileRegex,
      ignore.case = TRUE,
      recursive = FALSE
    );
  names(searchHitFiles) <- searchHitDirs;
  
  ### Remove subdirectories
  searchHitFiles <-
    lapply(
      searchHitDirs,
      function(fileList) {
        if (length(fileList) == 0) {
          return(NULL);
        } else {
          return(
            fileList[file.exists(fileList)]
          );
        }
      }
    );

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
  
  msg("Will now start to import the following files: ",
      vecTxtQ(unlist(searchHitFiles)), ".\n",
      silent = silent);

  searchHitDfs <-
    lapply(
      names(searchHitFiles),
      function(currentDirName) {
        return(
          lapply(
            searchHitFiles[[currentDirName]],
            function(currentFileName) {
              res <-
                synthesisr::read_refs(
                  filename = file.path(path, currentDirName, currentFileName),
                  verbose = !silent
                );
              res[, search_originFile_col] <- currentFileName;
              res[, search_originDir_col] <- currentDirName;
              return(res);
            }
          )
        );
      }
    );
  names(searchHitDfs) <- names(searchHitFiles);
  
  bibHitDf <-
    metabefor::rbind_df_list(searchHitDfs);

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

  class(res) <- c("metabefor", "mbfSearch");
  
  return(res);
  
}
