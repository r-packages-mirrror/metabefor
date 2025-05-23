#' Import an OpenAlex CSV export
#'
#' @param filename The file to import
#' @param extraFields Extra fields to store in the resulting dataframe, used
#' when exporting from CSV
#'
#' @return A data frame.
#' @export
#'
#' @examples openAlex_CSV <-
#'   system.file(
#'     "extdata",
#'     "openalex-exports",
#'     "example-1---OpenAlex---CSV-format.csv",
#'     package = "metabefor"
#'   );
#'
#' exampleDat <-
#'   import_openalex(
#'     openAlex_CSV
#'   );
#'
#' exampleDat$author;
import_openalex <- function(filename,
                            extraFields = NULL) {
  
  openAlex_extract_csv_fields <-
    metabefor::opts$get("openAlex_extract_csv_fields");
  
  if (!file.exists(filename)) {
    stop("The `file` that you specified ('", filename,
         "') does not exist!");
  }
  
  fileExtension <- tools::file_ext(filename);
  
  if (!is.null(extraFields)) {
    fields <- c(openAlex_extract_csv_fields, extraFields);
  } else {
    fields <- openAlex_extract_csv_fields;
  }
  
  if (grepl("csv", fileExtension, ignore.case = TRUE)) {
    
    importedDf <- utils::read.csv(filename);
    
    res <- importedDf[, fields];
    
    res$author <- res$authorships.raw_author_name;

    res$author <- 
      tryCatch(
        gsub("\\|", " and ", res$author),
        error = function(e) {
          stop("I ran into an error processing this file; it may contain ",
               "problems in its encoding. Try to re-encode it as UTF-8.");
        }
      );
    
    res$year <- res$publication_year;
    
  } else if (grepl("txt", fileExtension, ignore.case = TRUE)) {
    stop("Sorry, functionality to import WoS files from OpenAlex has not yet been implemented!");
  } else if (grepl("ris", fileExtension, ignore.case = TRUE)) {
    stop("Sorry, functionality to import RIS files from OpenAlex has not yet been implemented!");
  } else {
    stop("I can read files with extension `.csv`, `.txt`, and `.ris`. ",
         "However, the file you specified ('", filename,
         "') has a different extension.");
  }
  
  return(res);
  
}