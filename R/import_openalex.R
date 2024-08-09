#' Import an OpenAlex CSV export
#'
#' @param file The file to import
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
#'     "example-1---CSV-format---works-2024-08-06T12-48-16.csv",
#'     package = "metabefor"
#'   );
#'
#' exampleDat <-
#'   import_openalex(
#'     openAlex_CSV
#'   );
#'
#' exampleDat$author;
import_openalex <- function(file,
                            extraFields = NULL) {
  
  openAlex_extract_csv_fields <-
    metabefor::opts$get("openAlex_extract_csv_fields");
  
  if (!file.exists(file)) {
    stop("The `file` that you specified ('", file, "') does not exist!");
  }
  
  fileExtension <- tools::file_ext(file);
  
  if (!is.null(extraFields)) {
    fields <- c(openAlex_extract_csv_fields, extraFields);
  } else {
    fields <- openAlex_extract_csv_fields;
  }
  
  if (grepl("csv", fileExtension, ignore.case = TRUE)) {
    
    importedDf <- utils::read.csv(file);
    
    res <- importedDf[, fields];
    
    res$author <- res$authorships.raw_author_name;
    res$author <- gsub("\\|", " and ", res$author);
    
    res$year <- res$publication_year;
    
  } else if (grepl("txt", fileExtension, ignore.case = TRUE)) {
    stop("Sorry, functionality to import WoS files from OpenAlex has not yet been implemented!");
  } else if (grepl("ris", fileExtension, ignore.case = TRUE)) {
    stop("Sorry, functionality to import RIS files from OpenAlex has not yet been implemented!");
  } else {
    stop("I can read files with extension `.csv`, `.txt`, and `.ris`. ",
         "However, the file you specified ('", file,
         "')has a different extension.");
  }
  
  return(res);
  
}