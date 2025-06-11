# read_sheet <- function(x,
#                        sheet = NULL,
#                        columnDictionary = NULL,
#                        localBackup = NULL) {
#   
#   if (!is.character(x)) {
#     stop("As `x`, you must pass a character value (i.e. a single string). ",
#          "Instead, you passed something with class ", vecTxt(class(x)), ".");
#   }
#   
#   if (!(length(x) == 1)) {
#     stop("As `x`, you must pass a character value (i.e. a single string). ",
#          "Instead, you passed a character vector with ", length(x),
#          " elements.");
#   }
#   
#   x <- trimws(x);
#   
#   if (grepl("^http", x) && grepl("google\\.com/spreadsheets/d/", x)) {
#     
#     if (requireNamespace("googlesheets4", quietly = TRUE)) {
# 
#       ### Indicate we want to use the Google Sheets API without authenticating
#       googlesheets4::gs4_deauth();
#       
#       sheetNames <- googlesheets4::sheet_names(x);
#       
#       res <-
#         lapply(
#           sheetNames,
#           googlesheets4::read_sheet,
#           ss = x
#         );
#       names(res) <- sheetNames;
#       
#       downloaded <- TRUE;
#       fileToRead <- NULL;
#       
#     } else {
#       stop("To read Google Sheets URLs, the {googlesheets4} package ",
#            "has to be installed. You can install it with:\n\n",
#            "  install.packages('googlesheets4');\n");
#     }
#     
#   } else if (grepl("^http", x)) {
# 
#     extension <- tools::file_ext(x);
#     
#     fileToRead <- tempfile(fileext = extension);
#     
#     downloadResult <- utils::download.file(x, fileToRead);
#     
#     downloaded <- TRUE;
# 
#     if (!(downloadResult == 0)) {
#       
#       stop("Failed to download a file from URL ", x, ".");
#       
#     }
#     
#   } else if (file.exists(x)) {
#       
#     fileToRead <- x;
#     extension <- tools::file_ext(x);
#     downloaded <- FALSE;
#     
#   } else {
#     
#     stop("In `x`, I couldn't recognize a Google Sheets URL, an URL to a file ",
#          "hosted on a website, or a path to an existing file.");
#     
#   }
#   
#   if (!is.null(fileToRead)) {
#     
#     extension <- trimws(tolower(extension));
#     
#     if (extension == "xlsx") {
#       if (requireNamespace("openxlsx", quietly = TRUE)) {
#         res <- openxlsx::read.xlsx(fileToRead);
#         
#       } else {
#         stop("To read Excel spreadsheets (`.xlsx`), the {openxlsx} package ",
#              "has to be installed. You can install it with:\n\n",
#              "  install.packages('openxlsx');\n");
#       }
#     } else if (extension == "sav") {
#       if (requireNamespace("haven", quietly = TRUE)) {
#         res <- haven::read_sav(fileToRead);
#       } else {
#         stop("To read SPSS files (`.sav`), the {haven} package ",
#              "has to be installed. You can install it with:\n\n",
#              "  install.packages('haven');\n");
#       }
#     } else if (extension == "csv") {
#       res <- utils::read.csv(x);
#     } else {
#       stop("Sorry, but I can't (yet) read files with the extension of the ",
#            "file you provided me with (", extension, ").");
#     }
#     
#   }
#   
#   if (downloaded && (!is.null(localBackup))) {
#     
#     if (!is.null(downloadResult) && (downloadResult == 0)) {
#      
#       file.copy(fileToRead, localBackup);
#        
#     } else {
#     
#       warning("Functionality to store a local copy not yet implemented!");
#       
#     }
#     
#   }
# 
#   if (length(res) == 1) {
#     res <- as.data.frame(res[[1]]);
#   }
#   
#   return(res);
#   
#   
# }
#                        