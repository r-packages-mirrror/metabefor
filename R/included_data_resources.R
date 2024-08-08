###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

### Easily update everything
updateEverything <- FALSE;

### Potentially load local package
# devtools::load_all();

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

#' An very rudimentary example Rxs specification
#'
#' This is a simple and relatively short Rxs specification. One version is
#' parsed into a single Rxs template, and the other into three modules.
#'
#' @rdname rxs_minimal_example_1
#' @aliases rxs_minimal_example_1 rxs_minimal_modular_example_1
#' @format An example of an Rxs specification.
#'
"rxs_minimal_example_1"
"rxs_minimal_modular_example_1"

if (exists("updateEverything") && updateEverything) {

  extDataDir <- here::here("inst", "extdata");

  gSheet_url_rxsEx_1 <-
    "https://docs.google.com/spreadsheets/d/1Ty38BS7MVXOgC-GJ6zzr7E3rC_vQNOMKe-uCvIuHs3c";

  localBackupFile_1 <-
    file.path(extDataDir, "Rxs_minimal_example_1.xlsx");
  
  minimalTemplateFile_1 <-
    file.path(extDataDir, "Rxs_minimal_example_noModules_1.Rxs.Rmd");
  
  modulesTemplateFilenamePattern_1 <- "Rxs_minimal_example_1_%s";
  
  rxs_minimal_example_1 <-
    metabefor::rxs_fromSpecifications(
      gSheet_url_rxsEx_1,
      localBackup = localBackupFile_1,
      silent = metabefor::opts$get("silent"),
      outputFile = minimalTemplateFile_1,
      ignoreModules = TRUE
    );
  
  rxs_minimal_modular_example_1 <-
    metabefor::rxs_fromSpecifications(
      gSheet_url_rxsEx_1,
      outputPath = extDataDir,
      outputFilenamePattern = modulesTemplateFilenamePattern_1,
      silent = metabefor::opts$get("silent")
    );
  
  usethis::use_data(rxs_minimal_example_1, overwrite=TRUE);
  usethis::use_data(rxs_minimal_modular_example_1, overwrite=TRUE);

}

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

#' Simple bibliographic exports from OpenAlex
#'
#' A number of simple small datasets with bibliographic information.
#'
#' These are the datasets obtained by searching,
#' in works' titles, for respectively:
#' 
#' - `openalex_example_1`: 2024-08-06, `"evidence synthesis" AND "extraction"`,
#' <https://openalex.org/works?filter=display_name.search%3A"evidence%20synthesis"%20AND%20"extraction">
#' - `openalex_example_2`: 2024-08-06, `"evidence synthesis" AND "appraisal"`,
#' <https://openalex.org/works?filter=display_name.search%3A"evidence%20synthesis"%20AND%20"appraisal">
#'
#' @rdname openalex_examples
#' @aliases openalex_example_1 openalex_example_2
#' @format A data frame
#'
"openalex_example_1"
"openalex_example_2"

if (exists("updateEverything") && updateEverything) {
  
  extDataDir <- here::here("inst", "extdata", "openalex-exports");
  
  openalex_example_1 <-
    metabefor::import_openalex(
      file.path(
        extDataDir,
        "example-1---CSV-format---works-2024-08-06T12-48-16.csv"
      )
    );
  
  openalex_example_2 <-
    metabefor::import_openalex(
      file.path(
        extDataDir,
        "example-2---CSV-format---works-2024-08-06T13-22-17.csv"
      )
    );
  
  usethis::use_data(openalex_example_1, overwrite=TRUE);
  usethis::use_data(openalex_example_2, overwrite=TRUE);
  
}

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

#' A minimal Rxs project with two sources extracted in three modules
#'
#' What it says on the tin.
#'
#' @format An Rxs project object (class ``)
#'
"example_rxsProject_1"

if (exists("updateEverything") && updateEverything) {
  
  rxsProject_path <-
    here::here("inst", "extdata", "extraction-examples");
  # system.file(
    #   "extdata",
    #   "extraction-examples",
    #   package = "metabefor"
    # ); 
  
  ### Parse the files
  example_rxsProject_1 <-
    metabefor::rxs_parseExtractionScripts(
      rxsProject_path
    );

  usethis::use_data(example_rxsProject_1, overwrite=TRUE);
  
}

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------

#' A very rudimentary example Rxs specification
#'
#' This is a simple and relatively short Rxs specification.
#'
#' @rdname rxs_minimal_example_2
#' @aliases rxs_minimal_example_2
#' @format An example of an Rxs specification.
#'
"rxs_minimal_example_2"

if (exists("updateEverything") && updateEverything) {
  
  extDataDir <- here::here("inst", "extdata");
  
  gSheet_url_rxsEx_2 <-
    "https://docs.google.com/spreadsheets/d/1TwT-ZeJYLM6bqbV-JG6JOqX_y5E-6xoU25Pyr4IoYAU";
  
  localBackupFile_2 <-
    file.path(extDataDir, "Rxs_minimal_example_2.xlsx");
  
  minimalTemplateFile_2 <-
    file.path(extDataDir, "Rxs_minimal_example_2.Rxs.Rmd");

  rxs_minimal_example_2 <-
    metabefor::rxs_fromSpecifications(
      gSheet_url_rxsEx_2,
      localBackup = localBackupFile_2,
      silent = metabefor::opts$get("silent"),
      outputFile = minimalTemplateFile_2,
      ignoreModules = TRUE
    );

  usethis::use_data(rxs_minimal_example_2, overwrite=TRUE);

}

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
