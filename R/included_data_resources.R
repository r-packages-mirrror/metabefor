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
#' @rdname rxs_minimal_example
#' @aliases rxs_minimal_example rxs_minimal_modular_example
#' @format An example of an Rxs specification.
#'
"rxs_minimal_example"
"rxs_minimal_modular_example"

if (exists("updateEverything") && updateEverything) {

  extDataDir <- here::here("inst", "extdata");

  gSheet_url <-
    "https://docs.google.com/spreadsheets/d/1Ty38BS7MVXOgC-GJ6zzr7E3rC_vQNOMKe-uCvIuHs3c";

  localBackupFile <-
    file.path(extDataDir, "Rxs_minimal_example.xlsx");
  
  minimalTemplateFile <-
    file.path(extDataDir, "Rxs_minimal_example_noModules.Rxs.Rmd");
  
  modulesTemplateFilenamePattern <- "Rxs_minimal_example_%s";
  
  rxs_minimal_example <-
    metabefor::rxs_fromSpecifications(
      gSheet_url,
      localBackup = localBackupFile,
      silent = metabefor::opts$get("silent"),
      outputFile = minimalTemplateFile,
      ignoreModules = TRUE
    );
  
  rxs_minimal_modular_example <-
    metabefor::rxs_fromSpecifications(
      gSheet_url,
      outputPath = extDataDir,
      outputFilenamePattern = modulesTemplateFilenamePattern,
      silent = metabefor::opts$get("silent")
    );
  
  usethis::use_data(rxs_minimal_example, overwrite=TRUE);
  usethis::use_data(rxs_minimal_modular_example, overwrite=TRUE);

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
