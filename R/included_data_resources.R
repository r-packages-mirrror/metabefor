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
    file.path(extDataDir, "rxs_minimal_example.xlsx");
  
  minimalTemplateFile <-
    file.path(extDataDir, "rxs_minimal_example_noModules.rxs.Rmd");
  
  modulesTemplateFilenamePattern <- "rxs_minimal_example_%s";
  
  rxs_minimal_example <-
    metabefor::rxs_fromSpecifications(
      gSheet_url,
      localBackup = localBackupFile,
      silent=FALSE,
      outputFile = minimalTemplateFile,
      ignoreModules = TRUE
    );
  
  rxs_minimal_modular_example <-
    metabefor::rxs_fromSpecifications(
      gSheet_url,
      outputPath = extDataDir,
      outputFilenamePattern = modulesTemplateFilenamePattern,
      silent=FALSE
    );
  
  usethis::use_data(rxs_minimal_example, overwrite=TRUE);
  usethis::use_data(rxs_minimal_modular_example, overwrite=TRUE);
  
}

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
