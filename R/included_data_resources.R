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

  gSheet_url <-
    "https://docs.google.com/spreadsheets/d/1Ty38BS7MVXOgC-GJ6zzr7E3rC_vQNOMKe-uCvIuHs3c";

  localBackupFile <-
    here::here(
      "inst", "extdata", "rxs_minimal_example.xlsx"
    );

  rxs_minimal_example <-
    metabefor::rxs_fromSpecifications(
      gSheet_url,
      localBackup = localBackupFile,
      silent=FALSE,
      ignoreModules = TRUE
    );

  usethis::use_data(rxs_minimal_example, overwrite=TRUE);
  
  rxs_minimal_modular_example <-
    metabefor::rxs_fromSpecifications(
      gSheet_url,
      localBackup = localBackupFile,
      silent=FALSE
    );
  
  usethis::use_data(rxs_minimal_modular_example, overwrite=TRUE);
  
}

###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
###-----------------------------------------------------------------------------
