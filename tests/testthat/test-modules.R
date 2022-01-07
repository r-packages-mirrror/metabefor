test_that("multiple modules are imported and merged correctly", {
  
  # devtools::load_all();
  
  rxsPath <-
    system.file(
      "extdata",
      "extraction-examples",
      package = "metabefor"
    );

  rxs <- metabefor::rxs_parseExtractionScripts(rxsPath, silent = metabefor::opts$get("silent"));
  
})
