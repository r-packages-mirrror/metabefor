#' Show and/or write the extractor instructions
#'
#' @param x An rxsStructure object, as imported
#' by metabefor::rxs_fromSpecifications().
#' @param output The format of the object to return: `"asis"` to return content
#' ready to be included in a knitted file; `"raw"` for the raw results; or
#' `"none"` to return the raw result invisibly.
#' @param outputFile Optionally a file to write the extractor instructions to.
#' @param title,author The title and author to use when exporting.
#'
#' @return
#' @export
#'
#' @examples
write_extractor_instructions <- function(x,
                                         output = "asis",
                                         outputFile = NULL,
                                         title = "Extractor Instructions",
                                         author = "") {
  
  instructionsColNames <- metabefor::opts$get("instructionsColNames");
  rxsSheetnames <- metabefor::opts$get("rxsSheetnames");

  if (inherits(x, "rxsStructures")) {
    
    moduleInstructions <-
      lapply(
        x,
        write_extractor_instructions,
        output = NULL
      );
    
    res <- unlist(moduleInstructions);
    
  } else if (inherits(x, "rxsStructure")) {

    # res <-
    #   unlist(
    #     apply(
    #       x$rxsSpecification$instructionSheet,
    #       1,
    #       function(currentRow) {
    #         return(
    #           c(
    #             metabefor::heading(
    #               currentRow[instructionsColNames$headingCol],
    #               headingLevel = headingLevel,
    #               cat = FALSE
    #             ),
    #             unname(currentRow[instructionsColNames$descriptionCol])
    #           )
    #         );
    #       },
    #       simplify = FALSE
    #     )
    #   )
    
    res <-
      paste0(
        "\n",
        x$rxsInstructions,
        x$entityOverview_list,
        "\n",
        sep = "\n"
      );

  } else {
    
    stop("You have to pass either an object of class `rxsStructure` or ",
         "an object of class `rxsStructures`; basically, the object ",
         "returned by metabefor::rxs_fromSpecifications().");

  }
  
  if (!is.null(outputFile)) {
    
    if (!requireNamespace('rmarkdown', quietly=TRUE)) {
      stop("You need to have 'rmarkdown' installed to export extractor instructions!");
    }

    paramsToPass <-
      list(title = title,
           author = author);
    
    if (dir.exists(dirname(outputFile))) {

      rmarkdown::render(
        input = system.file("templates",
                            "_metabefor_extractor-instructions_full_template_for_pdf.Rmd",
                            package = "metabefor"),
        params = paramsToPass,
        output_file = outputFile,
        quiet = TRUE
      );
      
      msg("Exported the extractor instructions to PDF file '",
          file, "'.\n",
          silent = silent);
      
    } else {
      
      stop("The path that you specified to save the file in ('",
           dirname(outputFile), "') does not exist.");
      
    }

  }
  
  if (output == "asis") {
    return(
      knitr::asis_output(
        res
      )
    );
  } else if (output == "none") {
    return(invisible(res));
  } else {
    return(res);
  }

}