#' Show and/or write the extractor instructions
#'
#' @param x An rxsStructure object, as imported
#' by metabefor::rxs_fromSpecifications().
#' @param output The format of the object to return: `"asis"` to return content
#' ready to be included in a knitted file; `"raw"` for the raw results; or
#' `"none"` to return the raw result invisibly.
#' @param outputFile Optionally a file to write the extractor instructions to.
#' @param title,author The title and author to use when exporting.
#' @param silent Whether to be silent or chatty.
#'
#' @return
#' @export
#'
#' @examples ### Load an example rxs specification
#' data("rxs_minimal_example_2", package="metabefor");
#' 
#' ### "Write" the extractor instructions but store
#' ### them in a character vector instead of writing
#' ### them to a file
#' extractorInstructions <-
#'   metabefor::write_extractor_instructions(
#'     rxs_minimal_example_2
#'   );
#'
#' ### Show the beginning
#' cat(substr(extractorInstructions, 1, 100));
#' 
#' ### Show the middle
#' cat(substr(extractorInstructions, 1000, 1100))
write_extractor_instructions <- function(x,
                                         output = "asis",
                                         outputFile = NULL,
                                         title = "Extractor Instructions",
                                         author = "",
                                         silent = TRUE) {
  
  instructionsColNames <- metabefor::opts$get("instructionsColNames");
  rxsSheetnames <- metabefor::opts$get("rxsSheetnames");

  if (inherits(x, "rxsStructure")) {
    
    res <-
      paste0(
        "\n",
        x$rxsInstructions,
        x$entityOverview_list,
        "\n",
        sep = "\n"
      );
    
  } else if (inherits(x, "rxsStructures")) {

    res <-
      paste0(
        "\n",
        x$rxsInstructions,
        "\n\n",
        paste0(
          paste0(
            "**Module: ",
            names(x$entityOverviews_list),
            "**\n",
            x$entityOverviews_list
          ),
          collapse = "\n"
        ),
        "\n",
        sep = "\n"
      );
    
    

  } else {
    
    stop("You have to pass either an object of class `rxsStructure` or ",
         "an object of class `rxsStructures`; basically, the object ",
         "returned by metabefor::rxs_fromSpecifications().\n\nInstead, ",
         "you passed an object of class(es): ", vecTxtQ(class(rxsSpecObject)));

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
          outputFile, "'.\n",
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