#' Write the extractor instructions
#'
#' @param x An rxsStructure object, as imported
#' by metabefor::rxs_fromSpecifications().
#' @param output The output file to write to.
#'
#' @return
#' @export
#'
#' @examples
write_extractor_instructions <- function(x,
                                         output = "asis",
                                         headingLevel = metabefor::opts$get("defaultHeadingLevel")) {
  
  instructionsColNames <- metabefor::opts$get("instructionsColNames");
  rxsSheetnames <- metabefor::opts$get("rxsSheetnames");
  
  browser();
  
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
        rxsSpecObject$rxsInstructions,
        rxsSpecObject$entityOverview_list,
        "\n",
        sep = "\n"
      );

  } else {
    
    stop("You have to pass either an object of class `rxsStructure` or ",
         "an object of class `rxsStructures`; basically, the object ",
         "returned by metabefor::rxs_fromSpecifications().");

  }
  
  if (output == "asis") {
    return(
      knitr::asis_output(
        res
      )
    );
  } else {
    return(res);
  }

}