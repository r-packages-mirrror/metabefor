#' Create extractor instructions from a spreadsheet
#' 
#' This function produces a Markdown document with the extractor instructions
#' as provided in an Rxs Specification.
#'
#' @param x The spreadsheet with extractor instructions; or an rxs
#' specification (as produced by a call to [rxs_fromSpecifications()]).
#' @param headingLevel The top-most heading level to use
#' @param ... Any additional arguments (ignored)
#'
#' @return The formatted extractor instructions
#' @export
#'
#' @examples ### Load an example rxs specification
#' data("rxs_minimal_example_2", package="metabefor");
#' 
#' ### Produce the instructions in Markdown format
#' extractorInstructions <-
#'   extractor_instructions_from_sheet(
#'     rxs_minimal_example_2
#'   );
#'   
#' ### Show the produced markdown
#' cat(extractorInstructions);
extractor_instructions_from_sheet <- function(x,
                                              headingLevel = 3) {
  
  ###---------------------------------------------------------------------------
  ### Get options
  ###---------------------------------------------------------------------------
  
  eC <- metabefor::opts$get("entityColNames");
  ws <- metabefor::opts$get("rxsSheetnames");
  valueTemplateCols <- metabefor::opts$get("valueTemplateColNames");
  instructionsCols <- metabefor::opts$get("instructionsColNames");
  definitionsCols <- metabefor::opts$get("definitionsColNames");
  
  if (inherits(x, "rxsStructure")) {
    x <- x$rxsSpecification$instructionSheet;
  } else if (!inherits(x, "data.frame")) {
    stop("As `x`, provide either a spreadsheet with the extractor ",
         "instructions, or an Rxs specification as produced by a call to ",
         "`metabefor::rxs_fromSpecifications()`. The object you provided ",
         "had class(es) ", vecTxtQ(class(x)), ".");
  }

  res <-
    paste0(
      heading(
        "Extractor instructions",
        headingLevel = headingLevel,
        cat = FALSE
      ),
      paste0(
        lapply(
          1:nrow(x),
          function(i) {
            return(
              paste0(
                heading(
                  x[[instructionsCols$headingCol]][i],
                  headingLevel = headingLevel + 1,
                  cat = FALSE
                ),
                x[[instructionsCols$descriptionCol]][i]
              )
            );
          }
        ),
        collapse = "\n\n"
      )
    );
  
  return(res);
  
}

