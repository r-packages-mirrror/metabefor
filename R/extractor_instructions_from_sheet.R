#' Create extractor instructions from a spreadsheet
#'
#' @param x The spreadsheet
#' @param headingLevel The top-most heading level to use
#'
#' @return The formatted extractor instructions
#' @export
#'
#' @examples
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

