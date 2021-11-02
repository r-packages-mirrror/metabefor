#' Parse Rxs (R extraction script) specifications
#' 
#' This function parses the specifications from
#' an Rxs specification spreadsheet.
#'
#' @param entities The entity specification, as a data frame
#' @param valueTemplates The value templates, as a data frame
#' @param definitions The definitions, as a data frame
#' @param eC The entity column names, as a names vector
#' @param valueTemplateCols The value template column names, as a named vector
#' @param rootName The name of the root node of the study tree
#'
#' @return An `rxsStructure` object
#' @export
rxs_parseSpecifications <- function(entities,
                                    valueTemplates,
                                    definitions = NULL,
                                    eC = metabefor::opts$get("entityColNames"),
                                    valueTemplateCols = metabefor::opts$get("valueTemplateColNames"),
                                    rootName = 'study') {

  res <- list(parsedEntities = rxs_parseEntities(entities = entities,
                                                 eC = eC,
                                                 rootName = rootName),
              parsedValueTemplates = rxs_parseValueTemplates(valueTemplateDataframe=valueTemplates,
                                                             valueTemplateCols = valueTemplateCols));

  class(res) <- "rxsStructure";

  return(res);

}



