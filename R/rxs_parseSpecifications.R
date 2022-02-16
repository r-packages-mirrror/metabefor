#' Parse Rxs (R extraction script) specifications
#' 
#' This function parses the specifications from
#' an Rxs specification spreadsheet.
#'
#' @param entities The entity specification, as a data frame
#' @param valueTemplates The value templates, as a data frame
#' @param moduleName The name of the module; only used to make error messages
#' more informative, and only used if not NULL.
#' @param definitions The definitions, as a data frame
#' @param rxsRootName The name of the root node of the Rxs tree
#'
#' @return An `rxsStructure` object
#' @export
rxs_parseSpecifications <- function(entities,
                                    valueTemplates,
                                    definitions = NULL,
                                    moduleName = NULL,
                                    rxsRootName = metabefor::opts$get("rxsRootName"),
                                    silent = metabefor::opts$get("silent")) {

  eC <- metabefor::opts$get("entityColNames");
  valueTemplateCols <- metabefor::opts$get("valueTemplateColNames");
  
  res <- list(parsedEntities = rxs_parseEntities(entities = entities,
                                                 rxsRootName = rxsRootName,
                                                 moduleName = NULL,
                                                 silent = silent),
              parsedValueTemplates = rxs_parseValueTemplates(valueTemplateDataframe=valueTemplates));

  class(res) <- "rxsStructure";

  return(res);

}



