#' Title
#'
#' @param entities 
#' @param valueTemplates 
#' @param definitions 
#' @param eC 
#' @param valueTemplateCols 
#' @param rootName 
#'
#' @return
#' @export
#'
#' @examples
rxs_parseSpecifications <- function(entities,
                                    valueTemplates,
                                    definitions = NULL,
                                    eC = metabefor::opts$get(entityColNames),
                                    valueTemplateCols = metabefor::opts$get(valueTemplateColNames),
                                    rootName = 'study') {

  res <- list(parsedEntities = rxs_parseEntities(entities = entities,
                                                 eC = eC,
                                                 rootName = rootName),
              parsedValueTemplates = rxs_parseValueTemplates(valueTemplateDataframe=valueTemplates,
                                                             valueTemplateCols = valueTemplateCols));

  class(res) <- "rxsStructure";

  return(res);

}



