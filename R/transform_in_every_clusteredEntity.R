#' Perform a transformation for every clustering entity
#' 
#' This function takes a `studies` object (as produced
#' by [metabefor::rxs_parseExtractionScripts()]) and processes all study
#' trees, looking for clustering entities that match the `entityId_regex`
#' regular expression (for information about what clustering entities are,
#' see <https://r-packages.gitlab.io/metabefor/articles/definitions.html>)
#' and/or that contain a field matching the `requiredField_regex` regular
#' expression, and passes those to function `fun` with `funArgs` as
#' arguments. For `funArgs`, you can pass entity identifiers of clustered
#' entities contained in the clustering entity. These are then assigned the
#' corresponding name before `fun` is called using `do.call`. This allows 
#' you to select values and rename them to match the function arguments.
#'
#' @param studies The `studies` object (as produced
#' by [metabefor::rxs_parseExtractionScripts()]).
#' @param newEntityName The name of the new entity to add to the clustering
#' entity.
#' @param fun The function to apply.
#' @param funArgs The arguments, as a character vector where each element is
#' a clustered entity stored in the clustering entity, and each element's name
#' is how that entity's value should be passed to `fun` (allowing you to
#' specify that you want to pass, for example, `uni.mean` and `uni.sd` as
#' arguments `mean` and `sd`, respectively, to `fun`). Anything in this vector
#' that is not the identifier of a clustered entity is passed as is.
#' @param entityId_regex An optional regular expression: if specified, only
#' entity nodes with entity identifiers that match this regular expression will
#' be processed.
#' @param requiredField_regex An optional regular expression specifying a field
#' that the clustering entity must contain for it to be processed.
#'
#' @return Invisibly, the studies object. Note that the study trees will be
#' changed in place given `data.tree`'s pass-by-reference logic; so you can
#' discard the result.
#' @export
#'
#' @examples
transform_in_every_clusteringEntity <- function(studies,
                                                newEntityName,
                                                fun,
                                                funArgs,
                                                entityId_regex = NULL,
                                                requiredField_regex = NULL) {
  
  if (!inherits(studies, "rxs_parsedExtractionScripts")) {
    stop("The object you pass as 'studies' must be an object ",
         "with parsed Rxs files, as produced by a call to ",
         "metabefor::rxs_parseExtractionScripts().");
  }
  
  if (!is.function(fun)) {
    stop("As `fun`, you must pass a function.");
  }
  
  for (currentStudyId in names(studies$rxsTrees)) {
    
    ### Traverse tree and add standardized means (where applicable)
    studies$rxsTrees[[currentStudyId]]$Do(
      
      ### For every node for which the filterFun specified
      ### below returns TRUE, execute this function:
      
      function(entityNode) {
        
        if (!is.null(funArgs)) {
          for (varName in names(funArgs)) {
            if (funArgs[varName] %in% names(entityNode$value)) {
              assign(varName, entityNode$value[[funArgs[varName]]]);
            } else {
              assign(varName, funArgs[varName]);
            }
          }
        }
        
        entityNode$value[[newEntityName]] <-
          do.call(fun,
                  args=as.list(environment())[names(funArgs)]);
        
      },
      
      ### The filter function that checks whether
      ### a node should be processed
      
      filterFun = function(currentNode) {
        
        selected <- TRUE;
        
        if (!is.null(entityId_regex)) {
          selected <- grepl(entityId_regex, currentNode$name);
        }
        
        if (selected && !is.null(requiredField_regex)) {
          
          ### Skip nodes that don't have values
          ### or where the value isn't a list
          if (is.null(currentNode$value) || (!is.list(currentNode$value))) {
            selected <- FALSE;
          } else {
            selected <- any(grepl(requiredField_regex, names(currentNode$value)));
          }
        }
        
        return(selected);
        
      }
    );
    
  }
  
  return(invisible(studies));
  
}
