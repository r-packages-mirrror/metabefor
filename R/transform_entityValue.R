#' Perform a transformation selectively
#' 
#' This function takes a full Rxs project object (as produced
#' by [metabefor::rxs_parseExtractionScripts()]) and processes all Rxs
#' trees, looking for entities that match the `entityId_regex`
#' regular expression (for information about what clustering entities are,
#' see <https://r-packages.gitlab.io/metabefor/articles/definitions.html>)
#' and passes those to function `fun`, passing the entity's value as the
#' argument named in `entityValue_argName` and with `funArgs` as additional
#' arguments.
#'
#' @param x The full Rxs project object (as produced
#' by [metabefor::rxs_parseExtractionScripts()]).
#' @param newEntityName_prefix,newEntityName_suffix The prefix and suffix to
#' add to the entity identifier to create the name of the new entity that is
#' created. That entity is added as a sibling of the target entity.
#' @param fun The function to apply.
#' @param entityValue_argName The argument name to pass the entity value as.
#' @param funArgs Additional arguments, as a names list with each element's
#' name is the argument name and the element itself the content.
#' @param entityId_regex An optional regular expression: if specified, only
#' entity nodes with entity identifiers that match this regular expression will
#' be processed.
#' @param requiredField_regex This functionality has not yet been implemented.
#'
#' @return Invisibly, the full Rxs project object. Note that the Rxs trees
#' will be changed in place given `data.tree`'s pass-by-reference logic; so
#' you can discard the result.
#' @export
#'
#' @examples
transform_entityValue <- function(x,
                                  fun,
                                  entityValue_argName = "x",
                                  entityId_regex = NULL,
                                  newEntityName_prefix = "",
                                  newEntityName_suffix = "_trfmd",
                                  funArgs = NULL,
                                  requiredField_regex = NULL) {
  
  if (!inherits(x, "rxs_parsedExtractionScripts")) {
    stop(wrap_error(
      "As `x`, you have to pass a full Rxs project (i.e. as ",
      "obtained when parsing a set of Rxs files ",
      "with `metabefor::rxs_parseExtractionScripts()`), but instead, ",
      "you passed an object with class(es) ", vecTxtQ(class(x)), "."
    ));
  }
  
  if (!is.function(fun)) {
    stop("As `fun`, you must pass a function.");
  }
  
  for (currentSourceId in names(x$rxsTrees)) {
    
    ### Traverse tree and add standardized means (where applicable)
    x$rxsTrees[[currentSourceId]]$Do(
      
      ### For every node for which the filterFun specified
      ### below returns TRUE, execute this function:
      
      function(entityNode) {

        tmpNode <-
          entityNode$AddSibling(
            name = paste0(newEntityName_prefix,
                          entityNode$name,
                          newEntityName_suffix)
          );
        
        tmpNode$value <-
          do.call(
            fun,
            args=
              c(stats::setNames(list(entityNode$value),
                                nm = entityValue_argName),
                funArgs)
            );
        
      },
      
      ### The filter function that checks whether
      ### a node should be processed
      
      filterFun = function(currentNode) {
        
        if (!is.null(entityId_regex)) {
          return(grepl(entityId_regex, currentNode$name));
        } else {
          return(TRUE);
        }

      }
    );
    
  }
  
  return(invisible(x));
  
}
