#' Explode a vector to values
#' 
#' This function takes a `studies` object (as produced
#' by [metabefor::rxs_parseExtractionScripts()]) and processes all study
#' trees, looking for the values stored in the entity with the specified
#' identifier (this cannot be a clustering entity or a clustered entity;
#' see the definitions at
#' <https://r-packages.gitlab.io/metabefor/articles/definitions.html>),
#' compiling a list of all occurring values, and then adds new entities
#' for each occurring value, with new values (by default, `0` and `1`)
#' indicating whether that value occurred in the original vector for a
#' given entity.
#' 
#' For example, imagine the following situation. We want to process an entity
#' called `exampleVector` in three study trees. In the first study tree,
#' that entity contains the value `c("a", "b")`; in the second study tree, it
#' contains `c("a", "c", "d")`; and in the third study tree, it doesn't exist.
#' If we would then run if this function is run with its default arguments,
#' in all three study trees, four new entities will be added, `exampleVector_a`,
#' `exampleVector_b`, `exampleVector_c`, and `exampleVector_d`. For the first
#' study, the values would be, respectively, `1`, `1`, `0`, and `0`; for the
#' second study, they would be `1`, `0`, `1`, and `1`; and for the third
#' study, they would all be `0` (see the example).
#'
#' @param studies The `studies` object (as produced
#' by [metabefor::rxs_parseExtractionScripts()]).
#' @param entityId The entity identifier.
#' @param prefix The prefix to use to create the new entity names.
#' @param values The values to store to represent, respectively, that a value
#' did not or did occur in the original vector.
#' @param valueDict Optionally, a dictionary that will be used to convert
#' values to entity identifiers (i.e. the bit appended to the `prefix`). If
#' nothing is specified, the original values will be sanitized and used.
#'
#' @return Invisibly (and irrelevantly, given R6's pass by reference behavior),
#' the `studies` object.
#' @export
#'
#' @examples ### Create fake 'studies' object
#' studies <-
#'   list(rxsTrees = list());
#' class(studies) <-
#'   "rxs_parsedExtractionScripts";
#'   
#' ### Add three fake studies
#' studies$rxsTrees <-
#'   list(study1 = data.tree::Node$new("study"),
#'        study2 = data.tree::Node$new("study"),
#'        study3 = data.tree::Node$new("study"));
#' 
#' ### Add values for an entity with id 'exampleVector'
#' studies$rxsTrees$study1$AddChild(
#'   "exampleVector", value=c("a", "b")
#' );
#' studies$rxsTrees$study2$AddChild(
#'   "exampleVector", value=c("a", "c", "d")
#' );
#' 
#' ### Explore this vector
#' explode_vector_to_values(studies, "exampleVector");
#' 
#' ### View the results for the first study
#' studies$rxsTrees$study1$Get(
#'   "value",
#'   filterFun = function(node) {
#'     return(grepl("exampleVector_", node$name));
#'   },
#'   simplify = FALSE
#' );
explode_vector_to_values <- function(studies,
                                     entityId,
                                     prefix = NULL,
                                     values = 0:1,
                                     valueDict = NULL) {
  
  if (!inherits(studies, "rxs_parsedExtractionScripts")) {
    stop("The object you pass as 'studies' must be an object ",
         "with parsed Rxs files, as produced by a call to ",
         "metabefor::rxs_parseExtractionScripts().");
  }
  
  if (is.null(prefix)) {
    prefix <- paste0(entityId, "_");
  }
  
  allPossibleValues_perStudy <-
    lapply(
      studies$rxsTrees,
      function(tree) {
        return(
          as.vector(
            tree$Get(
              "value",
              filterFun = function(node) {
                return(
                  node$name == entityId
                );
              }
            )
          )
        );
      }
    );
  
  allPossibleValues_freq <-
    table(unlist(allPossibleValues_perStudy));
  
  allPossibleValues <- names(allPossibleValues_freq);
  
  if (is.null(valueDict)) {
    valueDict <-
      stats::setNames(
        gsub("[^a-zA-Z0-9_.]", "_", allPossibleValues),
        nm = allPossibleValues
      );
  }
  
  for (currentStudyTree in studies$rxsTrees) {
    
    if (is.null(currentStudyTree[[entityId]])) {
      
      valueOccurrences <-
        rep(values[1], length(valueDict));
      
    } else {
      
      valueOccurrences <-
        ifelse(
          names(valueDict) %in% currentStudyTree[[entityId]]$value,
          values[2],
          values[1]
        );
    }
    
    for (i in seq_along(valueOccurrences)) {
      currentStudyTree$AddChild(
        name = paste0(prefix, valueDict[i]),
        value = valueOccurrences[i]
      );
    }
    
  }
  
  return(invisible(NULL));
  
}
