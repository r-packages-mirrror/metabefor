#' Explode a vector to values
#' 
#' This function takes a full Rxs project (as produced
#' by [metabefor::rxs_parseExtractionScripts()]) and processes all Rxs
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
#' called `exampleVector` in three Rxs trees. In the first Rxs tree,
#' that entity contains the value `c("a", "b")`; in the second Rxs tree, it
#' contains `c("a", "c", "d")`; and in the third Rxs tree, it doesn't exist.
#' If we would then run this function with its default arguments,
#' in all three Rxs trees, four new entities will be added, `exampleVector_a`,
#' `exampleVector_b`, `exampleVector_c`, and `exampleVector_d`. In the first
#' Rxs tree, the values would be, respectively, `1`, `1`, `0`, and `0`; in the
#' second, they would be `1`, `0`, `1`, and `1`; and in the third Rxs tree,
#' they would all be `0` (see the example).
#'
#' @param x The full Rxs project object (as produced
#' by [metabefor::rxs_parseExtractionScripts()]).
#' @param entityId The entity identifier.
#' @param prefix The prefix to use to create the new entity names.
#' @param values The values to store to represent, respectively, that a value
#' did not or did occur in the original vector.
#' @param valueDict Optionally, a dictionary that will be used to convert
#' values to entity identifiers (i.e. the bit appended to the `prefix`). If
#' nothing is specified, the original values will be sanitized and used.
#'
#' @return Invisibly, the full Rxs project object. Note that the Rxs trees
#' will be changed in place given `data.tree`'s pass-by-reference logic; so
#' you can discard the result if you want.
#' @export
#'
#' @examples ### Create fake Rxs project
#' rxs <-
#'   list(rxsTrees = list());
#' class(rxs) <-
#'   "rxs_parsedExtractionScripts";
#'   
#' ### Add three fake sources
#' rxs$rxsTrees <-
#'   list(source1 = data.tree::Node$new("source"),
#'        source2 = data.tree::Node$new("source"),
#'        source3 = data.tree::Node$new("source"));
#' 
#' ### Add values for an entity with id 'exampleVector'
#' rxs$rxsTrees$source1$AddChild(
#'   "exampleVector", value=c("a", "b")
#' );
#' rxs$rxsTrees$source2$AddChild(
#'   "exampleVector", value=c("a", "c", "d")
#' );
#' 
#' ### Explore this vector
#' explode_vector_to_values(rxs, "exampleVector");
#' 
#' ### View the results for the first source
#' rxs$rxsTrees$source1$Get(
#'   "value",
#'   filterFun = function(node) {
#'     return(grepl("exampleVector_", node$name));
#'   },
#'   simplify = FALSE
#' );
explode_vector_to_values <- function(x,
                                     entityId,
                                     prefix = NULL,
                                     values = 0:1,
                                     valueDict = NULL) {
  
  if (!inherits(x, "rxs_parsedExtractionScripts")) {
    stop("The object you pass as 'x' must be an object ",
         "with parsed Rxs files, as produced by a call to ",
         "metabefor::rxs_parseExtractionScripts().");
  }
  
  if (is.null(prefix)) {
    prefix <- paste0(entityId, "_");
  }
  
  allPossibleValues_perSource <-
    lapply(
      x$rxsTrees,
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
    table(unlist(allPossibleValues_perSource));
  
  allPossibleValues <- names(allPossibleValues_freq);
  
  if (is.null(valueDict)) {
    valueDict <-
      stats::setNames(
        gsub("[^a-zA-Z0-9_.]", "_", allPossibleValues),
        nm = allPossibleValues
      );
  }
  
  for (currentRxsTree in x$rxsTrees) {
    
    if (is.null(currentRxsTree[[entityId]])) {
      
      valueOccurrences <-
        rep(values[1], length(valueDict));
      
    } else {
      
      valueOccurrences <-
        ifelse(
          names(valueDict) %in% currentRxsTree[[entityId]]$value,
          values[2],
          values[1]
        );
    }
    
    for (i in seq_along(valueOccurrences)) {
      currentRxsTree$AddChild(
        name = paste0(prefix, valueDict[i]),
        value = valueOccurrences[i]
      );
    }
    
  }
  
  return(invisible(x));
  
}
