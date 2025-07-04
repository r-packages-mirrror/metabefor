#' Check whether a prospective node name is valid, sanitize it, and return it
#'
#' @param x The prospective node name
#' @param entityName The original name of the entity before renaming
#' @param thisEntityText Text to insert with the the current name of this
#' entity, in case it's about to be renamed.
#'
#' @return The (sanitized) node name
#' @export
#'
#' @examples metabefor::nodeName("this_is_valid");
#' metabefor::nodeName("this is sanitized");
#' if (FALSE) {
#'   metabefor::nodeName(
#'     "_thisThrowsAnError:first character can't be _"
#'   );
#'   ### This also throws an error: a reserved name by data.tree
#'   metabefor::nodeName(
#'     "parent"
#'   );
#' }
nodeName <- function(x,
                     entityName = NULL,
                     node = NULL,
                     thisEntityText = "this entity (with temporary name '%s')") {
  
  ### Added on 2025-06-11, to allow completely empty
  ### clustering entities to also omit the entity identifier
  if (!is.null(node)) {
    if (is.list(node$value)) {
      if (all(unlist(lapply(node$value, is.null)))) {
        if (is.null(x)) {
          if (is.null(entityName)) {
            return(paste0("empty_entity_id_", squids::squids(1)));
          } else {
            return(entityName);
          }
        }
      }
    }
  }

  reservedNames <-
    c(metabefor::opts$get("rxsReservedNames"),
      data.tree::NODE_RESERVED_NAMES_CONST);
  
  identifierExplanationText <- metabefor::opts$get("identifierExplanationText");
  
  if (is.null(entityName)) {
    thisEntityText <- "this entity";
  } else {
    thisEntityText <- sprintf(thisEntityText, entityName);
  }
  
  if (is.null(x)) {
    stop("\n---------- metabefor error, please read carefully:\n\n",
         wrapVector(paste0("As an identifier for ", thisEntityText,
         ", you specified `NULL` ",
         "(you probably forgot to specify an identifier). ",
         "Please change it to a valid entity identifier!\n\n",
         identifierExplanationText), 60),
         "\n\n----------\n\n");
  }
  
  if (is.na(x)) {
    stop("\n---------- metabefor error, please read carefully:\n\n",
         wrapVector(paste0("As an identifier for ", thisEntityText,
         ", you specified `NA` ",
         "(you probably forgot to specify an identifier). ",
         "Please change it to a valid entity identifier!\n\n",
         identifierExplanationText), 60),
         "\n\n----------\n\n");
  }

  res <- sanitize_identifiers(x);

  if (!grepl("^[a-zA-Z][a-zA-Z0-9_]*$", res)) {
    stop("\n---------- metabefor error, please read carefully:\n\n",
         wrapVector(paste0("As an identifier for ", thisEntityText,
         ", you specified '", x, "', which, after I sanitized illegal ",
         "characters, becomes '", res, "', which is not a valid identifier. ",
         "Please change it to a valid entity identifier!\n\n",
         identifierExplanationText), 60),
         "\n\n----------\n\n");
  }
  
  if (res %in% reservedNames) {
    stop("\n---------- metabefor error, please read carefully:\n\n",
         wrapVector(paste0("As an identifier for ", thisEntityText,
         ", you specified a reserved ",
         "name (originally '", x, "', and after I sanitized illegal ",
         "characters, '", res, "'). ",
         "Please change it to something else! The reserved names ",
         "that you cannot use as entity identifiers are: ",
         vecTxtQ(reservedNames), ".\n\n",
         identifierExplanationText), 60),
         "\n\n----------\n\n");
  }
  
  return(res);
  
}