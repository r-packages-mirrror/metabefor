#' Supplement a full Rxs project object with entities from a tabulated extraction spreadsheet (txs) specification
#' 
#' Sometimes, you forget to extract one or more entities. In such cases, it can
#' be more efficient to create a spreadsheet with the omitted entities and then
#' add those into the existing full Rxs project object. For that purpose, the
#' tabulated extraction spreadsheet (txs) format exists. This is a rectangular
#' table with the following columns: `source_identification_entity_id`,
#' `source_identification_value`, `parent_entity_id`, `entity_id`, and `value`.
#' 
#' If `source_identification_entity_id` is empty (i.e. does not contain any
#' non-whitespace characters), `source_identification_value` will be assumed to
#' be the name of the relevant Rxs tree in the full Rxs project object.
#'
#' @param x The parsed full Rxs project object, containing a list of Rxs trees. 
#' @param txs_specs The txs specifications, as a link to a publicly readable
#' Google spreadsheet, an Excel file (if you have {`openxlsx`} installed),
#' an SPSS dataset (if you have {`haven`} installed), or a comma separated
#' values file (which will be read with [read.csv()]).
#' @param stopOnErrors Whether to throw an error or show a warning (or just
#' use `cat` to show a message is `silent = metabefor::opts$get("silent")`) when encountering errors.
#' @param explode_vector_to_values Whether to call `explode_vector_to_values`
#' if there are vectors among the added values.
#' @param silent Whether to the chatty or silent.
#'
#' @return Invisibly, the full Rxs project object. Note that the Rxs trees
#' will be changed in place given `data.tree`'s pass-by-reference logic; so
#' you can discard the result.
#' 
#' @export
supplement_rxsTrees_from_txs <- function(x,
                                         txs_specs,
                                         stopOnErrors = FALSE,
                                         explode_vector_to_values = FALSE,
                                         silent = metabefor::opts$get("silent")) {
  
  txsColNames <- metabefor::opts$get("txsColNames");

  if (!inherits(x, "rxs_parsedExtractionScripts")) {
    stop("The object you pass as 'x' must be an object ",
         "with parsed Rxs files, as produced by a call to ",
         "metabefor::rxs_parseExtractionScripts().");
  }
  
  dat <- read_spreadsheet(txs_specs,
                          sheet = 1,
                          flattenSingleDf = TRUE,
                          silent=silent);
  
  checkForColName(dat, txsColNames$source_identification_entity_id);
  checkForColName(dat, txsColNames$source_identification_value);
  checkForColName(dat, txsColNames$parent_entity_id);
  checkForColName(dat, txsColNames$entity_id);
  checkForColName(dat, txsColNames$value);
  
  entityIds <- unique(trimws(dat[, txsColNames$entity_id]));
  
  msg("Read ", nrow(dat), " txs specifications for entity identifiers ",
      vecTxtQ(entityIds), "\n",
      silent = silent);

  for (i in 1:nrow(dat)) {
    
    source_identification_entity_id <-
      trimws(dat[i, txsColNames$source_identification_entity_id]);
    source_identification_value <-
      trimws(dat[i, txsColNames$source_identification_value]);
    parent_entity_id <-
      trimws(dat[i, txsColNames$parent_entity_id]);
    entity_id <-
      trimws(dat[i, txsColNames$entity_id]);
    value <-
      trimws(dat[i, txsColNames$value]);

    if (is.na(source_identification_entity_id) ||
        nchar(source_identification_entity_id) == 0) {
      currentTree <- x$rxsTrees[[source_identification_value]];
    } else {
      stop("This functionality has not been implemented yet!");
    }
    
    if (!inherits(currentTree, "Node")) {
      errMsg <- paste0(
        "I could not find a valid Rxs tree with identifier `",
        source_identification_value, "`.\n"
      );
      if (stopOnErrors) {
        stop(errMsg);
      } else {
        if (silent) {
          warning(errMsg)
        } else {
          msg(errMsg, silent=silent);
        }
      }
    } else {

      currentParentNode <- data.tree::FindNode(currentTree, parent_entity_id);
      
      if (is.null(currentParentNode)) {
        errMsg <- paste0(
          "In the Rxs tree identified by `",
          source_identification_value,
          "`, I could not find the designated parent entity node (`",
          parent_entity_id, "`).\n");
        if (stopOnErrors) {
          stop(errMsg);
        } else {
          if (silent) {
            warning(errMsg)
          } else {
            msg(errMsg, silent=silent);
          }
        }
      }
      
      tryCatch({
          if (is.list(currentParentNode$value)) {
            currentParentNode$value <-
              c(
                currentParentNode$value,
                stats::setNames(
                  list(eval(parse(text = value))),
                  nm = entity_id
                )
              );
          } else {
            currentParentNode$AddChild(
              name = entity_id,
              value = eval(parse(text = value))
            )
          }
        },
        error = function(e) {
          errMsg <-
            paste0(
              "In the Rxs tree identified by `",
              source_identification_value,
              "`, I could not add an entity node to the designated parent ",
              "entity node (`", parent_entity_id, "`). This is either because ",
              "the entity id you passed (`", entity_id,
              "`) is not a valid name for a node, or because the value you ",
              "passed to store (`", value, "`) is not a valid R expression. ",
              "Note that if you want to store a text string, you must quote it ",
              "(using either the double quote, \", or the single quote, ').\n"
            );
          if (stopOnErrors) {
            stop(errMsg);
          } else {
            if (silent) {
              warning(errMsg)
            } else {
              msg(errMsg, silent=silent);
            }
          }
        }
      );
    }

  }
  
  msg("Finished processing ", nrow(dat), " txs specifications.\n",
      silent = silent);
  
  if (explode_vector_to_values) {
    
    msg("Exploding vectors to values.\n",
        silent = silent);
    
    for (currentEntityId in entityIds) {
      
      msg("Exploding entity identifier ",
          currentEntityId, ".\n",
          silent = silent);
      
      explode_vector_to_values(
        x,
        currentEntityId
      );
      
    }
    
    msg("Finished exploding vectors to values.\n",
        silent = silent);
  }
  
  return(invisible(x));
  
}

