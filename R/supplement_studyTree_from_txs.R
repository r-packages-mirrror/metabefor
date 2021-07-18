#' Supplement a parsed studies object with entities from a tabulated extraction spreadsheet (txs) specification
#' 
#' Sometimes, you forget to extract one or more entities. In such cases, it can
#' be more efficient to create a spreadsheet with the omitted entities and then
#' add those into the existing studies object. For that purpose, the tabulated
#' extraction spreadsheet (txs) format exists. This is a rectangular table
#' with the following columns: `study_identification_entity_id`,
#' `study_identification_value`, `parent_entity_id`, `entity_id`, and `value`.
#' 
#' If `study_identification_entity_id` is empty (i.e. does not contain any
#' non-whitespace characters), `study_identification_value` will be assumed to
#' be the name of the relevant study tree in the studies object.
#'
#' @param studies The parsed studies object, containing a list of study trees. 
#' @param txs_specs The txs specifications, as a link to a publicly readable
#' Google spreadsheet, an Excel file (if you have {`openxlsx`} installed),
#' an SPSS dataset (if you have {`haven`} installed), or a comma separated
#' values file (which will be read with [read.csv()]).
#'
#' @return Invisibly, the studies object. Note that the study trees will be
#' changed in place given `data.tree`'s pass-by-reference logic; so you can
#' discard the result.
#' 
#' @export
supplement_studyTrees_from_txs <- function(studies,
                                           txs_specs) {
  
  if (!inherits(studies, ""))
  
  dat <- read_sheet(txs_specs);

  for (i in 1:nrow(dat)) {
    
    study_identification_entity_id <-
      trimws(dat[i, "study_identification_entity_id"]);
    study_identification_value <-
      trimws(dat[i, "study_identification_value"]);
    parent_entity_id <-
      trimws(dat[i, "parent_entity_id"]);
    entity_id <-
      trimws(dat[i, "entity_id"]);
    value <-
      trimws(dat[i, "value"]);

    if (is.na(study_identification_entity_id) ||
        nchar(study_identification_entity_id) == 0) {
      currentTree <- studies$rxsTrees[[study_identification_value]];
    } else {
      stop("This functionality has not been implemented yet!");
    }
    
    if (!inherits(currentTree, "Node")) {
      warning("I could not find a valid study tree with identifier `",
              study_identification_value, "`, so I'm skipping this row.")
    } else {

      currentParentNode <- data.tree::FindNode(currentTree, parent_entity_id);
      
      if (is.null(currentParentNode)) {
        stop("In the study tree identified by `",
             study_identification_entity_id,
             "`, I could not file the designated parent entity node (`",
             parent_entity_id, "`).");
      }
      
      tryCatch(
        currentParentNode$AddChild(
          name = entity_id,
          value = eval(parse(text = value))
        ),
        error = function(e) {
          stop("In the study tree identified by `",
               study_identification_entity_id,
               "`, I could not add an entity node to the designated parent ",
               "entity node (`", parent_entity_id, "`). This is either because ",
               "the entity id you passed (`", entity_id,
               "`) is not a valid name for a node, or because the value you ",
               "passed to store (`", value, "`) is not a valid R expression. ",
               "Note that if you want to store a text string, you must quote it ",
               "(using either the double quote, \", or the single quote, ').");
        }
      );
    }

  }
  
  return(invisible(studies));
  
}

