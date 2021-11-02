isRecurringEntityDefinition <- function(node,
                                        recurringColName=metabefor::opts$get("valueTemplateColNames")$recurringCol) {
  return(((!is.null(node[[recurringColName]])) &&
          (!is.na(node[[recurringColName]])) &&
            (isTRUE(node[[recurringColName]]) ||
            (toupper(node[[recurringColName]])=="TRUE"))));
}
