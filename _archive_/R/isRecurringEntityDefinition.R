isRecurringEntityDefinition <- function(node,
                                        recurringColName=metabefor::opts$get("valueTemplateColNames")$recurringCol) {
  return(is_TRUE(node[[recurringColName]]));
}
