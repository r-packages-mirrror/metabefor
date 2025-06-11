isRecurringEntityInclusion <- function(node,
                                       recurringColName=metabefor::opts$get("entityColNames")$recurringCol) {
  return((!is.null(node[[recurringColName]])) &&
         (!is.na(node[[recurringColName]])) &&
         (is.character(node[[recurringColName]])) &&
         (!(toupper(node[[recurringColName]])=="TRUE")));
}
