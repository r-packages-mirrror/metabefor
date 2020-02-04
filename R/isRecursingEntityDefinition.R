isRecursingEntityDefinition <- function(node,
                                        recursingColName=metabefor::opts$get(entityColNames)$recursingCol) {
  return(((!is.null(node[[recursingColName]])) &&
          (!is.na(node[[recursingColName]])) &&
            (isTRUE(node[[recursingColName]]) ||
            (toupper(node[[recursingColName]])=="TRUE"))));
}
