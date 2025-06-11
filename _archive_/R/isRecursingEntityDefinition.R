isRecursingEntityDefinition <- function(node,
                                        recursingColName=metabefor::opts$get("entityColNames")$recursingCol) {
  return(is_TRUE(node[[recursingColName]]));
}
